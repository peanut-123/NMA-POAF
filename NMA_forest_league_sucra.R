library(readxl)
library(tidyverse)
library(dplyr)
library(gemtc)
library(patchwork)
library(stringr)
library(extrafont) 
arm <- read_excel("article_arm.xlsx", sheet = 1)
colnames(arm)
arm=rename(arm,
           study  =  No ,      
           treatment  = ARM,      
           sampleSize = ARM_n_size,          
           responders = ARM_POAF_events      
)%>% 
  mutate(
    treatment = str_trim(treatment) %>% 
      str_replace_all("[^A-Za-z0-9]", "_") %>% 
      str_replace_all("_+", "_")          
  )

parm <- arm %>% filter(`Study type` == "P")
nparm <- arm %>% filter(`Study type` == "NP")
####p-nma####

set.seed(2025)
pnma <- mtc.network(parm)
summary(pnma)
pmodel <- mtc.model(pnma, 
                    n.chain = 4,
                    type = "consistency", 
                    likelihood = "binom", 
                    link = "logit",         ##logit for OR  log for RR
                    linearModel = "random",
                    dic =  T)

pmcmc <- mtc.run(pmodel, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(pmcmc)

saveRDS(pmcmc, "pmcmc.RDS")

pdf("p.gelman.pdf", width = 8, height = 10)
gelman.plot(pmcmc$samples)
dev.off()

pdf("p.Trace and Density Plots.pdf")
plot(pmcmc)
dev.off()

## SUCRA 
ranks <- rank.probability(pmcmc, preferredDirection = -1)   
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "pSUCRA_sorted.csv")

pdf("pSUCRA_plot.pdf", width = 10, height = 7) 
par(mar = c(10, 5, 4, 2))
barplot(sucra_sorted, 
        col = "#2D6A4F",             
        border = "white",            
        ylab = "SUCRA Value",
        main = "SUCRA Ranking",
        las = 2,                     
        cex.names = 0.8,             
        ylim = c(0, 1))
abline(h = 0, col = "black")
dev.off()
#league
sorted_names <- names(sort(sucra_raw, decreasing = TRUE))
league_res <- relative.effect.table(pmcmc)
or_val  <- exp(league_res[,,2])
low_val <- exp(league_res[,,1])
upp_val <- exp(league_res[,,3])
n <- length(sorted_names)
formatted_matrix <- matrix("", nrow = n, ncol = n, dimnames = list(sorted_names, sorted_names))

for (i in sorted_names) {
  for (j in sorted_names) {
    if (i == j) {
      formatted_matrix[i, j] <- i  
    } else {
      formatted_matrix[i, j] <- sprintf("%.2f (%.2f, %.2f)", 
                                        or_val[i, j], low_val[i, j], upp_val[i, j])
    }
  }
}

write.csv(formatted_matrix, "pleaguetable.csv")

####node-splitting

p.nodesplit_results <- mtc.nodesplit(pnma, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(p.nodesplit_results)
saveRDS(p.nodesplit_results, "p.nodesplit_final_data.rds")

####p.forset plot####

rel_eff <- relative.effect(pmcmc, t1 = "Control")

rel_sum <- summary(rel_eff)
res_quant <- as.data.frame(rel_sum$summaries$quantiles)

final_data <- res_quant %>%
  mutate(Parameter = rownames(res_quant)) %>%
  mutate(
    Treatment = gsub("d\\(Control, (.*)\\)", "\\1", Parameter),
    OR = exp(`50%`),
    Lower = exp(`2.5%`),
    Upper = exp(`97.5%`),
    OR_label = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper)
  )


cinema <- read.csv("cinema_random_OR_Report.csv",header = T,sep = ",")
clean_text <- function(x) {
  x %>% 
    str_replace_all("[^[:alnum:]]", "") %>% 
    tolower() %>% 
    str_trim()
}

final_data_clean <- final_data %>%
  filter(Treatment != "sd.d") %>%
  mutate(
    Display_Name = gsub("d\\.Control\\.", "", Treatment) %>% 
      str_replace_all("_", " "),
    Match_Key = clean_text(Display_Name)
  )

cinema_clean <- cinema %>%
  mutate(Match_Key = clean_text(Comparison))
final_plot_data <- final_data_clean %>%
  left_join(cinema_clean, by = "Match_Key")

extraction_data <- pnma$data.ab %>%
  group_by(treatment) %>%
  summarise(
    Study_Count = n_distinct(study),   
    Total_N = sum(sampleSize, na.rm = TRUE)  
  ) %>%
  left_join(data.frame(treatment = pnma$treatments$id, 
                       Comparison = pnma$treatments$description), 
            by = "treatment")
indirect_treatments <- c("NAC_BB", "Propofol", "Carvedilol")
final_plot_data <- final_plot_data %>%
  mutate(Comparison_Match = case_when(
    Comparison == "ACEI/ARB" ~ "ACEI_ARB",
    Comparison == "L-carnitine" ~ "L_carnitine",
    Comparison == "N-Acetylcysteine" ~ "N_Acetylcysteine",
    Comparison == "Other antiarrhythmic drugs" ~ "Other_antiarrhythmic_drugs",
    Comparison == "Vitamin C" ~ "Vitamin_C",
    Comparison == "Vitamin D" ~ "Vitamin_D",
    TRUE ~ Comparison
  )) %>%
  select(-any_of(c("Study_Count", "Total_N", "Clinical_Strength", "Final_Sort_Value"))) %>% 
  left_join(extraction_data %>% select(Comparison, Study_Count, Total_N), 
            by = c("Comparison_Match" = "Comparison"))

final_plot_data <- final_plot_data %>%
  mutate(
    Clinical_Strength = case_when(
      Upper < 1.0 & OR < 0.5 ~ "Strong",
      Upper < 1.0 & OR >= 0.5 & OR <= 0.8 ~ "Moderate",
      TRUE ~ "Weak/Non-significant"
    ),
    Clinical_Strength = factor(Clinical_Strength, levels = c("Strong", "Moderate", "Weak/Non-significant")),
    Confidence.rating = factor(Confidence.rating, levels = c("High", "Moderate", "Low", "Very low")),

    Sort_Group = ifelse(Comparison %in% c("NAC_BB", "Propofol", "Carvedilol"), 1, 0),
    Final_Sort_Value = Sort_Group * 100 + OR
  )

hline_pos <- sum(final_plot_data$Sort_Group == 1) + 0.5

clinical_colors <- c("Strong" = "#2D6A4F", "Moderate" = "#92C5AD", "Weak/Non-significant" = "#ADB5BD")
conf_shapes <- c("High" = 24, "Moderate" = 22, "Low" = 23, "Very low" = 25)

p_left <- ggplot(final_plot_data, aes(y = reorder(Comparison, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = Comparison), hjust = 0, fontface = "bold", size = 5.2, family = "Arial") +
  xlim(0, 1.2) + theme_void() + labs(title = "Intervention") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0),
        plot.margin = margin(l = 10, r = -30, t = 0, b = 0))

p_mid <- ggplot(final_plot_data, aes(y = reorder(Comparison, -Final_Sort_Value), x = OR)) +
  geom_hline(yintercept = hline_pos, linetype = "dotdash", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Clinical_Strength), height = 0.2, linewidth = 1) +
  geom_point(aes(fill = Clinical_Strength, shape = Confidence.rating), size = 5.5, color = "white", stroke = 0.6) +
  scale_fill_manual(values = clinical_colors) +
  scale_color_manual(values = clinical_colors) +
  scale_shape_manual(values = conf_shapes) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5)) +
  coord_cartesian(xlim = c(0.03, 10)) +
  theme_minimal(base_family = "Arial") +
  theme(panel.grid = element_blank(), axis.line.x = element_line(color = "black"),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20), hjust = 0.68),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.85, 0.80), legend.background = element_rect(fill = "white", color = "grey80")) +
  labs(x = "Favours Treatment <---  ---> Favours Control") +
  guides(color = "none", 
         fill = guide_legend(title = "Clinical Strength", order = 1, override.aes = list(shape = 21, size = 7)),
         shape = guide_legend(title = "Confidence (CINeMA)", order = 2, override.aes = list(size = 7, fill = "black")))

p_right_or <- ggplot(final_plot_data, aes(y = reorder(Comparison, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper)), hjust = 0, size = 4.5, family = "Arial") +
  xlim(0, 1.2) + theme_void() + labs(title = "OR (95% CrI)") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0))

p_right_n <- ggplot(final_plot_data, aes(y = reorder(Comparison, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = format(Total_N, big.mark = ",")), hjust = 0.5, size = 4.5, family = "Arial") +
  xlim(-0.5, 0.5) + theme_void() + labs(title = "Total N") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0.5))

p_right_studies <- ggplot(final_plot_data, aes(y = reorder(Comparison, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = Study_Count), hjust = 0.5, size = 4.5, family = "Arial") +
  xlim(-0.5, 0.5) + theme_void() + labs(title = "Studies") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0.5))

final_output <- p_left + p_mid + p_right_or + p_right_n + p_right_studies + 
  plot_layout(widths = c(1.5, 3.5, 1.2, 0.9, 0.7)) + 
  plot_annotation(
    title = "Network Meta-Analysis: Prevention of Postoperative Atrial Fibrillation",
    theme = theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, family = "Arial"))
  )

ggsave("BMJ_Forest_Final_Perfect.pdf", final_output, width = 28, height = 16, device = cairo_pdf)


#####np-nma####
set.seed(2025)
npnma <- mtc.network(nparm)
summary(npnma)
npmodel <- mtc.model(npnma, 
                    n.chain = 4,
                    type = "consistency", 
                    likelihood = "binom", 
                    link = "logit",         ##logit for OR  log for RR
                    linearModel = "random",
                    dic =  T)

npmcmc <- mtc.run(npmodel, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(npmcmc)

saveRDS(npmcmc, "npmcmc.RDS")

pdf("np.gelman.pdf", width = 8, height = 10)
gelman.plot(npmcmc$samples)
dev.off()

pdf("np.Trace and Density Plots.pdf")
plot(npmcmc)
dev.off()

## SUCRA 
ranks <- rank.probability(npmcmc, preferredDirection = -1)   
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "npSUCRA_sorted.csv")

pdf("npSUCRA_plot.pdf", width = 10, height = 7) 
par(mar = c(10, 5, 4, 2))
barplot(sucra_sorted, 
        col = "#003366",             
        border = "white",            
        ylab = "SUCRA Value",
        main = "SUCRA Ranking",
        las = 2,                     
        cex.names = 0.8,             
        ylim = c(0, 1))
abline(h = 0, col = "black")
dev.off()
#league
sorted_names <- names(sort(sucra_raw, decreasing = TRUE))
league_res <- relative.effect.table(npmcmc)
or_val  <- exp(league_res[,,2])
low_val <- exp(league_res[,,1])
upp_val <- exp(league_res[,,3])
n <- length(sorted_names)
formatted_matrix <- matrix("", nrow = n, ncol = n, dimnames = list(sorted_names, sorted_names))

for (i in sorted_names) {
  for (j in sorted_names) {
    if (i == j) {
      formatted_matrix[i, j] <- i  
    } else {
      formatted_matrix[i, j] <- sprintf("%.2f (%.2f, %.2f)", 
                                        or_val[i, j], low_val[i, j], upp_val[i, j])
    }
  }
}

write.csv(formatted_matrix, "npleaguetable.csv")

####np.forest plot####

rel_eff <- relative.effect(npmcmc, t1 = "Control")

rel_sum <- summary(rel_eff)
res_quant <- as.data.frame(rel_sum$summaries$quantiles)

final_data <- res_quant %>%
  mutate(Parameter = rownames(res_quant)) %>%
  mutate(
    Treatment = gsub("d\\(Control, (.*)\\)", "\\1", Parameter),
    OR = exp(`50%`),
    Lower = exp(`2.5%`),
    Upper = exp(`97.5%`),
    OR_label = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper)
  )


cinema <- read.csv("cinema_random_OR_Report.csv",header = T,sep = ",")
clean_text <- function(x) {
  x %>% 
    str_replace_all("[^[:alnum:]]", "") %>% 
    tolower() %>% 
    str_trim()
}

final_data_clean <- final_data %>%
  filter(Treatment != "sd.d") %>%
  mutate(
    Display_Name = gsub("d\\.Control\\.", "", Treatment) %>% 
      str_replace_all("_", " "),
    Match_Key = clean_text(Display_Name)
  )

cinema_clean <- cinema %>%
  mutate(Match_Key = clean_text(Comparison))
final_plot_data <- final_data_clean %>%
  left_join(cinema_clean, by = "Match_Key")

extraction_data <- npnma$data.ab %>%
  group_by(treatment) %>%
  summarise(
    Study_Count = n_distinct(study),   
    Total_N = sum(sampleSize, na.rm = TRUE)  
  ) %>%
  
  left_join(data.frame(treatment = npnma$treatments$id, 
                       Comparison = npnma$treatments$description), 
            by = "treatment")
extraction_data <- extraction_data %>%
  mutate(match_key = str_replace_all(treatment, "_", " "))
final_plot_data <- final_plot_data %>%
  left_join(extraction_data, by = c("Display_Name" = "match_key"))

final_plot_data <- final_plot_data %>%
  mutate(
    Clinical_Strength = case_when(
      Upper < 1.0 & OR < 0.5 ~ "Strong",
      Upper < 1.0 & OR >= 0.5 & OR <= 0.8 ~ "Moderate",
      TRUE ~ "Weak/Non-significant"
    ),
    Clinical_Strength = factor(Clinical_Strength, levels = c("Strong", "Moderate", "Weak/Non-significant")),
    Confidence.rating = factor(Confidence.rating, levels = c("High", "Moderate", "Low", "Very low")),
    
    Sort_Group = ifelse(Comparison.x %in% c("NAC_BB", "Propofol", "Carvedilol"), 1, 0),
    Final_Sort_Value = Sort_Group * 100 + OR
  )

clinical_colors <- c("Strong" = "#003366", "Moderate" = "#8ECAE6", "Weak/Non-significant" = "#ADB5BD")
conf_shapes <- c("High" = 24, "Moderate" = 22, "Low" = 23, "Very low" = 25)

p_left <- ggplot(final_plot_data, aes(y = reorder(Comparison.x, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = Comparison.x), hjust = 0, fontface = "bold", size = 5.2, family = "Arial") +
  xlim(0, 1.2) + theme_void() + labs(title = "Intervention") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0),
        plot.margin = margin(l = 10, r = -30, t = 0, b = 0))

p_mid <- ggplot(final_plot_data, aes(y = reorder(Comparison.x, -Final_Sort_Value), x = OR)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Clinical_Strength), height = 0.2, linewidth = 1) +
  geom_point(aes(fill = Clinical_Strength, shape = Confidence.rating), size = 5.5, color = "white", stroke = 0.6) +
  scale_fill_manual(values = clinical_colors) +
  scale_color_manual(values = clinical_colors) +
  scale_shape_manual(values = conf_shapes) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5)) +
  coord_cartesian(xlim = c(0.03, 10)) +
  theme_minimal(base_family = "Arial") +
  theme(panel.grid = element_blank(), axis.line.x = element_line(color = "black"),
        axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20), hjust = 0.68),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.85, 0.80), legend.background = element_rect(fill = "white", color = "grey80")) +
  labs(x = "Favours Treatment <---  ---> Favours Control") +
  guides(color = "none", 
         fill = guide_legend(title = "Clinical Strength", order = 1, override.aes = list(shape = 21, size = 7)),
         shape = guide_legend(title = "Confidence (CINeMA)", order = 2, override.aes = list(size = 7, fill = "black")))

p_right_or <- ggplot(final_plot_data, aes(y = reorder(Comparison.x, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = sprintf("%.2f (%.2f, %.2f)", OR, Lower, Upper)), hjust = 0, size = 4.5, family = "Arial") +
  xlim(0, 1.2) + theme_void() + labs(title = "OR (95% CrI)") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0))

p_right_n <- ggplot(final_plot_data, aes(y = reorder(Comparison.x, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = format(Total_N, big.mark = ",")), hjust = 0.5, size = 4.5, family = "Arial") +
  xlim(-0.5, 0.5) + theme_void() + labs(title = "Total N") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0.5))

p_right_studies <- ggplot(final_plot_data, aes(y = reorder(Comparison.x, -Final_Sort_Value))) +
  geom_text(aes(x = 0, label = Study_Count), hjust = 0.5, size = 4.5, family = "Arial") +
  xlim(-0.5, 0.5) + theme_void() + labs(title = "Studies") +
  theme(plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0.5))

final_output <- p_left + p_mid + p_right_or + p_right_n + p_right_studies + 
  plot_layout(widths = c(1.5, 3.5, 1.2, 0.9, 0.7)) + 
  plot_annotation(
    title = "Network Meta-Analysis: Prevention of Postoperative Atrial Fibrillation",
    theme = theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, family = "Arial"))
  )

ggsave("npforestplot.pdf", final_output, width = 28, height = 16, device = cairo_pdf)

