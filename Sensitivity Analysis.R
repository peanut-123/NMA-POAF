library(openxlsx)

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
#####remove high ROB  & samll N study(n<100)####
arm_filter <- arm %>%
  filter(N >= 100) %>%             
  filter(ROB2 != 3)

parm_filter <- arm_filter %>% filter(`Study type` == "P")
nparm_filter <- arm_filter %>% filter(`Study type` == "NP")
set.seed(2025)

##
pnma_f <- mtc.network(parm_filter)
summary(pnma_f)
pmodel_f <- mtc.model(pnma_f, 
                    n.chain = 4,
                    type = "consistency", 
                    likelihood = "binom", 
                    link = "logit",         ##logit for OR  log for RR
                    linearModel = "random",
                    dic =  T)

pmcmc_f <- mtc.run(pmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(pmcmc_f)

ranks <- rank.probability(pmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "pSUCRA_small_sucra.csv")

pdf("pSUCRA_small_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(pmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"parm_small_or.xlsx")
#
npnma_f <- mtc.network(nparm_filter)
summary(npnma_f)
npmodel_f <- mtc.model(npnma_f, 
                      n.chain = 4,
                      type = "consistency", 
                      likelihood = "binom", 
                      link = "logit",         ##logit for OR  log for RR
                      linearModel = "random",
                      dic =  T)

npmcmc_f <- mtc.run(npmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(npmcmc_f)

ranks <- rank.probability(npmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "npSUCRA_small.csv")

pdf("npSUCRA_small_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(npmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"nparm_samll_or.xlsx")

####remove study early<2000 ####
arm_filter <- arm %>%
  filter(Year >= 2000)
parm_filter <- arm_filter %>% filter(`Study type` == "P")
nparm_filter <- arm_filter %>% filter(`Study type` == "NP")
set.seed(2025)

##
pnma_f <- mtc.network(parm_filter)
summary(pnma_f)
pmodel_f <- mtc.model(pnma_f, 
                      n.chain = 4,
                      type = "consistency", 
                      likelihood = "binom", 
                      link = "logit",         ##logit for OR  log for RR
                      linearModel = "random",
                      dic =  T)

pmcmc_f <- mtc.run(pmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(pmcmc_f)

ranks <- rank.probability(pmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "pSUCRA_year.csv")

pdf("pSUCRA_year_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(pmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"parm_year_or.xlsx")
#
npnma_f <- mtc.network(nparm_filter)
summary(npnma_f)
npmodel_f <- mtc.model(npnma_f, 
                       n.chain = 4,
                       type = "consistency", 
                       likelihood = "binom", 
                       link = "logit",         ##logit for OR  log for RR
                       linearModel = "random",
                       dic =  T)

npmcmc_f <- mtc.run(npmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(npmcmc_f)

ranks <- rank.probability(npmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "npSUCRA_year.csv")

pdf("npSUCRA_year_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(npmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"nparm_year_or.xlsx")


#######remove continuously monitored duration (days) < 3 &NA #####
arm_filter <- arm %>%
  filter(!(arm$`continuously monitored duration (days)`%in% c(0,1,2,NA,"Until ICU discharge")))
parm_filter <- arm_filter %>% filter(`Study type` == "P")
nparm_filter <- arm_filter %>% filter(`Study type` == "NP")
set.seed(2025)

##
pnma_f <- mtc.network(parm_filter)
summary(pnma_f)
pmodel_f <- mtc.model(pnma_f, 
                      n.chain = 4,
                      type = "consistency", 
                      likelihood = "binom", 
                      link = "logit",         ##logit for OR  log for RR
                      linearModel = "random",
                      dic =  T)

pmcmc_f <- mtc.run(pmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(pmcmc_f)

ranks <- rank.probability(pmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "pSUCRA_ecg.csv")

pdf("pSUCRA_ecg_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(pmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"parm_ecg_or.xlsx")
#
npnma_f <- mtc.network(nparm_filter)
summary(npnma_f)
npmodel_f <- mtc.model(npnma_f, 
                       n.chain = 4,
                       type = "consistency", 
                       likelihood = "binom", 
                       link = "logit",         ##logit for OR  log for RR
                       linearModel = "random",
                       dic =  T)

npmcmc_f <- mtc.run(npmodel_f, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(npmcmc_f)

ranks <- rank.probability(npmcmc_f, preferredDirection = -1)   # -1 = 低值更好
sucra_raw <- colMeans(apply(t(ranks), 2, cumsum)[-nrow(ranks), ])
sucra_sorted <- sort(sucra_raw, decreasing = TRUE)
sucra_export <- round(sucra_sorted, 4)
write.csv(sucra_export, "npSUCRA_ecg.csv")

pdf("npSUCRA_ecg_plot.pdf", width = 10, height = 7) 
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
rel_eff <- relative.effect(npmcmc_f, t1 = "Control")

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

write.xlsx(final_data,"nparm_ecg_or.xlsx")
