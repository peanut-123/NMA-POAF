library(openxlsx)

arm_level <- read_excel("article_arm.xlsx")
pmcmc <- readRDS("pmcmc.RDS")
npmcmc <- readRDS("npmcmc.RDS")
arm  <- arm_level
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

#p.nnt
pcontrol_data <- subset(parm, treatment == "Control")

p_b <- sum(pcontrol_data$responders) / sum(pcontrol_data$sampleSize)

rel_eff <- relative.effect(pmcmc, t1 = "Control")
rel_sum <- summary(rel_eff)
or_table <- exp(rel_sum$summaries$quantiles[, c("50%", "2.5%", "97.5%")])

or_to_nnt <- function(OR, Pb) {
  p_t <- (OR * Pb) / (1 - Pb + (OR * Pb))
  rd <- abs(p_t - Pb)
  return(1 / rd)
}

nnt_table <- apply(or_table, 2, function(x) or_to_nnt(x, p_b))

export_df <- data.frame(
  Treatment = rownames(or_table),
  Baseline_Risk = round(p_b, 4),
  OR = round(or_table[, "50%"], 3),
  OR_95_CrI = paste0(round(or_table[, "2.5%"], 3), " to ", round(or_table[, "97.5%"], 3)),
  NNT = round(nnt_table[, "50%"], 2),
  NNT_95_CrI = paste0(round(nnt_table[, "2.5%"], 2), " to ", round(nnt_table[, "97.5%"], 2))
)

write.xlsx(export_df, file = "pNNT.xlsx", sheetName = "Results", rowNames = FALSE)
#np.nnt

npcontrol_data <- subset(nparm, treatment == "Control")

p_b <- sum(npcontrol_data$responders) / sum(npcontrol_data$sampleSize)

rel_eff <- relative.effect(npmcmc, t1 = "Control")
rel_sum <- summary(rel_eff)
or_table <- exp(rel_sum$summaries$quantiles[, c("50%", "2.5%", "97.5%")])

or_to_nnt <- function(OR, Pb) {
  p_t <- (OR * Pb) / (1 - Pb + (OR * Pb))
  rd <- abs(p_t - Pb)
  return(1 / rd)
}

nnt_table <- apply(or_table, 2, function(x) or_to_nnt(x, p_b))

export_df <- data.frame(
  Treatment = rownames(or_table),
  Baseline_Risk = round(p_b, 4),
  OR = round(or_table[, "50%"], 3),
  OR_95_CrI = paste0(round(or_table[, "2.5%"], 3), " to ", round(or_table[, "97.5%"], 3)),
  NNT = round(nnt_table[, "50%"], 2),
  NNT_95_CrI = paste0(round(nnt_table[, "2.5%"], 2), " to ", round(nnt_table[, "97.5%"], 2))
)

write.xlsx(export_df, file = "npNNT.xlsx", sheetName = "Results", rowNames = FALSE)
