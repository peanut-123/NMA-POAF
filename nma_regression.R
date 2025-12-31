library(dplyr)
library(gemtc)
library(readxl)
library(tidyverse)


setwd("D:\\liuchenyu\\paper\\network\\paper")
arm <- read_excel("article_arm.xlsx", sheet = 1)
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

##arm-level  to study-level
colnames(arm)
study_data <- arm %>%
  group_by(study) %>%
  summarise(
    N=first(N),
    type = first(`Study type`),
    year= first(Year),
    age = if(all(is.na(AGE))) NA_real_ else sum(AGE * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(AGE)], na.rm = TRUE),
    female=if(all(is.na(Female))) NA_real_ else sum(Female * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(Female)], na.rm = TRUE),
    hypertension = if(all(is.na(hypertension))) NA_real_ else sum(hypertension * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(hypertension)], na.rm = TRUE),
    MI= if(all(is.na(MI))) NA_real_ else sum(MI * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(MI)], na.rm = TRUE),
    diabetes = if(all(is.na(`Diabetes mellitus`))) NA_real_ else sum(`Diabetes mellitus` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`Diabetes mellitus`)], na.rm = TRUE),
    renal = if(all(is.na(renal))) NA_real_ else sum(renal * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(renal)], na.rm = TRUE),
    af = if(all(is.na(`AF history`))) NA_real_ else sum(`AF history` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`AF history`)], na.rm = TRUE),
    copd=if(all(is.na(copd))) NA_real_ else sum(copd * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(copd)], na.rm = TRUE),
    lvef = if(all(is.na(LVEF))) NA_real_ else sum(LVEF * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(LVEF)], na.rm = TRUE),
    la = if(all(is.na(`Left atrial diameter (cm)`))) NA_real_ else sum(`Left atrial diameter (cm)` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`Left atrial diameter (cm)`)], na.rm = TRUE),
    bb = if(all(is.na(bb))) NA_real_ else sum(bb * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(bb)], na.rm = TRUE),
    acei = if(all(is.na(`ACEI/ARB USE`))) NA_real_ else sum(`ACEI/ARB USE` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`ACEI/ARB USE`)], na.rm = TRUE),
    ccb = if(all(is.na(CCB))) NA_real_ else sum(CCB * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(CCB)], na.rm = TRUE),
    statin = if(all(is.na(STATIN))) NA_real_ else sum(STATIN * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(STATIN)], na.rm = TRUE),
    cpb_rate = if(all(is.na(`CPB rate`))) NA_real_ else sum(`CPB rate` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`CPB rate`)], na.rm = TRUE),
    cpb_time= if(all(is.na(`CPB time(min)`))) NA_real_ else sum(`CPB time(min)` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`CPB time(min)`)], na.rm = TRUE),
    cross = if(all(is.na(`cross time (min)`))) NA_real_ else sum(`cross time (min)` * sampleSize, na.rm = TRUE) / sum(sampleSize[!is.na(`cross time (min)`)], na.rm = TRUE)
  )
study_counts <- arm %>% count(study)
study_base = study_data
study_data_list <- lapply(1:nrow(study_base), function(i) {
  s_id <- study_base$study[i]
  n_rows <- study_counts$n[study_counts$study == s_id]
  df_temp <- as.data.frame(matrix(NA, nrow = n_rows, ncol = ncol(study_base)))
  colnames(df_temp) <- colnames(study_base)
  df_temp[1, ] <- study_base[i, ]
  df_temp$study <- s_id
  return(df_temp)
})
study_data <- do.call(rbind, study_data_list)

#####age####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(age))) %>% 
  pull(study) %>% 
  unique()
study_age_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_age_mr <- arm %>%
  filter(study %in% studies_to_keep)

age_mr <- mtc.network(data= arm_age_mr,studies = study_age_mr)

regressor <- list(coefficient="shared",
                  variable="age",
                  control="Control"
                  )
model.mr <- mtc.model(age_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)


####female####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(female))) %>% 
  pull(study) %>% 
  unique()
study_sex_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_sex_mr <- arm %>%
  filter(study %in% studies_to_keep)

sex_mr <- mtc.network(data= arm_sex_mr,studies = study_sex_mr)

regressor <- list(coefficient="shared",
                  variable="female",
                  control="Control"
)
model.mr <- mtc.model(sex_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)
#### lvef####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(lvef))) %>% 
  pull(study) %>% 
  unique()
study_lvef_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_lvef_mr <- arm %>%
  filter(study %in% studies_to_keep)

lvef_mr <- mtc.network(data= arm_lvef_mr,studies = study_lvef_mr)

regressor <- list(coefficient="shared",
                  variable="lvef",
                  control="Control"
)
model.mr <- mtc.model(lvef_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)
#####htn####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(hypertension))) %>% 
  pull(study) %>% 
  unique()
study_hypertension_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_hypertension_mr <- arm %>%
  filter(study %in% studies_to_keep)

hypertension_mr <- mtc.network(data= arm_hypertension_mr,studies = study_hypertension_mr)

regressor <- list(coefficient="shared",
                  variable="hypertension",
                  control="Control"
)
model.mr <- mtc.model(hypertension_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)





#####diabetes#####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(hypertension))) %>% 
  pull(study) %>% 
  unique()
study_hypertension_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_hypertension_mr <- arm %>%
  filter(study %in% studies_to_keep)

hypertension_mr <- mtc.network(data= arm_hypertension_mr,studies = study_hypertension_mr)

regressor <- list(coefficient="shared",
                  variable="hypertension",
                  control="Control"
)
model.mr <- mtc.model(hypertension_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)
######mi####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(MI))) %>% 
  pull(study) %>% 
  unique()
study_MI_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_MI_mr <- arm %>%
  filter(study %in% studies_to_keep)

MI_mr <- mtc.network(data= arm_MI_mr,studies = study_MI_mr)

regressor <- list(coefficient="shared",
                  variable="MI",
                  control="Control"
)
model.mr <- mtc.model(MI_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)
####cpb_rate####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(cpb_rate))) %>% 
  pull(study) %>% 
  unique()
study_cpb_rate_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_cpb_rate_mr <- arm %>%
  filter(study %in% studies_to_keep)

cpb_rate_mr <- mtc.network(data= arm_cpb_rate_mr,studies = study_cpb_rate_mr)

regressor <- list(coefficient="shared",
                  variable="cpb_rate",
                  control="Control"
)
model.mr <- mtc.model(cpb_rate_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)

#####cpb_time#####
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(cpb_time))) %>% 
  pull(study) %>% 
  unique()
study_cpb_time_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_cpb_time_mr <- arm %>%
  filter(study %in% studies_to_keep)

cpb_time_mr <- mtc.network(data= arm_cpb_time_mr,studies = study_cpb_time_mr)

regressor <- list(coefficient="shared",
                  variable="cpb_time",
                  control="Control"
)
model.mr <- mtc.model(cpb_time_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)

########xtime######
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(cross))) %>% 
  pull(study) %>% 
  unique()
study_cross_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_cross_mr <- arm %>%
  filter(study %in% studies_to_keep)

cross_mr <- mtc.network(data= arm_cross_mr,studies = study_cross_mr)

regressor <- list(coefficient="shared",
                  variable="cross",
                  control="Control"
)
model.mr <- mtc.model(cross_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)
#####bb######
studies_to_keep <- study_data %>%
  group_by(study) %>%
  filter(any(!is.na(year))) %>% 
  pull(study) %>% 
  unique()
study_year_mr <- study_data %>%
  filter(study %in% studies_to_keep)
arm_year_mr <- arm %>%
  filter(study %in% studies_to_keep)

year_mr <- mtc.network(data= arm_year_mr,studies = study_year_mr)

regressor <- list(coefficient="shared",
                  variable="year",
                  control="Control"
)
model.mr <- mtc.model(year_mr, 
                      n.chain = 4,
                      type = "regression", 
                      likelihood = "binom", 
                      link = "logit",         
                      linearModel = "random",
                      dic =  T,
                      regressor=regressor)
result.mr <- mtc.run(model.mr, n.adapt = 5000, n.iter = 100000, thin = 5)
summary(result.mr)



####forest#####

library(forestplot)

data <- data.frame(
  variable = c("Publication year", "Age", "Female rate", "LVEF (%)", 
               "Hypertension rate", "Diabetes rate", "Myocardial infarction rate", 
               "ON_pump rate", "CPB time", "Clamping time"),
  mean = c(0.135, 0.293, -0.202, 0.274, 0.076, -0.305, -0.055, 0.162, 0.128, 0.2134),
  lower = c(-0.083, 0.127, -0.387, -0.01, -0.116, -0.503, -0.292, -0.002, -0.079, -0.020),
  upper = c(0.353, 0.459, -0.019, 0.558, 0.270, -0.107, 0.182, 0.327, 0.336, 0.447),
  crl = c("-0.083 to 0.353", "0.127 to 0.459", "-0.387 to -0.019", "-0.01 to 0.558", 
          "-0.116 to 0.270", "-0.503 to -0.107", "-0.292 to 0.182", "-0.002 to 0.327", 
          "-0.079 to 0.336", "-0.020 to 0.447")
)

# 2. 构建用于显示的文本表格 (包含表头)
tabletext <- cbind(
  c("Variable", data$variable),
  c("Mean", format(round(data$mean, 3), nsmall = 3)),
  c("95% CrI", data$crl)
)

# 3. 绘图
forestplot(
  labeltext = tabletext,
  mean = c(NA, data$mean),   # 第一行是表头，所以填 NA
  lower = c(NA, data$lower),
  upper = c(NA, data$upper),
  is.summary = c(TRUE, rep(FALSE, nrow(data))), # 第一行加粗显示
  
  # X 轴设置
  xlab = "Regression Coefficient",
  grid = structure(0, gp = gpar(lty = 2, col = "gray")), # 在0处画虚线
  xticks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6),
  
  # 底部文字标注：<0 正相关，>0 负相关
  txt_gp = fpTxtGp(xlab = gpar(cex = 1), ticks = gpar(cex = 0.8)),
  title = "Forest Plot of Regression Coefficients",
  
  # 颜色与样式
  col = fpColors(box = "royalblue", line = "darkblue", summary = "black"),
  boxsize = 0.2,
  
  # 设置 X 轴下方的正负相关标注
  # 注意：通常回归系数 > 0 表示正相关，但根据你的要求进行了文字自定义
  clip = c(-0.6, 0.6),
  graph.pos = 2 # 森林图放在第二列后面
)

# 添加底部自定义标注 (手动在绘图区下方添加说明)
grid.text("Positively correlated with POAF<---", x = 0.4, y = 0.05, gp = gpar(col = "red", fontsize = 10))
grid.text("---> Negatively correlated with POAF", x = 0.7, y = 0.05, gp = gpar(col = "blue", fontsize = 10))




