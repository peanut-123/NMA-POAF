library(readxl)
library(tidyverse)
library(dplyr)
arm <- read_excel("article_arm.xlsx", sheet = 1)
unique(arm$ARM)
unique(arm$ARM[arm$`Study type` == "P"])
unique(arm$ARM[arm$`Study type` == "NP"])
sum(arm$ARM_n_size)
sum(arm$ARM_n_size[arm$`Study type` == "P"])
sum(arm$ARM_n_size[arm$`Study type` == "NP"])  

#####for disaplay in SP Characteristics of Included Studies #####
df=arm
colnames(df)
df_clean <- df %>%
  arrange(No) %>%
  group_by(No) %>%
  arrange(arm == "Control", .by_group = TRUE) %>%
  ungroup()

df_sorted <- df_clean %>%
  mutate(Study_type = factor(`Study type`, levels = c("P", "NP"))) %>%
  group_by(No) %>%
  mutate(intervention_group = first(ARM)) %>%
  ungroup() %>%
  arrange(Study_type, intervention_group, No) %>%
  mutate(`Study type` = as.character(Study_type)) %>%
  select(-intervention_group, -Study_type)

library(flextable)
library(officer)
colnames(df_sorted)
df_to_show <- df_sorted %>% select(1, 4:9, 13:21)
colnames(df_to_show)

df_to_show <- df_to_show  %>%
  rename(
    `No` = No,`Id` = study_id, `Country` = Country, `Register` = `Trials Register`,
    `Type` = `Study type`, `Total POAF` = `POAF events`, `Arm` = ARM,
    `Route` = `Administration route`, `Arm N` = `ARM_n_size`, `Arm POAF` = `ARM_POAF_events`,
    `Pre-op` = `interv_detail_pre-op`, `In-op` = `interv_detail_in-op`, `Post-op` = `interv_detail_post-op`
  )
colnames(df_to_show)

cols_to_merge <- names(df_to_show)[1:9]

ft <- flextable(df_to_show) %>%
  merge_v(j = "No", target = cols_to_merge) %>% 
  theme_booktabs() %>%
  font(fontname = "Arial Narrow", part = "all") %>%
  fontsize(size = 7, part = "all") %>% 
  width(j = c("No",  "Type", "Center","Open-label","N","Total POAF","Route","Arm N","Arm POAF"), width = 0.35) %>%
  width(j = c("Country"), width = 0.5) %>%
  width(j = c("Id", "Register"), width = 0.7) %>%
  width(j = "Arm", width = 0.8) %>%
  width(j = c("Pre-op", "In-op", "Post-op"), width = 1.3) %>% 
  align(align = "center", part = "all") %>%
  valign(valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1.5)) %>%
  hline(i = 1, part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(border = fp_border(width = 1.5)) %>%
  hline(i = which(df_to_show$No != lead(df_to_show$No)), 
        border = fp_border(color = "gray90", width = 0.8)) %>%
  fix_border_issues()

doc <- read_docx() %>%
  body_add_par("Characteristics of Included Studies", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_end_section_landscape()

print(doc, target = "Characteristics of Included Studies.docx")