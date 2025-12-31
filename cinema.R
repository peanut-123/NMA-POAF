library(tidyverse)  
library(readxl)      
library(dplyr)
library(stringr)
setwd("D:\\liuchenyu\\paper\\network\\paper")
arm_level <- read_excel("article_arm.xlsx")

library(dplyr)
library(stringr)
#Due to the issue of the web version of cinema forcing alphabetical order, renaming the control can ensure that the control is in position t2.
cinema <- arm_level %>%
  select(
    study = study_id,
    id = No,
    t = ARM,
    r = ARM_POAF_events,
    n = ARM_n_size,
    rob = ROB2,
    indirectness = Indirectness
  ) %>%
  mutate(t = ifelse(t == "Control", "z_Control", t)) %>%
  arrange(id) %>%
  group_by(id) %>%
  arrange(t == "z_Control", .by_group = TRUE) %>%
  ungroup()
table(cinema$t)
unique(cinema$t)
write.csv(cinema, "cinema.csv", row.names = FALSE)




