library(tidyverse)
library(devEMF)
#####p#######

raw_data <- read.csv("pleaguetable.csv", row.names = 1, check.names = FALSE)
interventions <- rownames(raw_data)
league_long <- as.data.frame(as.matrix(raw_data)) %>%
  rownames_to_column("Row") %>%
  pivot_longer(-Row, names_to = "Col", values_to = "RawText") %>%
  mutate(
    OR = as.numeric(str_extract(RawText, "^[0-9.]+")),
    Lower = as.numeric(str_extract(RawText, "(?<=\\()[0-9.]+")),
    Upper = as.numeric(str_extract(RawText, "(?<=, )[0-9.]+")),
    CleanRow = Row %>% str_replace_all("[_-]", " "), 
    WrappedRow = str_wrap(CleanRow, width = 1),
    DisplayLabel = ifelse(Row == Col, 
                          WrappedRow, 
                          paste0(sprintf("%.2f", OR), "\n(", sprintf("%.2f", Lower), " to ", sprintf("%.2f", Upper), ")")),
    Row = factor(Row, levels = interventions),
    Col = factor(Col, levels = interventions)
  ) %>%
  filter(as.numeric(Row) >= as.numeric(Col)) %>%
  mutate(
    fill_group = case_when(
      Row == Col ~ "Diag",                      
      OR >= 1 ~ "Insignificant", 
      OR < 0.5 ~ "Strong",                       
      OR >= 0.5 & OR <= 0.8 ~ "Moderate",         
      OR > 0.8 & OR < 1.0 ~ "Light",              
      TRUE ~ "Insignificant"
    )
  )

p <- ggplot(league_long, aes(x = Col, y = fct_rev(Row))) +
  geom_tile(aes(fill = fill_group), color = "white", size = 0.05) +
  geom_text(aes(label = DisplayLabel), 
            color = "white",
            size = 1.8, 
            lineheight = 0.85,
            fontface = "plain") +
  scale_fill_manual(values = c(
    "Diag" = "#2D6A4F",           
    "Strong" = "#2D6A4F",         
    "Moderate" = "#74A38A",       
    "Light" = "#95C5AF",          
    "Insignificant" = "#9EA3A8"   
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed()
emf(file = "pLeagueTable.emf", width = 16, height = 16)
print(p)
dev.off()





#######np##########

raw_data <- read.csv("npleaguetable.csv", row.names = 1, check.names = FALSE)
interventions <- rownames(raw_data)
league_long <- as.data.frame(as.matrix(raw_data)) %>%
  rownames_to_column("Row") %>%
  pivot_longer(-Row, names_to = "Col", values_to = "RawText") %>%
  mutate(
    OR = as.numeric(str_extract(RawText, "^[0-9.]+")),
    Lower = as.numeric(str_extract(RawText, "(?<=\\()[0-9.]+")),
    Upper = as.numeric(str_extract(RawText, "(?<=, )[0-9.]+")),
    CleanRow = Row %>% str_replace_all("[_-]", " "), 
    WrappedRow = str_wrap(CleanRow, width = 1),
    DisplayLabel = ifelse(Row == Col, 
                          WrappedRow, 
                          paste0(sprintf("%.2f", OR), "\n(", sprintf("%.2f", Lower), " to ", sprintf("%.2f", Upper), ")")),
    Row = factor(Row, levels = interventions),
    Col = factor(Col, levels = interventions)
  ) %>%
  filter(as.numeric(Row) >= as.numeric(Col)) %>%
  mutate(
    fill_group = case_when(
      Row == Col ~ "Diag",                      
      OR >= 1 ~ "Insignificant", 
      OR < 0.5 ~ "Strong",                       
      OR >= 0.5 & OR <= 0.8 ~ "Moderate",         
      OR > 0.8 & OR < 1.0 ~ "Light",              
      TRUE ~ "Insignificant"
    )
  )

p <- ggplot(league_long, aes(x = Col, y = fct_rev(Row))) +
  geom_tile(aes(fill = fill_group), color = "white", size = 0.05) +
  geom_text(aes(label = DisplayLabel), 
            color = "white",
            size = 2.2, 
            lineheight = 0.85,
            fontface = "plain") +
  scale_fill_manual(values = c(
    "Diag" = "#003366",           
    "Strong" = "#003366",         
    "Moderate" = "#48ADE1",       
    "Light" = "#8ECAE6",          
    "Insignificant" = "#9EA3A8"   
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed()
emf(file = "npLeagueTable.emf", width = 10, height = 10)
print(p)
dev.off()
