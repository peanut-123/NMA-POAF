library(readxl)
library(tidyverse)
library(dplyr)
library(gemtc)
library(igraph)
library(scales)

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
###p.nmaplot
set.seed(2025)
pnma <- mtc.network(parm)
raw_df <- pnma$data
edges_df <- raw_df %>%
  select(study, treatment) %>%
  mutate(treatment = as.character(treatment)) %>% 
  inner_join(raw_df %>% 
               select(study, treatment) %>% 
               mutate(treatment = as.character(treatment)), 
             by = "study", relationship = "many-to-many") %>%
  filter(treatment.x < treatment.y) %>% 
  group_by(treatment.x, treatment.y) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  rename(from = treatment.x, to = treatment.y)
nodes_df <- raw_df %>%
  group_by(treatment) %>%
  summarise(total_n = sum(sampleSize, na.rm = TRUE)) %>%
  rename(name = treatment)
g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = FALSE)
V(g)$size <- scales::rescale(nodes_df$total_n, to = c(4, 18))
E(g)$width <- rescale(edges_df$weight, to = c(0.8, 7))
edge_colors <- alpha("gray55", scales::rescale(edges_df$weight, to = c(0.3, 0.8)))
n_vertices <- vcount(g)
L <- layout_in_circle(g)

angles <- atan2(L[,2], L[,1])
pdf("pNetworkplot.pdf", width = 12, height = 12)
par(mar = c(6, 6, 6, 6)) 
plot(g,
     layout = L,
     rescale = FALSE,             
     asp = 1,                     
     xlim = c(-1.3, 1.3),         
     ylim = c(-1.3, 1.3),
     vertex.color = "#006D5B", 
     vertex.frame.color = "white",
     vertex.label = NA,           
     edge.color = "grey85",
     edge.width = scales::rescale(E(g)$weight, to = c(1, 8)), 
     edge.curved = 0.15)
label_dist <- 1.05  
text(
  x = L[,1] * label_dist, 
  y = L[,2] * label_dist, 
  labels = V(g)$name, 
  pos = ifelse(abs(angles) < pi/2, 4, 2), 
  cex = 0.6,
  col = "black"
)
dev.off()    #Then make some adjustments manually

###np.nmaplot
set.seed(2025)
npnma <- mtc.network(nparm)
raw_df <- npnma$data
edges_df <- raw_df %>%
  select(study, treatment) %>%
  mutate(treatment = as.character(treatment)) %>% 
  inner_join(raw_df %>% 
               select(study, treatment) %>% 
               mutate(treatment = as.character(treatment)), 
             by = "study", relationship = "many-to-many") %>%
  filter(treatment.x < treatment.y) %>% 
  group_by(treatment.x, treatment.y) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  rename(from = treatment.x, to = treatment.y)
nodes_df <- raw_df %>%
  group_by(treatment) %>%
  summarise(total_n = sum(sampleSize, na.rm = TRUE)) %>%
  rename(name = treatment)
g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = FALSE)
V(g)$size <- scales::rescale(nodes_df$total_n, to = c(4, 18))
E(g)$width <- rescale(edges_df$weight, to = c(0.8, 7))
edge_colors <- alpha("gray55", scales::rescale(edges_df$weight, to = c(0.3, 0.8)))
n_vertices <- vcount(g)
L <- layout_in_circle(g)
angles <- atan2(L[,2], L[,1])
pdf("npNetworkplot.pdf", width = 12, height = 12)
par(mar = c(6, 6, 6, 6)) 
plot(g,
     layout = L,
     rescale = FALSE,             
     asp = 1,                     
     xlim = c(-1.3, 1.3),         
     ylim = c(-1.3, 1.3),
     vertex.color = "#1B4F72", 
     vertex.frame.color = "white",
     vertex.label = NA,           
     edge.color = "grey85",
     edge.width = scales::rescale(E(g)$weight, to = c(1, 8)), 
     edge.curved = 0.15)
label_dist <- 1.05  
text(
  x = L[,1] * label_dist, 
  y = L[,2] * label_dist, 
  labels = V(g)$name, 
  pos = ifelse(abs(angles) < pi/2, 4, 2), 
  cex = 0.6,
  col = "black"
)
dev.off()   #Then make some adjustments manually







