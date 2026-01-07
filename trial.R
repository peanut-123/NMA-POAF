
library(rentrez)
library(progress)
set_entrez_key("your API")
nct<- read.csv("NCT.csv")

nct_ids<- nct$NCT.Number

final_results <- data.frame(
  NCT_ID = character(),
  Found = character(),
  PMID = character(),
  Title = character(),
  stringsAsFactors = FALSE
)
pb <- progress_bar$new(
  total = length(nct_ids),
  format = " process [:bar] :percent time: :eta"
)
for (i in 1:length(nct_ids)) {
  id <- nct_ids[i]
  pb$tick() 
  result <- tryCatch({
    search <- entrez_search(db = "pubmed", term = paste0(id, "[Secondary Source ID]")) 
    if (search$count > 0) {
      summ <- entrez_summary(db = "pubmed", id = search$ids[1])
      data.frame(NCT_ID = id, Found = "Yes", PMID = search$ids[1], Title = summ$title)
    } else {
      data.frame(NCT_ID = id, Found = "No", PMID = NA, Title = "No article found")
    }
  }, error = function(e) {
    data.frame(NCT_ID = id, Found = "Error", PMID = NA, Title = as.character(e))
  })
  final_results <- rbind(final_results, result)
  Sys.sleep(0.15) 
}
# install.packages("dplyr")
library(dplyr)
res <- nct %>%  left_join(final_results, by =c("NCT.Number" = "NCT_ID"))
library(dplyr)
res_subset <- res %>% 
  select(-Primary.Outcome.Measures, -Secondary.Outcome.Measures)
library(writexl)
write_xlsx(res_subset, "clinical_trials_clean.xlsx")


          
