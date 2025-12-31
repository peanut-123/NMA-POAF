# 1. 加载必要库

library(rentrez)
library(progress) # 用于显示进度条

# 2. 安全设置 API Key
# 请将下方字符串替换为你从 NCBI 申请到的 Key
set_entrez_key("3c4e7ca166bbf8e4f7e5345f7f93fd9f2309")
setwd("D:\\liuchenyu\\paper\\network\\paper\\last")
# 3. 准备数据 (假设你的 ID 存在一个 csv 的第一列)
nct<- read.csv("NCT.csv")

nct_ids<- nct$NCT.Number

# 4. 初始化结果容器
final_results <- data.frame(
  NCT_ID = character(),
  Found = character(),
  PMID = character(),
  Title = character(),
  stringsAsFactors = FALSE
)

# 5. 创建进度条，让你知道还要等多久
pb <- progress_bar$new(
  total = length(nct_ids),
  format = "  进度 [:bar] :percent 剩余时间: :eta"
)

# 6. 开始循环检索
message("开始批量检索...")

for (i in 1:length(nct_ids)) {
  id <- nct_ids[i]
  pb$tick() # 更新进度条
  
  # 使用 tryCatch 防止网络抖动导致整个脚本中断
  result <- tryCatch({
    # 搜索
    search <- entrez_search(db = "pubmed", term = paste0(id, "[Secondary Source ID]"))
    
    if (search$count > 0) {
      # 抓取概要
      summ <- entrez_summary(db = "pubmed", id = search$ids[1])
      data.frame(NCT_ID = id, Found = "Yes", PMID = search$ids[1], Title = summ$title)
    } else {
      data.frame(NCT_ID = id, Found = "No", PMID = NA, Title = "No article found")
    }
  }, error = function(e) {
    # 如果报错（如网络超时），记录错误信息
    data.frame(NCT_ID = id, Found = "Error", PMID = NA, Title = as.character(e))
  })
  
  final_results <- rbind(final_results, result)
  
  # 既然有了 API Key，休眠时间可以缩短至 0.15 秒
  Sys.sleep(0.15) 
}
# install.packages("dplyr")
library(dplyr)
res <- nct %>%  left_join(final_results, by =c("NCT.Number" = "NCT_ID"))
library(dplyr)

# 使用减号 (-) 表示删除这些列
res_subset <- res %>% 
  select(-Primary.Outcome.Measures, -Secondary.Outcome.Measures)

# 现在尝试导出
library(writexl)
write_xlsx(res_subset, "clinical_trials_clean.xlsx")

          