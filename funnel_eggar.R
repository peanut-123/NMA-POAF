 library(netmeta)
 library(readxl)
 library(dplyr)
 setwd("D:/liuchenyu/paper/network/paper")
 arm_level <- read_excel("article_arm.xlsx") 
 pairs <- pairwise(treat = ARM, 
                   event = ARM_POAF_events, 
                   n = ARM_n_size, 
                   studlab = No, 
                   data = arm_level, 
                   sm = "OR")
 net_result <- netmeta(TE, seTE, treat1, treat2, studlab, 
                       data = pairs, 
                       ref = "Control", 
                       sm = "OR")
 all_treats <- net_result$trts
 my_order <- c("Control", all_treats[all_treats != "Control"])
 funnel(net_result, 
        order = my_order, 
        pch = 19, 
        col = rgb(0.1, 0.4, 0.7, 0.3), 
        cex = 0.7,                     
        method.bias = "Egger",                 
        xlab = "Comparison-adjusted outcome (Log OR)",
        legend =F )
 metabias(net_result, method.bias = "Egger", 
          order = my_order)
 ## remove small arm, n<100
 arm_level_filtered <- arm_level %>%
   filter(N >= 100) 
length(unique(arm_level_filtered$No)) 
 
pairs2 <- pairwise(treat = ARM, 
                  event = ARM_POAF_events, 
                  n = ARM_n_size, 
                  studlab = No, 
                  data = arm_level_filtered, 
                  sm = "OR")
 
net_result2 <- netmeta(TE, seTE, treat1, treat2, studlab, 
                      data = pairs2, 
                      ref = "Control", 
                      sm = "OR")
 
all_treats2 <- net_result2$trts
my_order2 <- c("Control", all_treats2[all_treats2 != "Control"])
funnel(net_result2, 
       order = my_order2, 
       pch = 19, 
       col = rgb(0.1, 0.4, 0.7, 0.3), 
       cex = 0.7,                     
       method.bias = "Egger",                 
       xlab = "Comparison-adjusted outcome (Log OR)",
       legend =F )
metabias(net_result2, method.bias = "Egger", 
         order = my_order2)
####Leave-one-out Sensitivity Analysis for Publication Bias



raw_data <- net_result$data
control_name <- "Control" # 自动获取参考组/对照组名称

# 2. 获取所有积极干预措施（除去对照组）
active_treats <- setdiff(net_result$trts, control_name)

# 3. 创建结果存储框
leave_one_out_egger <- data.frame(
  Excluded_Drug = character(), 
  Egger_P = numeric(), 
  stringsAsFactors = FALSE
)

counter <- 0

cat("开始循环报数：\n")

# 3. 执行循环
for (drug in active_treats) {
  counter <- counter + 1
  cat(counter, " ") # 报数 1 2 3...
  
  # 过滤数据
  sub_data <- net_result$data[net_result$data$treat1 != drug & 
                                net_result$data$treat2 != drug, ]
  
  try({
    # 重新计算网络
    tmp_net <- netmeta(TE, seTE, treat1, treat2, studlab, 
                       data = sub_data, sm = net_result$sm, random = TRUE)
    
    # 运行 Egger 检验，带上 order 确保不报错
    tmp_bias <- metabias(tmp_net, method.bias = "linreg", order = tmp_net$trts)
    
    # 存入结果
    leave_one_out_egger <- rbind(leave_one_out_egger, 
                                 data.frame(Excluded_Drug = drug, 
                                            Egger_P = as.numeric(tmp_bias$p.value)))
  }, silent = TRUE)
}

# 4. 结果排序：看哪个药被剔除后 P 值最大
leave_one_out_egger <- leave_one_out_egger[order(-leave_one_out_egger$Egger_P), ]
leave_one_out_egger$P_Scientific <- format(leave_one_out_egger$Egger_P, scientific = TRUE)

# 2. 写入 Excel
writexl::write_xlsx(leave_one_out_egger, "NMA_Egger_Sensitivity_Analysis.xlsx")
cat("\n\n--- 分析完成 --- \n")
print(leave_one_out_egger)


