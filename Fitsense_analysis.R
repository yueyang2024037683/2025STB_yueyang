# ============================================================
# Fitsense Customer Segmentation Analysis
# 고객 세분화 분석
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(readxl)
library(cluster)
library(writexl)
library(openxlsx)
library(scatterplot3d)
library(fmsb)

# 如果第一次运行且没有安装包，再单独运行：
# install.packages(c(
#   "readxl",
#   "cluster",
#   "writexl",
#   "openxlsx",
#   "scatterplot3d",
#   "fmsb"
# ))


# ------------------------------------------------------------
# 2. Import data
# ------------------------------------------------------------

Fitsense <- read_excel(
  "F:/한양대학교/汉阳大学修業/경영학전공相关学习资料和作业以及成绩/애널리틱스와AI/1st Lab Pratice_고객 세분화와 프로파일링-악양_2024037683.xlsx",
  sheet = "Fitsense_Lab Practice_Dataset"
)

# 检查数据结构
str(Fitsense)


# ------------------------------------------------------------
# 3. Data preprocessing
# ------------------------------------------------------------

# Categorical variables
Fitsense$gender <- as.factor(Fitsense$gender)
Fitsense$preferred_category <- as.factor(Fitsense$preferred_category)
Fitsense$channel <- as.factor(Fitsense$channel)

# Numerical variables
numeric_columns <- c(
  "age",
  "purchase_frequency",
  "avg_order_value",
  "days_since_last_purchase",
  "total_spent",
  "return_rate"
)

Fitsense[numeric_columns] <- lapply(
  Fitsense[numeric_columns],
  as.numeric
)

# 小数保留两位
Fitsense$avg_order_value <- round(Fitsense$avg_order_value, 2)
Fitsense$total_spent <- round(Fitsense$total_spent, 2)
Fitsense$return_rate <- round(Fitsense$return_rate, 2)

# 检查数据
str(Fitsense)
summary(Fitsense)

# 检查缺失值
colSums(is.na(Fitsense))


# ------------------------------------------------------------
# 4. Save cleaned data
# ------------------------------------------------------------

write_xlsx(
  Fitsense,
  "Fitsense_cleaned.xlsx"
)


# ------------------------------------------------------------
# 5. Optional: Save Excel with fixed 2-decimal formatting
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(
  wb,
  "Sheet1"
)

writeData(
  wb,
  sheet = "Sheet1",
  Fitsense
)

decimal2 <- createStyle(
  numFmt = "0.00"
)

# E = avg_order_value
# I = total_spent
# J = return_rate
addStyle(
  wb,
  sheet = "Sheet1",
  style = decimal2,
  rows = 2:(nrow(Fitsense) + 1),
  cols = c(5, 9, 10),
  gridExpand = TRUE,
  stack = TRUE
)

saveWorkbook(
  wb,
  "Fitsense_numeric_fixed2.xlsx",
  overwrite = TRUE
)


# ============================================================
# K-MEANS CLUSTERING
# ============================================================


# ------------------------------------------------------------
# 6. Select clustering variables
# ------------------------------------------------------------

cluster_data <- Fitsense[, c(
  "days_since_last_purchase",
  "purchase_frequency",
  "total_spent",
  "avg_order_value",
  "return_rate"
)]


# ------------------------------------------------------------
# 7. Standardization
# ------------------------------------------------------------

cluster_scaled <- scale(cluster_data)

# 检查标准化结果
apply(cluster_scaled, 2, mean)
apply(cluster_scaled, 2, sd)


# ------------------------------------------------------------
# 8. Elbow Method
# ------------------------------------------------------------

set.seed(123)

wcss <- numeric(10)

for (k in 1:10) {
  
  km <- kmeans(
    cluster_scaled,
    centers = k,
    nstart = 25,
    iter.max = 100
  )
  
  wcss[k] <- km$tot.withinss
}

print(wcss)

plot(
  1:10,
  wcss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "WCSS",
  main = "Elbow Method"
)


# ------------------------------------------------------------
# 9. Silhouette Analysis
# ------------------------------------------------------------

set.seed(123)

# 距离矩阵只计算一次
d <- dist(cluster_scaled)

sil_score <- numeric(9)

for (k in 2:10) {
  
  km <- kmeans(
    cluster_scaled,
    centers = k,
    nstart = 25,
    iter.max = 100
  )
  
  sil <- silhouette(
    km$cluster,
    d
  )
  
  sil_score[k - 1] <- mean(sil[, 3])
}

print(sil_score)

plot(
  2:10,
  sil_score,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "Average Silhouette Score",
  main = "Silhouette Analysis"
)

# 找出 Silhouette 最大值对应的 K
best_k <- which.max(sil_score) + 1

cat(
  "Best K based on Silhouette Score:",
  best_k,
  "\n"
)


# ------------------------------------------------------------
# 10. Final K-means model
# ------------------------------------------------------------

set.seed(123)

# 当前分析选择 K = 4
# 如果完全依据 Silhouette，可改为：
# final_k <- best_k

final_k <- 4

km_final <- kmeans(
  cluster_scaled,
  centers = final_k,
  nstart = 25,
  iter.max = 100
)

# 添加群集编号
Fitsense$cluster <- as.factor(
  km_final$cluster
)

# 每个 Cluster 人数
table(Fitsense$cluster)


# ------------------------------------------------------------
# 11. Cluster centers - standardized scale
# ------------------------------------------------------------

print(
  km_final$centers
)


# ------------------------------------------------------------
# 12. Convert cluster centers back to original scale
# ------------------------------------------------------------

centers_original <- sweep(
  km_final$centers,
  2,
  attr(cluster_scaled, "scaled:scale"),
  "*"
)

centers_original <- sweep(
  centers_original,
  2,
  attr(cluster_scaled, "scaled:center"),
  "+"
)

centers_original <- round(
  centers_original,
  2
)

print(
  centers_original
)


# ------------------------------------------------------------
# 13. 2D visualization
# ------------------------------------------------------------

cluster_col <- adjustcolor(
  1:final_k,
  alpha.f = 0.35
)

plot(
  Fitsense$purchase_frequency,
  Fitsense$avg_order_value,
  col = cluster_col[as.numeric(Fitsense$cluster)],
  pch = 19,
  xlab = "Purchase Frequency",
  ylab = "Average Order Value",
  main = paste(
    "K-means Clustering, K =",
    final_k
  )
)

legend(
  "topright",
  legend = levels(Fitsense$cluster),
  col = 1:final_k,
  pch = 19,
  title = "Cluster"
)


# ------------------------------------------------------------
# 14. 3D visualization
# ------------------------------------------------------------

scatterplot3d(
  x = Fitsense$purchase_frequency,
  y = Fitsense$total_spent,
  z = Fitsense$days_since_last_purchase,
  color = as.numeric(Fitsense$cluster),
  pch = 19,
  xlab = "Purchase Frequency",
  ylab = "Total Spent",
  zlab = "Days Since Last Purchase",
  main = paste(
    "3D K-means Clustering, K =",
    final_k
  )
)


# ------------------------------------------------------------
# 15. Cluster profiling
# ------------------------------------------------------------

cluster_profile <- aggregate(
  cluster_data,
  by = list(
    Cluster = Fitsense$cluster
  ),
  FUN = mean
)

cluster_profile[, -1] <- round(
  cluster_profile[, -1],
  2
)

print(
  cluster_profile
)


# ------------------------------------------------------------
# 16. Radar chart
# ------------------------------------------------------------

radar <- as.data.frame(
  km_final$centers
)

# Recency：
# days_since_last_purchase 越低，
# 实际表示客户越活跃。
# 因此乘以 -1，让数值越高代表越活跃。
radar$days_since_last_purchase <-
  -radar$days_since_last_purchase

colnames(radar) <- c(
  "Recency",
  "Frequency",
  "Total Spent",
  "Avg Order Value",
  "Return Rate"
)

# K = 4 时的客户群名称
cluster_names <- c(
  "低频高客单型",
  "高退货一般消费型",
  "高频高价值型",
  "低活跃低价值型"
)

rownames(radar) <- cluster_names

# Radar chart 范围
max_value <- 2
min_value <- -2

radar_plot <- rbind(
  max = rep(max_value, 5),
  min = rep(min_value, 5),
  radar
)

radarchart(
  radar_plot,
  axistype = 1,
  
  pcol = c(
    "red",
    "blue",
    "green",
    "orange"
  ),
  
  pfcol = c(
    rgb(1, 0, 0, 0.1),
    rgb(0, 0, 1, 0.1),
    rgb(0, 1, 0, 0.1),
    rgb(1, 0.5, 0, 0.1)
  ),
  
  plwd = 2,
  plty = 1,
  
  cglcol = "grey",
  cglty = 1,
  
  axislabcol = "grey",
  
  caxislabels = c(
    "-2",
    "-1",
    "0",
    "1",
    "2"
  ),
  
  vlcex = 0.8
)

legend(
  "topright",
  legend = cluster_names,
  col = c(
    "red",
    "blue",
    "green",
    "orange"
  ),
  lty = 1,
  lwd = 2,
  bty = "n"
)


# ------------------------------------------------------------
# 17. Save final results
# ------------------------------------------------------------

write_xlsx(
  list(
    Customer_Data = Fitsense,
    Cluster_Profile = cluster_profile,
    Cluster_Centers = as.data.frame(
      centers_original
    )
  ),
  "Fitsense_clustering_results.xlsx"
)
 

# ------------------------------------------------------------
# 18. Save R workspace
# ------------------------------------------------------------

save.image(
  "F:/한양대학교/2025STB_YUEYANG/2025STB_yueyang/Fitsense_analysis.RData"
)


# ============================================================
# END
# ============================================================