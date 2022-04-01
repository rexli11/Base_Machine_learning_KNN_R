library(tidyverse)
library(dplyr)
library(ggplot2)

# ====================================================
# Phase1 : Import data
# 階段1 : 載入數據
# ====================================================
# 載入數據集位置
setwd("D:\\Github_version_file_R\\data_set\\marchine_learning_data\\KNN")

# Import learning data載入已知數據集
knn_df <- read.csv("machine_learn_R.csv")

# ====================================================
# Phase2 : Create forecast data
# 階段2 : 創建需預測數據
# ====================================================
# Define forecast age、income 自定義年齡與收入
age <- as.numeric(readline(prompt = "請輸入年齡 : "))
income <- as.numeric(readline(prompt = "請輸入年收入/萬元 : "))

# Create forecast data建立需預測數據
predict_df <- tibble::tribble(
    ~name, ~age, ~income,
    "Rex", age, income # 導入輸入的年齡與收入
)

# ====================================================
# Phase3 : Correlation diagram
# 階段3 : 畫出關聯圖
# ====================================================
# Correlation diagram between forecast data and learning data
# 繪製測試圖表與預測數據的關聯性
ggplot(
    data = knn_df, # 畫出原始的數據集圖
    mapping = aes(
        x = age_year,
        y = income_million,
        color = buy_product
    )
) +
    geom_point() +
    geom_point(
        x = predict_df$age, # 將測試的數據畫上去
        y = predict_df$income,
        color = "black",
        size = 5
    )

# ====================================================
# Phase4 : Calculate the values required for KNN learning
# 階段4 : 計算出KNN機器學習所需數據
# ====================================================
# Calculate frature avg、sd 計算特徵的平均值與標準差
avg_age <- mean(knn_df$age_year) # age avg
sd_age <- sd(knn_df$age_year) # income sd
avg_income <- mean(knn_df$income_million) # income avg
sd_income <- sd(knn_df$income_million) # income sd

# Change col type 調整欄位型態
knn_df$age_year <- as.numeric(knn_df$age_year) # convert to num
knn_df$income_million <- as.numeric(knn_df$income_million) # convert to num

# Unit standardization and create knn_df cols  knn_df進行單位標準化

knn_df <- knn_df %>%
    mutate( # 平均年收減平均年齡除以年齡標準差，Average annual income minus average age divided by the standard deviation of age
        age_stand = (knn_df$age_year - avg_age) / sd_age,
        income_stand = (knn_df$income_million - avg_income) / sd_income
    )

# simulation_df進行單位標準化
predict_df <- predict_df %>%
    mutate(
        age_stand = (predict_df$age - avg_age) / sd_age,
        income_stand = (predict_df$income - avg_income) / sd_income
    )

# Create similarity col knn_df增加相似度欄位
knn_df <- knn_df %>%
    mutate(
        similarity = (sqrt((predict_df$age_stand - knn_df$age_stand)^2 + (predict_df$income_stand - knn_df$income_stand)^2))
    )

# ====================================================
# Phase5 : Find most correlation predict product and print
# 階段5 : 找出最具相關性商品並列印
# ====================================================
# check數據
knn_df$age_stand
knn_df$income_stand
knn_df$similarity
predict_df

# 找出最接近預測數據之值
correlation_df <- min(knn_df$similarity)

# 確定符合數值的數據位置
accord_loc <- which(knn_df$similarity == correlation_df)

# 存入符合的數據列
predict_product <- knn_df[accord_loc:accord_loc, ]

# 印出預測商品
cat("依照年齡與年收入預測商品為 : ", predict_product$buy_product)