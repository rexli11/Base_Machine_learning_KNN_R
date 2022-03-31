library(tidyverse)
library(dplyr)
library(ggplot2)

# ====================================================
# Phase1 : Import data 載入數據
# ====================================================
# 載入數據集位置
setwd("D:\\Github_version_file_R\\data_set\\marchine_learning_data\\KNN")

# 載入已知數據集
knn_df <- read.csv("machine_learn_R.csv")

# ====================================================
# Phase2 : Create forecast data
# ====================================================
# 自訂義年齡與收入
age <- as.numeric(readline(prompt = "請輸入年齡 : "))
income <- as.numeric(readline(prompt = "請輸入年收入/萬元 : "))

# 建立需預測數據
simulation_df <- tibble::tribble(
    ~name, ~age, ~income,
    "Rex", age, income # 導入輸入的年齡與收入
)

# ====================================================
# Phase3 : Correlation diagram
# ====================================================
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
        x = simulation_df$age, # 將測試的數據畫上去
        y = simulation_df$income,
        color = "black",
        size = 5
    )

# ====================================================
# Phase4 : Calculate the values required for learning
# ====================================================
# 計算特徵的平均值與標準差
avg_age <- mean(knn_df$age_year) # age avg
sd_age <- sd(knn_df$age_year)
avg_income <- mean(knn_df$income_million)
sd_income <- sd(knn_df$income_million)

# 調整欄位型態
knn_df$age_year <- as.numeric(knn_df$age_year)
knn_df$income_million <- as.numeric(knn_df$income_million)

# knn_df進行單位標準化
knn_df <- knn_df %>%
    mutate(
        age_stand = (knn_df$age_year - avg_age) / sd_age,
        income_stand = (knn_df$income_million - avg_income) / sd_income
    )

# simulation_df進行單位標準化
simulation_df <- simulation_df %>%
    mutate(
        age_stand = (simulation_df$age - avg_age) / sd_age,
        income_stand = (simulation_df$income - avg_income) / sd_income
    )

# knn_df增加相似度欄位
knn_df <- knn_df %>%
    mutate(
        similarity = (sqrt((simulation_df$age_stand - knn_df$age_stand)^2 + (simulation_df$income_stand - knn_df$income_stand)^2))
    )

# check數據
knn_df$age_stand
knn_df$income_stand
knn_df$similarity
simulation_df

# 將相關性最符合的數據存入
simulation_similarity <- min(knn_df$similarity)

# 找出符合數值的數據位置
sim_loc <- which(knn_df$similarity == simulation_similarity)

# 存入符合的數據列
sim_pro <- knn_df[sim_loc:sim_loc, ]

# 印出預測商品
print(sim_pro$buy_product)