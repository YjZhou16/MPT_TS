#================环境======================
library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)
library(ggplot2)
library(tseries)
library(rugarch)
library(FinTS)
library(gridExtra)
setwd('D:\\AAA桌面\\class\\time_series\\mpt_source\\')
file_path <- "D:\\AAA桌面\\class\\time_series\\mpt_source\\mpt_histdata.xlsx"
data <- read_excel(file_path, sheet = 3)
head(data)
table(is.na(data))

#===========提取"Rate: mean"的行===========
rate_mean <- data %>%
  filter(field == "Rate: mean") %>%
  arrange(date) %>%
  select(date, value)

rate_mean$value <- as.numeric(rate_mean$value)

#===========对相同日期求均值===========
rate_mean_daily <- rate_mean %>%
  group_by(date = as.Date(date)) %>%
  summarize(value = mean(value, na.rm = TRUE))

#===========划分数据集===========
#选取了5月2号之前的数据进行fit
#留了一周的数据进行预测对比
train_cutoff <- as.Date("2025-05-02")  
train_rate <- rate_mean_daily %>% filter(date <= train_cutoff)
test_rate <- rate_mean_daily %>% filter(date > train_cutoff)



#===================ADF检验===================
adf_test <- adf.test(train_rate$value, alternative = "stationary")
print(adf_test)


if(adf_test$p.value > 0.05){
  print('序列是非平稳的')
}else{
  print('序列是平稳的')
}

ts_train <- ts(train_rate$value, start = c(year(min(train_rate$date)), yday(min(train_rate$date))), frequency = 365)
ts_test <- ts(test_rate$value, start = c(year(min(test_rate$date)), yday(min(test_rate$date))), frequency = 365)

ts_train_diff <- diff(ts_train, differences = 1)
adf_test_diff <- adf.test(ts_train_diff, alternative = "stationary")
print(adf_test_diff)
raw_dates <- seq.Date(from = as.Date("2023-03-31"),
                      by = "day",
                      length.out = length(ts_train))
raw_data <- tibble(
  date = raw_dates,
  value = as.numeric(ts_train)
)
diff_dates <- raw_dates[-1]
diff_data <- tibble(
  date = diff_dates,
  diff_value = as.numeric(diff(ts_train))
)

adf_test_diff <- adf.test(diff_data$diff_value, alternative = "stationary")
ggplot(raw_data, aes(x = date, y = value)) +
  geom_line(color = "darkorange") +
  labs(title = "原始联邦基金利率预期序列", x = "日期", y = "利率均值") +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),  # 浅灰色背景
  )

ggplot(diff_data, aes(x = date, y = diff_value)) +
  geom_line(color = "steelblue") +
  labs(title = "一阶差分后的序列", x = "日期", y = "差分值") +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),  # 浅灰色背景
  )

#===============纯随机性检验==================
lb_result <- Box.test(train_rate$value, lag = 20, type = "Ljung-Box")

#===========自相关、偏自相关系数==============
ggAcf(diff_data$diff_value, lag.max = 50) + 
  ggtitle("一阶差分后利率自相关图") +
  theme_minimal()
ggPacf(diff_data$diff_value, lag.max = 50) + 
  ggtitle("一阶差分后利率偏自相关图") +
  theme_minimal()

#=================ARIMA建模与预测=================
fit_rate <- auto.arima(
  ts_train, lambda = 0,
  d = 1, max.p = 3, max.q = 3,
  ic = 'aic', stepwise = FALSE,
  approximation = FALSE
)
summary(fit_rate)
model_summary <- capture.output(summary(fit_rate))
writeLines(model_summary, "fit_rate_summary.txt")

forecasted <- forecast(fit_rate, h = length(ts_test))
orig_df <- rate_mean_daily

fit_df_rate <- data.frame(date = train_rate$date, value = as.numeric(fit_rate$fitted))
ggplot() +
  geom_line(data = orig_df %>% filter(date <= train_cutoff), 
            aes(x = date, y = value, color = "原始数据", linetype = "原始数据"), 
            alpha = 0.7, size = 1) +   # 加粗线条
  geom_line(data = fit_df_rate, 
            aes(x = date, y = value, color = "拟合结果", linetype = "拟合结果"),
            size = 0.8) +
  #scale_color_manual(values = c("原始数据" = "#2b2e77", "拟合结果" = "#F9BEB9")) +
  scale_color_manual(values = c("原始数据" = "#6755a6", "拟合结果" = "#F9BEB9")) +
  scale_linetype_manual(values = c("原始数据" = "solid", "拟合结果" = "dashed")) +
  ggtitle('利率序列ARIMA 拟合结果与原始结果对比') +
  xlab('Date') + ylab('Value') +
  #theme_minimal()+
  #theme(legend.title = element_blank()+
  theme(
        panel.background = element_rect(fill = "#f0f0f0", color = NA),  # 浅灰色背景
        )

#==================预测效果评估===================
rmse <- sqrt(mean((as.numeric(forecasted$mean) - as.numeric(ts_test))^2))
mae <- mean(abs(as.numeric(forecasted$mean) - as.numeric(ts_test)))
print(paste("RMSE:", round(rmse, 4)))
print(paste("MAE:", round(mae, 4)))

#=================预测对比数据框==================
pred_df <- data.frame(date = test_rate$date, value = as.numeric(forecasted$mean))

ggplot() +
  geom_line(data = orig_df %>% filter(date > train_cutoff), 
            aes(x = date, y = value, color = "实际值"), 
            alpha = 0.6, size = 1.2) +
  geom_point(data = orig_df %>% filter(date > train_cutoff), 
             aes(x = date, y = value, color = "实际值", shape = "实际值"), 
             fill = "#108b96", alpha = 0.7, size = 2, stroke = 2) +
  geom_line(data = pred_df, 
            aes(x = date, y = value, color = "预测值"), 
            alpha = 0.6, size = 1.2) +
  geom_point(data = pred_df, 
             aes(x = date, y = value, color = "预测值", shape = "预测值"), 
             fill = "#f3993a", alpha = 0.7, size = 2, stroke = 2) +
  scale_color_manual(
    name = "图例", values = c("实际值" = "#108b96", "预测值" = "#f3993a")
  ) +
  scale_shape_manual(
    name = "图例", values = c("实际值" = 21, "预测值" = 21)
  ) +
  ggtitle('ARIMA Prediction vs. Actual') +
  xlab('Date') + ylab('Value') +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    legend.position = "top"
  )

#===================残差检验===================
checkresiduals(fit_rate)
#=================残差白噪声检验=================
Box.test(residuals(fit_rate), lag=20, type="Ljung-Box")

res <- residuals(fit_rate) 
res_std <- (res - mean(res)) / sd(res)
qqnorm(res_std)
qqline(res_std, col = "red")


#===================异方差检验===================
arch_test <- ArchTest(residuals(fit_rate), lags = 12)
print(arch_test) #不存在异方差性 不需要构建GARCH

