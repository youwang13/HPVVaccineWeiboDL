#检验流程
#极值点
#是否在+/-2SD之外（异常大或小）
#趋势：若极值点显著，则从极值点到极值点；若极值点不显著，则从头到尾
#不同维度对比

install.packages("forecast")
install.packages("dplyr")
library(forecast)
library(dplyr)

setwd("C:/YOU/Research/AI & Health/article/Results/时间序列分析")
data_risk = read.csv("month_HBM_risk.csv")
data_benefit = read.csv("month_HBM_benefit.csv")
data_hbmbarrier = read.csv("month_HBM_barrier.csv")
data_social = read.csv("month_ext_social.csv")
data_misinformation = read.csv("month_ext_misinformation.csv")
data_extbarrier = read.csv("month_ext_barrier.csv")
data_attitudes = read.csv("month_attitudes.csv")


###################################### risk
outliers = data_risk %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_risk$X1, mu = max(data_risk$X1), alternative = "less")
print(t_test_result)

outliers = data_risk %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

risk_max_value = which.max(data_risk$X1)
risk_max_month = data_risk$date_month[risk_max_value]
risk_max_month

risk_min_value = which.min(data_risk$X1)
risk_min_month = data_risk$date_month[risk_min_value]
risk_min_month

risk_start_date = "2018-04"
risk_end_date = "2023-06"

subset_data_risk = data_risk[data_risk$date_month >= risk_start_date & data_risk$date_month <= risk_end_date, ]
ts_data_risk <- ts(subset_data_risk$X1, start = c(2018, 04), frequency = 12)
lm_model_risk = lm(ts_data_risk ~ time(ts_data_risk))
summary(lm_model_risk)


###################################### benefit
outliers = data_benefit %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_benefit$X1, mu = max(data_benefit$X1), alternative = "less")
print(t_test_result)

outliers = data_benefit %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

benefit_max_value = which.max(data_benefit$X1)
benefit_max_month = data_benefit$date_month[benefit_max_value]
benefit_max_month

benefit_min_value = which.min(data_benefit$X1)
benefit_min_month = data_benefit$date_month[benefit_min_value]
benefit_min_month

benefit_start_date = "2018-07"
benefit_end_date = "2023-06"

subset_data_benefit = data_benefit[data_benefit$date_month >= benefit_start_date & data_benefit$date_month <= benefit_end_date, ]
ts_data_benefit <- ts(subset_data_benefit$X1, start = c(2018, 07), frequency = 12)
lm_model_benefit = lm(ts_data_benefit ~ time(ts_data_benefit))
summary(lm_model_benefit)


###################################### hbmbarrier
outliers = data_hbmbarrier %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_hbmbarrier$X1, mu = max(data_hbmbarrier$X1), alternative = "less")
print(t_test_result)

outliers = data_hbmbarrier %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

hbmbarrier_max_value = which.max(data_hbmbarrier$X1)
hbmbarrier_max_month = data_hbmbarrier$date_month[hbmbarrier_max_value]
hbmbarrier_max_month

hbmbarrier_min_value = which.min(data_hbmbarrier$X1)
hbmbarrier_min_month = data_hbmbarrier$date_month[hbmbarrier_min_value]
hbmbarrier_min_month

hbmbarrier_start_date = "2020-02"
hbmbarrier_end_date = "2023-06"

subset_data_hbmbarrier = data_hbmbarrier[data_hbmbarrier$date_month >= hbmbarrier_start_date & data_hbmbarrier$date_month <= hbmbarrier_end_date, ]
ts_data_hbmbarrier <- ts(subset_data_hbmbarrier$X1, start = c(2020, 02), frequency = 12)
lm_model_hbmbarrier = lm(ts_data_hbmbarrier ~ time(ts_data_hbmbarrier))
summary(lm_model_hbmbarrier)


###################################### social
outliers = data_social %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_social$X1, mu = max(data_social$X1), alternative = "less")
print(t_test_result)

outliers = data_social %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

social_max_value = which.max(data_social$X1)
social_max_month = data_social$date_month[social_max_value]
social_max_month

social_min_value = which.min(data_social$X1)
social_min_month = data_social$date_month[social_min_value]
social_min_month

social_start_date = "2021-02"
social_end_date = "2023-06"

subset_data_social = data_social[data_social$date_month >= social_start_date & data_social$date_month <= social_end_date, ]
ts_data_social <- ts(subset_data_social$X1, start = c(2018, 01), frequency = 12)
lm_model_social = lm(ts_data_social ~ time(ts_data_social))
summary(lm_model_social)


###################################### misinformation
outliers = data_misinformation %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_misinformation$X1, mu = max(data_misinformation$X1), alternative = "less")
print(t_test_result)

outliers = data_misinformation %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

misinformation_max_value = which.max(data_misinformation$X1)
misinformation_max_month = data_misinformation$date_month[misinformation_max_value]
misinformation_max_month

misinformation_min_value = which.min(data_misinformation$X1)
misinformation_min_month = data_misinformation$date_month[misinformation_min_value]
misinformation_min_month

misinformation_start_date = "2020-04"
misinformation_end_date = "2023-06"

subset_data_misinformation = data_misinformation[data_misinformation$date_month >= misinformation_start_date & data_misinformation$date_month <= misinformation_end_date, ]
ts_data_misinformation <- ts(subset_data_misinformation$X1, frequency = 12)
lm_model_misinformation = lm(ts_data_misinformation ~ time(ts_data_misinformation))
summary(lm_model_misinformation)


###################################### extbarrier
outliers = data_extbarrier %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_extbarrier$X1, mu = max(data_extbarrier$X1), alternative = "less")
print(t_test_result)

outliers = data_extbarrier %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

extbarrier_max_value = which.max(data_extbarrier$X1)
extbarrier_max_month = data_extbarrier$date_month[extbarrier_max_value]
extbarrier_max_month

extbarrier_min_value = which.min(data_extbarrier$X1)
extbarrier_min_month = data_extbarrier$date_month[extbarrier_min_value]
extbarrier_min_month

extbarrier_start_date = "2018-01"
extbarrier_end_date = "2023-06"

subset_data_extbarrier = data_extbarrier[data_extbarrier$date_month >= extbarrier_start_date & data_extbarrier$date_month <= extbarrier_end_date, ]
ts_data_extbarrier <- ts(subset_data_extbarrier$X1, frequency = 12)
lm_model_extbarrier = lm(ts_data_extbarrier ~ time(ts_data_extbarrier))
summary(lm_model_extbarrier)


###################################### attitudes_positive
outliers = data_attitudes %>%
  filter(X1 > mean(X1) + 2 * sd(X1)) 
print(outliers)
t_test_result <- t.test(data_attitudes$X1, mu = max(data_attitudes$X1), alternative = "less")
print(t_test_result)

outliers = data_attitudes %>%
  filter(X1 < mean(X1) - 2 * sd(X1)) 
print(outliers)

positive_max_value = which.max(data_attitudes$X1)
positive_max_month = data_attitudes$date_day[positive_max_value]
positive_max_month

positive_min_value = which.min(data_attitudes$X1)
positive_min_month = data_attitudes$date_day[positive_min_value]
positive_min_month

positive_start_date = "2018-03"
positive_end_date = "2023-06"

subset_data_attitudes = data_attitudes[data_attitudes$date_day >= positive_start_date & data_attitudes$date_day <= positive_end_date, ]
ts_data_attitudes <- ts(subset_data_attitudes$X1, frequency = 12)
lm_model_positive = lm(ts_data_attitudes ~ time(ts_data_attitudes))
summary(lm_model_positive)


###################################### attitudes_negative
outliers = data_attitudes %>%
  filter(X2 > mean(X2) + 2 * sd(X2)) 
print(outliers)
t_test_result <- t.test(data_attitudes$X2, mu = max(data_attitudes$X2), alternative = "less")
print(t_test_result)

outliers = data_attitudes %>%
  filter(X2 < mean(X2) - 2 * sd(X2)) 
print(outliers)

negative_max_value = which.max(data_attitudes$X2)
negative_max_month = data_attitudes$date_day[negative_max_value]
negative_max_month

negative_min_value = which.min(data_attitudes$X2)
negative_min_month = data_attitudes$date_day[negative_min_value]
negative_min_month

negative_start_date = "2018-01"
negative_end_date = "2019-08"

subset_data_attitudes = data_attitudes[data_attitudes$date_day >= negative_start_date & data_attitudes$date_day <= negative_end_date, ]
ts_data_attitudes <- ts(subset_data_attitudes$X2, frequency = 12)
lm_model_negative = lm(ts_data_attitudes ~ time(ts_data_attitudes))
summary(lm_model_negative)


###################################### HBM_benefit x barrier
t_test_result = t.test(data_benefit$X1,data_hbmbarrier$X1, paired = TRUE)
print(t_test_result)


###################################### extbarrier x misinformation/social
t_test_result = t.test(data_extbarrier$X1,data_misinformation$X1, paired = TRUE)
print(t_test_result)

t_test_result = t.test(data_extbarrier$X1,data_social$X1, paired = TRUE)
print(t_test_result)


###################################### misinformation x social
start = "2020-03"
end = "2023-06"

subset_data_misinformation = data_misinformation[data_misinformation$date_month >= start & data_misinformation$date_month <= end, ]
subset_data_social = data_social[data_social$date_month >= start & data_social$date_month <= end, ]

t_test_result = t.test(subset_data_misinformation$X1,subset_data_social$X1, paired = TRUE)
print(t_test_result)


