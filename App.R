library(tidyverse)
library(lubridate)
library(janitor)

# Load data
df <- read_csv("/Users/roli/Downloads/SDGE-ELEC-2025-Q3.csv")


# Clean column names (good habit)
df <- clean_names(df)

# View structure
str(df)

# Preview first rows
head(df)

# Dataset dimensions (rows/columns)
dim(df)

# Summary stats
summary(df)

# Missing data overview
colSums(is.na(df))

# Count unique values (e.g., zones/regions/customers)
df %>% summarise(across(everything(), n_distinct))

install.packages("forecast")   # only once
library(forecast)              # every session

# Fit ARIMA model
fit_arima <- auto.arima(df$AveragekWh)

# Print full model info
summary(fit_arima)

# Coefficients
fit_arima$coef

# Residuals
resid_arima <- residuals(fit_arima)
head(resid_arima)

# AIC / BIC
fit_arima$aic
fit_arima$bic

# Model accuracy (R^2, RMSE, MAE, MAPE, etc.)
accuracy(fit_arima)

fit_ets <- ets(df$AveragekWh)

summary(fit_ets)     # coefficients + error measures

coef(fit_ets)        # smoothing parameters
resid_ets <- residuals(fit_ets)
accuracy(fit_ets)    # RMSE, MAE, MAPE etc.

y <- ts(df$AveragekWh, frequency = 12)

fit_ets <- ets(y)
fit_arima <- auto.arima(y)

summary(fit_ets)
summary(fit_arima)
accuracy(fit_ets)
accuracy(fit_arima)
checkresiduals(fit_ets)
checkresiduals(fit_arima)
