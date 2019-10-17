# Dependencies
## processing
library(readxl)
library(tidyverse)
library(janitor)

## timeseries
library(zoo)

## math packages
library(urca)
library(forecast)

# load data
atm_data <- read_excel("data/ATM624Data.xlsx") 

# clean dataframe
atm <- atm_data %>% 
  # create wide dataframe
  spread(ATM, Cash) %>% 
  # remove NA column using function from janitor package
  remove_empty(which = "cols") %>%
  # filter unobserved values from May 2010
  filter(DATE < as.Date("2010-05-01")) %>%
  # ensure dates are ascending
  arrange(DATE) 

## remove NA
atm$ATM2[is.na(atm$ATM2)] <- mean(atm$ATM2, na.rm = TRUE)

## remove outlier
atm$ATM4[which.max(atm$ATM4)] <- mean(atm$ATM4, na.rm = TRUE)

# create zoo time series   
atm_zoo <- atm %>%  
  # remove column & generate date in timeseries using zoo
  select(-DATE) %>% 
  # generate ts using zoo 
  zoo(seq(from = as.Date("2009-05-01"), to = as.Date("2010-05-01"), by = 1))

# create standard time series   
atm_ts <- atm %>%  
  # remove column & generate date in timeseries using zoo
  select(-DATE) %>% 
  # generate ts using zoo 
  ts(start=1,  frequency = 7)

#subset data 
ATM1_zoo <- atm_zoo[,1]; ATM1_ts <- atm_ts[,1]
ATM4_zoo <- atm_zoo[,4]; ATM4_ts <- atm_ts[,4]
ATM2_zoo <- atm_zoo[,2]; ATM2_ts <- atm_ts[,2]

#unit root test
## no diff
ATM1_ur <-ur.kpss(ATM1_ts)
ATM2_ur <-ur.kpss(ATM2_ts)
ATM4_ur <-ur.kpss(ATM4_ts)
## first order diff
ATM1d_ur <-ur.kpss(diff(ATM1_ts, lag=7))
ATM2d_ur <-ur.kpss(diff(ATM2_ts, lag=7))
ATM4d_ur <-ur.kpss(diff(ATM4_ts, lag=7))
## seasonal diff
ATM1sd_ur <-ur.kpss(diff(log(ATM1_ts), lag=7))
ATM2sd_ur <-ur.kpss(diff(log(ATM2_ts), lag=7))
ATM4sd_ur <-ur.kpss(diff(log(ATM4_ts), lag=7))
## seasonal diff-diff
ATM1sdd_ur <-ur.kpss(diff(diff(log(ATM1_ts)), lag=7))
ATM2sdd_ur <-ur.kpss(diff(diff(log(ATM2_ts)), lag=7))
ATM4sdd_ur <-ur.kpss(diff(diff(log(ATM4_ts)), lag=7))

# Modeling 
## Lambda for Box-cox transformation
ATM1_lambda <- BoxCox.lambda(ATM1_ts)
ATM2_lambda <- BoxCox.lambda(ATM2_ts)
ATM4_lambda <- BoxCox.lambda(ATM4_ts)

## ARIMA
ATM1_arima<-Arima(ATM1_ts, order = c(1, 0, 1), seasonal=list(order=c(0, 2, 1),period = 7), lambda=ATM1_lambda,  method="ML")
ATM2_arima<-Arima(ATM2_ts, order = c(2, 1, 2), seasonal=list(order=c(2, 1, 2),period = 7), lambda=ATM2_lambda,  method="ML")
ATM4_arima<-Arima(ATM4_ts, order = c(1, 0, 1), seasonal=list(order=c(1, 1, 1),period = 7), lambda=ATM1_lambda, method="ML")

# Forecast
ATM1_fc <- forecast(ATM1_arima,h=4)
ATM2_fc <- forecast(ATM2_arima,h=4)
ATM4_fc <- forecast(ATM4_arima,h=4)

# Save output
write.csv(ATM1_fc, file="forecasts/ATM1_Forecast.csv")
write.csv(ATM2_fc, file="forecasts/ATM2_Forecast.csv")
write.csv(ATM4_fc, file="forecasts/ATM4_Forecast.csv")

