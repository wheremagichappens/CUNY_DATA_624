#-----DEPENDENCIES-----#

# processing
library(readxl)
library(tidyverse)
library(janitor)

# timeseries
library(zoo)

# math packages
library(urca)
library(forecast)

#-----PRE-PROCESSING-----#

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
  zoo(seq(from = as.Date("2009-05-01"), to = as.Date("2010-05-14"), by = 1))

# create standard time series   
atm_ts <- atm %>%  
  # remove column & generate date in timeseries using zoo
  select(-DATE) %>% 
  # generate ts using zoo 
  ts(end=c(2010,4))

#-----ATM-1-----#

#subset data 
ATM1_zoo <- atm_zoo[,1]
ATM1_ts <- atm_ts[,1]

#differentiated
ATM1d <-  diff(ATM1_ts, lag=7)

#unit root test
ATM1_ur <-ur.kpss(ATM1_ts)
ATM1d_ur <-ur.kpss(ATM1d)

# ARIMA
ATM1_arima <- auto.arima(ATM1d, seasonal=F, stepwise=F, approximation=F)

# Forecast
ATM1_fc <- ATM1_arima %>% forecast(h=17) 

write.csv(ATM1_fc, file="forecasts/ATM1_Forecast.csv")

#-----ATM-2-----#

#subset data 
ATM2_zoo <- atm_zoo[,2]
ATM2_ts <- atm_ts[,2]

#differentiated
ATM2d <-  diff(ATM2_ts, lag=7)

#unit root test
ATM2_ur <-ur.kpss(ATM2_ts)
ATM2d_ur <-ur.kpss(ATM2d)

# ARIMA
ATM2_arima <- auto.arima(ATM2d, seasonal=F, stepwise=F, approximation=F)

# Forecast
ATM2_fc <- ATM2_arima %>% forecast(h=17) 

write.csv(ATM2_fc, file="forecasts/ATM2_Forecast.csv")

#-----ATM-4-----#

#subset data 
ATM4_zoo <- atm_zoo[,4]
ATM4_ts <- atm_ts[,4]

#differentiated
ATM4d <-  diff(ATM4_ts, lag=7)

#unit root test
ATM4_ur <-ur.kpss(ATM4_ts)
ATM4d_ur <-ur.kpss(ATM4d)

# ARIMA
ATM4_arima <- auto.arima(ATM4d, seasonal=F, stepwise=F, approximation=F)

# Forecast
ATM4_fc <- ATM4_arima %>% forecast(h=17) 

write.csv(ATM4_fc, file="forecasts/ATM4_Forecast.csv")

