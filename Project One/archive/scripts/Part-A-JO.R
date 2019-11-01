# Dependencies
## processing
library(readxl)
library(tidyverse)
library(janitor)

## forecasting packages
library(urca)
library(forecast)
library(fpp2)
library(zoo)


# Load data
atm_data <- read_excel("data/ATM624Data.xlsx") 

# Purge observations for which ATM and cash are NA
atm_data <- atm_data %>% 
  na.omit()

# Transform data frame to time series
atm_ts <- atm_data %>% 
  spread(key = ATM, value = Cash) %>% 
  read.zoo(FUN = as.POSIXct) %>% 
  ts()

# Split the time series by ATM [COULD GENERATE MORE SUCCINCT CODE TO FUNCTIONALIZE THIS]

ATM1 <- atm_data %>%
  dplyr::filter(ATM == 'ATM1') %>% 
  select(DATE, Cash) %>% 
  read.zoo(FUN = as.POSIXct) %>% 
  ts()

ATM2 <- atm_data %>%
  dplyr::filter(ATM == 'ATM2') %>% 
  select(DATE, Cash) %>% 
  read.zoo(FUN = as.POSIXct) %>% 
  ts()

ATM3 <- atm_data %>%
  dplyr::filter(ATM == 'ATM3') %>% 
  select(DATE, Cash) %>% 
  read.zoo(FUN = as.POSIXct) %>% 
  ts()

ATM4 <- atm_data %>%
  dplyr::filter(ATM == 'ATM4') %>% 
  select(DATE, Cash) %>% 
  read.zoo(FUN = as.POSIXct) %>% 
  ts()

# Generate ATM1 ETS model with autoselection
ATM1_ets <- ATM1 %>% 
  ets()

# Calculate ATM1 ETS model forecast
ATM1_ets_fc <- ATM1_ets %>% 
  forecast(h = 31)

# Generate ATM1 ARIMA model with autoselection, single-differenced with no shortcuts to consider all possible models
ATM1_arima <- ATM1 %>% 
  auto.arima(D = 1,
             stepwise = FALSE, 
             approximation = FALSE)

# Calculate ATM1 ARIMA model forecast
ATM1_arima_fc <- ATM1_arima %>% 
  forecast(h = 31)

# Generate ATM2 ETS model with autoselection
ATM2_ets <- ATM2 %>% 
  ets()

# Calculate ATM2 ETS model forecast
ATM2_ets_fc <- ATM2_ets %>% 
  forecast(h = 31)

# Generate ATM2 ARIMA model with autoselection, single-differenced with no shortcuts to consider all possible models
ATM2_arima <- ATM2 %>% 
  auto.arima(D = 1,
             stepwise = FALSE, 
             approximation = FALSE)

# Calculate ATM2 ARIMA model forecast
ATM2_arima_fc <- ATM2_arima %>% 
  forecast(h = 31)

# Generate ATM3 mean model
ATM3_mean <- ATM3 %>% 
  window(start = 363)

# Calculate ATM3 mean model forecast
ATM3_mean_fc <- ATM3_mean %>%
  meanf(ATM3, h = 31) 

# Generate ATM4 ETS model with autoselection
ATM4_ets <- ATM4 %>% 
  ets()

# Generate ATM4 ETS model forecast
ATM4_ets_fc <- ATM4_ets %>% 
  forecast(h = 31)

# Generate ATM4 ARIMA model with autoselection, single-differenced with no shortcuts to consider all possible models
ATM4_arima <- ATM4 %>% 
  auto.arima(D = 1,
             stepwise = FALSE, 
             approximation = FALSE)

# Generate ATM4 ARIMA model forecast
ATM4_arima_fc <- ATM4_arima %>% 
  forecast(h = 31)


# Combine the forecasts for the different ATMS
atm_all_fc <- bind_cols(as.data.frame(seq(from = 366,
                                          to = 396, 
                                          by = 1)),
                        as.data.frame(ATM1_arima_fc[4:6]),
                        as.data.frame(ATM2_arima_fc[4:6]),
                        as.data.frame(ATM3_mean_fc[5]),
                        as.data.frame(ATM4_arima_fc[4:6]),) %>% 
  
  rename(Day = 'seq(from = 366, to = 396, by = 1)',
         ATM1_mean = 'mean',
         ATM1_low80CI = 'lower.80.',
         ATM1_low95CI = 'lower.95.',
         ATM1_upper80CI = 'upper.80.',
         ATM1_upper95CI = 'upper.95.',
         ATM2_mean = 'mean1',
         ATM2_low80CI = 'lower.80.1',
         ATM2_low95CI = 'lower.95.1',
         ATM2_upper80CI = 'upper.80.1',
         ATM2_upper95CI = 'upper.95.1',
         ATM3_mean = 'mean2',
         ATM4_mean = 'mean3',
         ATM4_low80CI = 'lower.80.2',
         ATM4_low95CI = 'lower.95.2',
         ATM4_upper80CI = 'upper.80.2',
         ATM4_upper95CI = 'upper.95.2'
  )

write_csv(atm_all_fc, path = "forecasts/ATM_all_forecast.csv")