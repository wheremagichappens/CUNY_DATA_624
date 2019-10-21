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

atm$ATM2[is.na(atm$ATM2)] <- mean(atm$ATM2, na.rm = TRUE) ## remove NA
atm$ATM4[which.max(atm$ATM4)] <- mean(atm$ATM4, na.rm = TRUE) ## remove outlier

# create TS with weekly frequency & subset data
atm_ts <- atm %>% select(-DATE) %>% ts(start=1,  frequency = 7)
ATM1_ts <- atm_ts[,1]; ATM2_ts <- atm_ts[,2]; ATM3_ts <- atm_ts[,3]; ATM4_ts <- atm_ts[,4]

#unit root test
## no diff
ATM1_ur <-ur.kpss(ATM1_ts)
ATM2_ur <-ur.kpss(ATM2_ts)
ATM4_ur <-ur.kpss(ATM4_ts)
## first order diff
ATM1d_ur <-ur.kpss(diff(ATM1_ts, lag=7))
ATM2d_ur <-ur.kpss(diff(ATM2_ts, lag=7))
ATM4d_ur <-ur.kpss(diff(ATM4_ts, lag=7))

# AUTO.ARIMA function; set D=1 for seasonal differencing
ATM1_AA <-auto.arima(ATM1_ts, D = 1, lambda = "auto", approximation = F, stepwise = T)
ATM2_AA <-auto.arima(ATM2_ts, D = 1, lambda = "auto", approximation = F, stepwise = T)
ATM4_AA <-auto.arima(ATM4_ts, D = 1, lambda = "auto", approximation = F, stepwise = T)

# Forecast Results
ATM1_fc <- forecast(ATM1_AA,h=31)
ATM2_fc <- forecast(ATM2_AA,h=31)
ATM3_fc <- meanf(ATM3_ts[ATM3_ts > 0], h=31)# based on three non-zero values (between observations 363 and 365)
ATM4_fc <- forecast(ATM4_AA,h=31)

# Prepare dataframe for ATM3 mean forcast plotting 
ATM3_plotdata_fc <- cbind(seq(from = 366, to = 396),
                          ATM3_fc[[5]], 
                          ATM3_fc[[6]], 
                          ATM3_fc[[7]]) %>% 
  as.data.frame()
colnames(ATM3_plotdata_fc) <- c('Date', 'Point Forecast', 'Lo 80', 'Lo 95', 'Hi 80', 'Hi 95')
ATM3_plotdata <- ATM3_ts %>% 
  fortify() %>% 
  select(-Index) %>%
  rename(Cash = Data) %>% 
  mutate(Date = as.numeric(row.names(.))) %>% 
  select(Date, Cash) %>% 
  full_join(ATM3_plotdata_fc, by = 'Date')

# Revert results back into original form
# date <- as.character(seq(as.Date('2010-05-01'), length.out=31, by=1))
# ATM_FC <-  cbind("Date"=date, "ATM1"=ATM1_fc$mean, "ATM2"=ATM2_fc$mean,
# "ATM3"=c(NA,NA,NA,NA),"ATM4"=ATM4_fc$mean) %>% as.data.frame()

# Combine the forecasts for the different ATMS
ATM_ALL_FC <- bind_cols(as.data.frame(ATM1_AA[4:6]),
                        as.data.frame(ATM2_AA[4:6]),
                        as.data.frame(ATM3_AA[5]),
                        as.data.frame(ATM4_AA[4:6]),) %>% 
  rename(ATM1_mean = 'mean',
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

# Save output
write.csv(ATM_ALL_FC, file="forecasts/ATM_FC.csv")