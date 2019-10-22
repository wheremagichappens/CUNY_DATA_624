library(tidyverse)
library(readxl)
library(fpp2)
library(forecast)
library(lubridate)
library(psych)
#library(xlsx)
options(scipen = 999)

# Reading Data
waterflow_1 <- read_excel("data/Waterflow_Pipe1.xlsx")
waterflow_2 <- read_excel("data/Waterflow_Pipe2.xlsx")

# Writing original data to submission file
#file ='forecasts/Group2_Project1_Fall2019.xlsx'
#write.xlsx(waterflow_1, file = file , sheetName ="Waterflow Pipe 1", col.names = TRUE, row.names = TRUE, append = FALSE)
#write.xlsx(waterflow_2, file=file, sheetName = "Waterflow Pipe 2", col.names = TRUE, row.names = TRUE, append = TRUE)

# Grooming, aligning dates and aggregating Data
waterflow_1<-waterflow_1 %>% 
    mutate(DateTime = as.POSIXct(DateTime))%>%
    group_by(hour=floor_date(DateTime, "hour")) %>%
    summarize(WaterFlow=mean(WaterFlow))


waterflow_2<-waterflow_2 %>% 
    mutate(DateTime = as.POSIXct(DateTime))%>%
    group_by(hour=floor_date(DateTime, "hour")) %>%
    summarize(WaterFlow=mean(WaterFlow))

# Creating a combined data set
waterflow_all <-merge(waterflow_1, waterflow_2, by = 'hour', all = TRUE)%>%
    mutate(waterflow = rowSums(.[c("WaterFlow.y", "WaterFlow.x")], na.rm = TRUE))%>%
    select(hour, waterflow)

# Converting all Three Data Sets to Time Series
w1<-ts(waterflow_1$WaterFlow ,start=c(1,7081),frequency=24)
w2<-ts(waterflow_2$WaterFlow ,start=c(1,7081),frequency=24)
ws <- ts(waterflow_all$waterflow ,start=c(1,7081),frequency=24)


#Decomposition of Time Series
ws_decomp<- ws%>% 
    decompose()%>%
    autoplot()+
    labs(title = "Decomposition of Hourly Waterflow Data",
         subtitle = 'First Reading October 23, 2015',
         x = 'Day of Year')+
    theme_bw()


# Checking Differences
ws_diffs<- ws%>%
    ndiffs() #1


# Testing Stationarity
dickie<-tseries::adf.test(ws)

# ACF & PACF

ws_acf <- ggAcf(ws, color = 'darkorange4')+
    labs(title = "ACF Combined Pipe Flow Rates", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Auto Correlation", x="Hours")+
    theme_bw()+ theme()

ws_pacf <- ggPacf(ws, color = 'darkorange4')+
    labs(title = "PACF Combined Pipe Flow Rates", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Partial Auto Correlation", x="Hours")+
    theme_bw()+ theme()

# Differencesd ACF & PACF

ws_acf_diff <-ggAcf(diff(ws,lag = 1), color = 'darkorange4')+
    labs(title = "ACF Combined Pipe Flow Rates", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Auto Correlation", x="Hours")+
    theme_bw()+ theme()

ws_pacf_diff <-ggPacf(diff(ws,lag = 1), color = 'darkorange4')+
    labs(title = "PACF Combined Pipe Flow Rates", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Auto Correlation", x="Hours")+
    theme_bw()+ theme()

#Establishing a lambda value for ARIMA transformations
lambda <-  BoxCox.lambda(ws)
#Lambda = 0.9531552


# Auto arima's including season components for AICc and BIC
aic<- auto.arima(ws, seasonal = TRUE, ic = 'aicc', lambda = lambda)

bic<-auto.arima(ws, seasonal = TRUE, ic = 'bic', lambda = lambda )


# Plots of auto.arimas
aic_plot <- auto.arima(ws, seasonal = TRUE, ic = 'aicc', lambda = lambda)%>%
    forecast(h=24*7)%>%
    autoplot() +
    labs(title = "AIC selected ARIMA(1,1,3)(0,0,1)[24] ", 
                 subtitle = 'October 23, 2015 - December 23, 2015',
                y="Flowrate", x="Days")+
    theme_bw()+ theme()

    
bic_plot<-auto.arima(ws, seasonal = TRUE, ic = 'bic', lambda = lambda )%>%
    forecast(h=24*7)%>%
    autoplot()+
    labs(title = "BIC selected ARIMA(2,1,1)  ", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Flowrate", x="Days")+
    theme_bw()+ theme()


# Final AIC from AICc and predictions
final_ws <- Arima(ws, order=c(1,1,3), seasonal=c(0,0,1),lambda=lambda)

preds_ws <-as.data.frame(forecast(final_ws, h = 168))


#Renaming fields for output data
waterflow_all <-waterflow_all%>%
    rename( DateTime = hour,
            WaterFlow = waterflow)
# Formatting forecasts for output data    
preds_ws<-preds_ws%>%
    mutate(DateTime = seq(from=as.POSIXct("2015-12-3 17:00", tz="UTC"),
                          to=as.POSIXct("2015-12-10 16:00", tz="UTC"), 
                          by="hour") )%>%
    select(DateTime, `Point Forecast`, `Lo 80`,`Hi 80`, `Lo 95`, `Hi 95`)

# Writing forecasts and final data to the 'XLSX' file
#write.xlsx(waterflow_all, file = file, sheetName = "Combined Waterflow", col.names = TRUE, row.names = FALSE, append = TRUE)
#write.xlsx(preds_ws, file =  file , sheetName = "Water Flow_Forecasts", col.names = TRUE, row.names = FALSE, append = TRUE)


