
require(tidyverse)
require(readxl)
require(fpp2)
require(forecast)
require(lubridate)
require(psych)
require(xlsx)
options(scipen = 999)
file ='data/water-pipes.xlsx'
# 10-23-15 at 1:00AM is hour 7081 Pipe 1
# End Date Hour is hour 7319
#10-23-15 00:00 AM is hour 7080
# End Date Hour 12-3-2015 16:00:00 is 8080
waterflow_1 <- read_excel("data/Waterflow_Pipe1.xlsx")
waterflow_2 <- read_excel("data/Waterflow_Pipe2.xlsx")

#write.xlsx(waterflow_1, file =  file , sheetName ="Waterflow Pipe 1", col.names = TRUE, row.names = TRUE, append = FALSE)

#write.xlsx(waterflow_2, file=file, sheetName = "Waterflow Pipe 2", col.names = TRUE, row.names = TRUE, append = TRUE)
waterflow_1<-waterflow_1 %>% 
    mutate(DateTime = as.POSIXct(DateTime))%>%
    group_by(hour=floor_date(DateTime, "hour")) %>%
    summarize(WaterFlow=mean(WaterFlow))


waterflow_2<-waterflow_2 %>% 
    mutate(DateTime = as.POSIXct(DateTime))%>%
    group_by(hour=floor_date(DateTime, "hour")) %>%
    summarize(WaterFlow=mean(WaterFlow))

waterflow_all <-merge(waterflow_1, waterflow_2, by = 'hour', all = TRUE)%>%
    mutate(waterflow = rowSums(.[c("WaterFlow.y", "WaterFlow.x")], na.rm = TRUE))%>%
    select(hour, waterflow)


w1<-ts(waterflow_1$WaterFlow ,start=c(1,7081),frequency=24)
w2<-ts(waterflow_2$WaterFlow ,start=c(1,7081),frequency=24)
ws <- ts(waterflow_all$waterflow ,start=c(1,7081),frequency=24)
#shows calendar form of 24 hour periods for days between 10-23-2015 & 12-3-2015
#print(ts(waterflow_all$waterflow, frequency = 24, start = c(1, 7081)), calendar = T)


# ws%>%
#     autoplot()+
#     labs(title = 'Flow Rates Combined Pipes (pipe one stops on November 1, 2015)',
#          subtitle ='October 23, 2015 00:00 - December 3, 2015 00:00',
#          x = 'Day of Year')
# 
# ws%>%
#     decompose()%>%
#     autoplot()+
#     labs(x = 'Day of Year')



ws_decomp<- ws%>% 
    decompose()%>%
    autoplot()+
    labs(title = "Decomposition of Hourly Waterflow Data",
         subtitle = 'First Reading October 23, 2015',
         x = 'Day of Year')+
    theme_bw()



ws_diffs<- ws%>%
    ndiffs()
#differences: 1


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

# Diffed ACF

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

lambda <-  BoxCox.lambda(ws)
#Lambda = 0.9531552

aic<- auto.arima(ws, seasonal = TRUE, ic = 'aicc', lambda = lambda)



aic_plot <- auto.arima(ws, seasonal = TRUE, ic = 'aicc', lambda = lambda)%>%
    forecast(h=24*7)%>%
    autoplot() +
    labs(title = "AIC selected ARIMA(1,1,3)(0,0,1)[24] ", 
                 subtitle = 'October 23, 2015 - December 23, 2015',
                y="Flowrate", x="Days")+
    theme_bw()+ theme()

bic<-auto.arima(ws, seasonal = TRUE, ic = 'bic', lambda = lambda )

    
bic_plot<-auto.arima(ws, seasonal = TRUE, ic = 'bic', lambda = lambda )%>%
    forecast(h=24*7)%>%
    autoplot()+
    labs(title = "BIC selected ARIMA(2,1,1)  ", 
         subtitle = 'October 23, 2015 - December 23, 2015',
         y="Flowrate", x="Days")+
    theme_bw()+ theme()

final_ws <- Arima(ws, order=c(1,1,3), seasonal=c(0,0,1),lambda=lambda)

preds_ws <-as.data.frame(forecast(final_ws, h = 168))



waterflow_all <-waterflow_all%>%
    rename( DateTime = hour,
            WaterFlow = waterflow)
    
preds_ws<-preds_ws%>%
    mutate(DateTime = seq(from=as.POSIXct("2015-12-3 17:00", tz="UTC"),
                          to=as.POSIXct("2015-12-10 16:00", tz="UTC"), 
                          by="hour") )%>%
    select(DateTime, `Point Forecast`, `Lo 80`,`Hi 80`, `Lo 95`, `Hi 95`)


#write.xlsx(waterflow_all, file = file, sheetName = "Combined Waterflow", col.names = TRUE, row.names = FALSE, append = TRUE)
#write.xlsx(preds_ws, file =  file , sheetName = "Forecasts", col.names = TRUE, row.names = FALSE, append = TRUE)


