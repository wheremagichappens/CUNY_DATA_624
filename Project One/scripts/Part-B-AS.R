# Part B: Forecasting Power {-#part-b}

# processing
library(readxl)
#install.packages('tinytex')
library(tinytex)
#library(l3backend)
#install.packages('MiKTeX')
# graphs
library(ggplot2)

# formatting
library(default)
library(knitr)
library(kableExtra)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = T, message=F, warning=F, error=F, comment=F, fig.width=10, fig.height = 3)

# Set default augments for `kable_styling()` 
default(kable) <- list("latex")
default(kable_styling)  <- list(latex_options = c("hold_position", "striped"))

# Set default for ggplot theme
default(theme) <- list(axis.text.x = element_text(angle = 90, hjust = 1),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(color="#B85231", size=10, face="bold"),
                       plot.subtitle = (element_text(size=8, color="#000000")),
                       legend.title = (element_text(size=10, color="#000000", face="bold")),
                       strip.background = element_rect(color="#000000", 
                                                       fill="#F5E8E4", size=1, linetype="solid"),
                       strip.text.x = element_text(size = 8, color = "#000000", face="bold"))

# GGplot Palette
default(scale_color_brewer) <- list(palette = "OrRd")

## Data Exploration and Processing {-#b-exploration}
library(tidyverse)
library(scales)
library(readxl)
library(forecast)
library(lubridate)
library(fpp2)
library(ggplot2)
library(forecast)
library(tseries)
library(imputeTS)
library(tsoutliers)
#install.packages('tsoutliers')

#power_data <- read_excel("data/ResidentialCustomerForecastLoad-624.xlsx") 
library (readr)

power="https://raw.githubusercontent.com/vindication09/DATA-624/master/ResidentialCustomerForecastLoad-624.csv"

partb_data<-read_csv(url(power))

head(partb_data)

ts_data <- ts(partb_data$KWH, frequency = 12, start = c(1998,1))
ts_data

ts_data<-na.interpolation(ts_data)

cycle(ts_data)

summary(ts_data)
hist(ts_data)

#disable scientific notation (ONLY RUN ONCE)
options(scipen = 99999)

autoplot(ts_data) +
labs(title = "Monthly Residential Power Usage", subtitle = "01/98 - 12/13")+
theme_classic();

ggseasonplot(ts_data);

boxplot(ts_data~cycle(ts_data),xlab="Month", ylab = "Monthly Residential Power Usage");

ggsubseriesplot(ts_data);

stl(ts_data, s.window = 'periodic') %>% autoplot();

ggAcf(ts_data);
#Box.test(ts_data, type = c("Ljung-Box"))


# handling outlier
#fit <- nnetar(tsclean(ts_data))
outlier_func <- tsoutliers(ts_data, iterate = 2, lambda = "auto")
ts_data[outlier_func$index] <- outlier_func$replacements

## Data Model {-#b-model}

### Model #1: ARIMA
arima_model <- auto.arima(ts_data)

arima_model <- forecast(arima_model, h=12)

autoplot(arima_model) + autolayer(fitted(arima_model))

checkresiduals(arima_model)

### Model #2: STL (no-demped) - MNN
#stlf - etsmodel estimation --- M,N,N is chosen.
stl_ndemp <- stlf(ts_data, s.window = "periodic", robust=TRUE, h = 12)

# forecast plot
autoplot(stl_ndemp) + autolayer(fitted(stl_ndemp))

checkresiduals(stl_ndemp)

### Model #2-2: STL (demped) - MAdN
#stlf - etsmodel estimation --- M, Ad, N is chosen.
stl_demp <- stlf(ts_data, damped=TRUE, s.window = "periodic", robust=TRUE, h = 12)

# forecast plot
autoplot(stl_demp) + autolayer(fitted(stl_demp))

checkresiduals(stl_demp)

### Model #3: ets - MNM
# ETS models - MNM
ets_model <- ets(ts_data)

# forecast plot
autoplot(forecast(ets_model, h=12)) + autolayer(fitted(ets_model))

checkresiduals(ets_model)

### Model #4: Regression
fit.reg <- tslm(ts_data ~ trend + season)
reg_model <- forecast(fit.reg, h = 12)

# forecast plot
autoplot(reg_model) + autolayer(fitted(reg_model))

checkresiduals(reg_model)

### Model #5: TBATS
fit.tbats <- tbats(ts_data)
tbats_model <- forecast(fit.tbats, h = 12)

# forecast plot
autoplot(tbats_model) + autolayer(fitted(tbats_model))

checkresiduals(tbats_model)

### Accuracy of Models
accuracy(arima_model);

accuracy(stl_ndemp);

accuracy(stl_demp);

accuracy(ets_model);

accuracy(reg_model);

accuracy(tbats_model)

## Forecast {-#b-forecast}
### Model #1: ARIMA
arima_cv <- function(x, h){forecast(Arima(x, order = c(3, 0, 2), seasonal = c(2, 1, 0), include.drift = TRUE), h=h)}

e <- tsCV(ts_data, arima_cv, h=12)

sqrt(mean(e^2, na.rm=TRUE))