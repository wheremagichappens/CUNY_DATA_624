require(tidyverse)
require(caret)
require(readxl)
require(XLConnect)
require(naniar)
require(caret)
require(mice)
require(VIM)
require(corrplot)
require(RColorBrewer)
require(gridExtra)
require(ggplot2)
require(readxl)
require(RANN)
require(MLmetrics)
require(dummies)
require(XLConnectJars)
require(openxlsx)
require(isa2)
setwd('/Users/bethany/Documents/Data-624/Project-2')
knn.tune = readRDS("knn.rds")
svm.tune = readRDS("svm.rds")
beverage_clean = readRDS('final_beverage.rds')
# Importing Data
beverage <- read_excel("/Users/bethany/Documents/Data-624/Project-2/raw.xlsx", sheet=1)
test<-read_excel("/Users/bethany/Documents/Data-624/Project-2/StudentEvaluation- TO PREDICT.xlsx")


#Creating Useful Names
colnames(beverage)<- make.names(colnames(beverage), unique=TRUE)
colnames(test)<-colnames(beverage)

#Estimating Missing Data
missing_values <- sapply(beverage, function(x) sum(is.na(x)))%>%knitr::kable( col.names = c( "Missing Values"), caption = 'Missing Values Student Data')
test_missing_values <-sapply(test, function(x) sum(is.na(x)))%>%knitr::kable( col.names = c( "Missing Values"), caption = 'Missing Values Test Data')

# Remove observations with no outcome
beverage <- beverage%>%drop_na(PH)
#training Data from Beverage with Missing PH removed
y_train <- beverage%>%select(PH)  



# Replace -9999 with NA
beverage_2<-beverage%>%
  mutate(Brand.Code = factor(beverage$Brand.Code))%>%
  drop_na(PH)


test<-test %>% 
  mutate(Brand.Code = factor(test$Brand.Code))




#Looking at Histograms by Brand
gather_if <- function(data, FUN, key = "key", value = "value", na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  data %>% {gather(., key = key, value = value , names(.)[sapply(., FUN = FUN)], na.rm = na.rm, convert = convert, factor_key = factor_key )}} 

hist_data<-beverage_2 %>%  
  mutate(PH=as.factor(PH))%>% 
  gather_if(is.numeric, 'key', 'value') %>% 
  filter(!is.na(value)) %>%  
  group_by(key)  


bev_hist<- ggplot(hist_data, aes(value)) + 
  geom_histogram(aes(fill=Brand.Code), color="#999999", alpha=.3)+
  labs(subtitle="Colored By Brand", x="", y="")+ 
  facet_wrap(~key, scales = 'free', nrow = 7)+ 
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        legend.position = "bottom", 
        legend.key.size = unit(.4, "cm"))+
  scale_fill_manual()

ph_brand <- beverage_2%>%select('PH', 'Brand.Code')%>%
  mutate(Brand.Code=as.factor(Brand.Code))%>% 
  gather_if(is.numeric, 'key', 'value') %>% 
  filter(!is.na(value)) %>%  
  group_by(key) 

y_hist<-ggplot(ph_brand, aes(value)) +
     facet_wrap(~ key, scales = "free") +
     geom_histogram(aes(fill = Brand.Code))+ 
     theme_bw()+
     theme(axis.text.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_blank(), 
          legend.position = "bottom", 
          legend.key.size = unit(.4, "cm"))+
  scale_fill_manual()
 
# Correlations
bev_cor<-cor(na.omit(beverage_2[2:ncol(beverage_2)]))
# correlations<-corrplot(bev_cor, 
#                        method = 'circle',
#                        type = 'lower',
#                        order ='FPC',
#                        col = brewer.pal(n = 8, name = "PuOr")
# )



#Estimating Near Zero Variance
nzv_bev <-nearZeroVar(beverage_2, saveMetrics = TRUE)%>%knitr::kable(caption = 'Near Zero Estimates Student Data') # Just HydPressure 1

#beverage$Brand.Code <- factor(beverage$Brand.Code)

# 
# #####################

#Missing Values Check
#sapply(beverage_x, function(x) sum(is.na(x)))

# Beverage TRaining Imputed using knn in Caret so applies to Test, converting missing Brand to Unknown
ivals <- preProcess(beverage_2, method = "knnImpute", k =10)
beverage_2 <- predict(ivals, newdata = beverage_2)

#Test Imputed
test_clean <-predict(ivals, test)

# Create Training Dummies
# b_dummies <-dummy(beverage_2$Brand.Code)
# beverage_clean<-cbind(b_dummies, beverage_2)%>%
#   select(-Brand.Code, -Hyd.Pressure1)

#Create Test Data Dummies
test_clean<-test_clean%>%
  mutate(`Brand.Code`=ifelse(is.na(`Brand.Code`),'Unknown', `Brand.Code`),
         `Brand.Code`= factor(`Brand.Code`))


t_dummies <-dummy(test_clean$Brand.Code)
test_clean<-cbind(t_dummies, test_clean)%>%
  select(-Brand.Code, -Hyd.Pressure1)






#Creating Training & Testing Data
x_train <-beverage_clean%>%select(-PH)
y_train <- beverage%>%select(PH)

#Creating Training & Testing Data

x_test <-test_clean%>%select(-PH)

# Setting up crossvalidation controls

# set.seed(58677)
# seeds <- vector(mode = "list", length = 61)
# for(i in 1:60) seeds[[i]] <- sample.int(1000, 10)
# seeds[[61]] <- sample.int(1000, 1)
# control <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 6,
#   seeds = seeds)




#Train and Tune the SVM
# set.seed(58677)
# svm.tune <- train(x=x_train,
#                   y= y_train$PH,
#                   method = "svmRadial",
#                   tuneLength = 10,
#                   preProc = c('center', 'scale', 'nzv', "BoxCox"),
#                   metric="MAPE",
#                   trControl=control)
# #See SVM Output
svm_results<-svm.tune$results%>%knitr::kable()

#SVM Training Predictions & Error
svm_train_preds <- predict(svm.tune, new_data = x_train)
svm_metrics <-postResample(svm_train_preds, obs = y_train$PH)%>%knitr::kable(col.names = c('Metric'), caption = "Training Performance Metrics for SVM")
svm_train_mape <- MAPE(svm_train_preds,  y_train$PH)

#SVM Training Predictions & Error
svm_test_preds <-predict(svm.tune, new_data =x_test)

#To Come
#varImp(svm.tune$finalModel)


#Traine & Tune KNN



# 
# set.seed(58677)
# knn.tune <- train(x = x_train,
#                      y = y_train$PH,
#                      method = "knn",
#                      preProc = c('center', 'scale', "nzv", "BoxCox"),
#                     tuneLength = 10,
#                   trControl = control)

# Evaluate Knn models
knn_results<-knn.tune$results%>%knitr::kable()




train_knn_pred <- predict(knn.tune, newdata = x_train) 
knn_metric <- postResample(pred = train_knn_pred, obs = y_train$PH)%>%knitr::kable(col.names = c('Metric'), caption = "Training Performance Metrics for knn")

knn_train_mape <- MAPE(train_knn_pred,  y_train$PH)
# Test Preds
knn_test_preds <-predict(knn.tune, new_data =x_test)



#TO Come on Single Models
#varImp(knn.tune)
# write.xlsx(svm_test_preds, 'group_2_predictions.xlsx', sheet='SVM')
# write.xlsx(knn_test_preds, 'group_2_predictions.xlsx', sheet='KNN')



# saveRDS(knn.tune, "knn.rds")
# saveRDS(svm.tune, "svm.rds")
# saveRDS(beverage_clean, 'final_beverage.rds')
# # knn.tune<- readRDS("knn.rds")
# # svm.tune < readRDS('svm.rds')
# # beverage_clean <- readRDS('final_beverage.rds')

#varImp(knn.tune$finalModel)

svm_best <-svm.tune$results[6, 3:5]
knn_best<-knn.tune$results[2,2:4]
best_models<- rbind(svm_best, knn_best)
best_models$MAPE <-rbind(svm_train_mape,knn_train_mape)


