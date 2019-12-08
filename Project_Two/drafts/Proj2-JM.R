library(tidyverse)
library(readxl)
library(psych)
library(ggplot2)
library(mice)
library(xtable)
library(GGally)
library(ggstance)
library(grid)
library(gridExtra)
library(caret)
library(data.table)
library(recipes)
library(Metrics)

# SEEDING
set.seed(58677)

# CUSTOM FUNCTIONS
gather_if <- function(data, FUN, key = "key", value = "value", na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  data %>% {gather(., key = key, value = value , names(.)[sapply(., FUN = FUN)], na.rm = na.rm, convert = convert, factor_key = factor_key )}} 

flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.table(row = rownames(cormat)[row(cormat)[ut]], column = rownames(cormat)[col(cormat)[ut]], cor  = (cormat)[ut])}

# DATA EXPLORATION
## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx')

## Data Tidying
names(StudentData) <- gsub(" ", "", names(StudentData))
StudentData <- StudentData %>% mutate(BrandCode=as.factor(BrandCode))

## Summary Stats
summary_stats <- describe(StudentData)

## Missing Data Analysis
MissingData <- StudentData %>% summarise_all(funs(sum(is.na(.)))) %>%  t() %>% as.data.frame() %>% rename("n"=V1) %>% rownames_to_column("predictor") %>% arrange(desc(n)) %>% mutate(`%`=round((n/nrow(StudentData)*100),2)) 

## Outiers Analysis
outliers <-StudentData %>%  mutate(PH=as.factor(PH))%>% gather_if(is.numeric, 'key', 'value') %>% filter(!is.na(value)) %>%  group_by(key)  %>%  mutate(outlier_lower = quantile(value, probs=.25, na.rm = T)-1.5 * IQR(value, na.rm = T), outlier_upper = 1.5 * IQR(value, na.rm = T)+quantile(value, probs = .75, na.rm = T), outlier=ifelse(value<outlier_lower, "TRUE", ifelse(value>outlier_upper, "TRUE", "FALSE"))) 
outlier_with <- outliers %>% filter(any(outlier=="TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_wo <- outliers %>% filter(all(outlier!="TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_freq <- outliers %>% select(key, outlier) %>% table() %>% as.data.frame.array() %>% rownames_to_column("variable") %>% arrange(desc(`TRUE`)) %>% mutate(`%`=round(`TRUE`/(`FALSE`+`TRUE`)*100,2)) %>% top_n(5,`%`)

## Correlation
cor <- StudentData %>% select(-PH, -BrandCode) %>% cor(use = "pairwise.complete.obs")
cor_freq <- findCorrelation(cor, .7, names = T)
cor_flat <- flattenCorrMatrix(cor) %>% arrange(row, desc(cor)) %>% filter(cor >.75 | cor < -0.75) 
cor_flat_left <- cor_flat %>% slice(1:10) %>% mutate(id = row_number())
cor_flat_right <- cor_flat %>% slice(11:20) %>% mutate(id = row_number())

# DATA PREPARATION

## Imputation
init <- mice(StudentData, maxit=0); meth <- init$method; predM <- init$predictorMatrix; meth[c("BrandCode")]="polyreg"
imputed <- mice(StudentData, method=meth, predictorMatrix=predM, m=5, printFlag = F)
BevData <- complete(imputed)

## Train/Test Splits 
trainingRows <- createDataPartition(BevData$PH, p = .80, list= FALSE)

## Split Train/Test Data 
train <- BevData[trainingRows, ]; test <- BevData[-trainingRows, ] 

# MODELING 
tl <- 5; trC <- trainControl(method = "cv", number = 10, savePredictions = T)

## SVM (RBF)
#svm_grid <-  expand.grid(sigma = c(0.02, 0.04, 0.06, 0.08, 0.1), C = c(.75, 1, 1.25, 1.5, 1.75, 2))
#svm_fit1 <- train(PH~., data=train, method = "svmRadial", metric="RMSE", tuneGrid = svm_grid, trControl=trC, tuneLength=tl)
#svm_fit2 <- train(PH~., data=train, method = "svmRadial", preProcess = c("center","scale"), metric="RMSE", tuneGrid = svm_grid, trControl=trC, tuneLength=tl)
#svm_fit3 <- train(PH~., data=train, method = "svmRadial", preProcess = c("nzv","zv", "BoxCox", "center","scale"), metric="RMSE", tuneGrid = svm_grid, trControl=trC, tuneLength=tl)

#svm_test_pred1 <- predict(svm_fit1, newdata = test); 
#svm_test_pred2 <- predict(svm_fit2, newdata = test);
#svm_test_pred3 <- predict(svm_fit3, newdata = test) 

##  MARS
mars_grid <- expand.grid(degree=1:3, nprune = seq(5, 50, by = 10))
mars_fit1 <- train(PH~., data=train, method = 'earth', tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
mars_fit2 <- train(PH~., data=train, method = 'earth', preProcess = c("center","scale"), tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
mars_fit3 <- train(PH~., data=train, method = 'earth', preProcess = c("nzv","zv", "BoxCox", "center","scale"), tuneGrid = mars_grid, trControl = trC, tuneLength=tl)

mars_test_pred1 <- predict(mars_fit1, newdata = test); 
mars_test_pred2 <- predict(mars_fit2, newdata = test);
mars_test_pred3 <- predict(mars_fit3, newdata = test) 

## eNET
enet_grid <- expand.grid(lambda=c(0,0.05,.1), fraction=seq(0.05,1, length=20))
enet_fit1 <- train(PH~., data=train, method = "enet", metric="RMSE", tuneGrid = enet_grid, tuneLength = tl, trControl = trC)
enet_fit2 <- train(PH~., data=train, method = "enet", preProcess = c("center", "scale"), metric="RMSE", tuneGrid = enet_grid, tuneLength = tl, trControl = trC)
enet_fit3 <- train(PH~., data=train, method = "enet", preProcess = c("nzv","zv", "BoxCox", "center","scale"), metric="RMSE", tuneGrid = enet_grid, trControl=trC, tuneLength=tl)

enet_test_pred1 <- predict(enet_fit1, newdata = test); 
enet_test_pred2 <- predict(enet_fit2, newdata = test);
enet_test_pred3 <- predict(enet_fit3, newdata = test)

## GBM
#gbmGrid <- expand.grid(n.trees=1000, interaction.depth=c(2,4,6), shrinkage=c(.01, .05), n.minobsinnode=seq(2,10, by=2))
#gbm_fit1 <- train(PH~., data=train, method = 'gbm',  trControl = trC, tuneGrid=gbmGrid, tuneLength = tl)
#gbm_fit2 <- train(PH~., data=train, method = 'gbm',  preProcess = c("nzv","zv","BoxCox"), trControl = trC, tuneGrid=gbmGrid, tuneLength = tl)
#gbm_test_pred1 <- predict(gbm_fit1, newdata = test); gbm_test_pred2 <- predict(gbm_fit2, newdata = test) 


# ACCURACY 
MARS_MAPE_TRN <- Metrics::mape(mars_fit1$pred$obs, mars_fit1$pred$pred)
eNET_MAPE_TRN <- Metrics::mape(enet_fit3$pred$obs, enet_fit3$pred$pred)
MARS_MAPE_TST <- Metrics::mape(test$PH, mars_test_pred1)
eNET_MAPE_TST <- Metrics::mape(test$PH, enet_test_pred3)
MARS_PERF_TRN <- mars_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="MARS_Train") %>% column_to_rownames("Variable") %>% t(); 
eNET_PERF_TRN <- enet_fit3$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="eNET_Train") %>% column_to_rownames("Variable") %>% t(); 
MARS_PERF_TST <- postResample(pred = mars_test_pred1, obs = test$PH);
eNET_PERF_TST <- postResample(pred = enet_test_pred3, obs = test$PH)
bind1 <- cbind(MARS_PERF_TRN, "MARS_Test"=MARS_PERF_TST, eNET_PERF_TRN, "eNET_Test"=eNET_PERF_TST)
bind2 <- cbind(MARS_MAPE_TRN, MARS_MAPE_TST, eNET_MAPE_TRN, eNET_MAPE_TST); row.names(bind2) <- 'MAPE'
Tbl_Accuracy <- rbind(bind1, bind2)

# VARIABLE IMPORTANCE 
#SVM_VarImp <- varImp(svm_fit2, scale = T)
MARS_VarImp <- varImp(mars_fit1, scale = T)
eNET_VarImp <- varImp(enet_fit3, scale = T)
#GBM_VarImp <- varImp(gbm_fit1, scale = T)


# UNUSED TABLES 
# Tbl_Corr <- full_join(cor_flat_left, cor_flat_right, by="id") %>% rename(`|`=id, "V1"=row.x, "V1  "="row.y", "V2"=column.x, "V2 "=column.y, "COR"=cor.x, "COR "=cor.y) %>% mutate(`|`="|")
# UNUSED PLOTS
# Plot_MissingData <- MissingData %>% filter(`%` > 1) %>% ggplot(aes(x=reorder(predictor, `%`), y=`%`)) + geom_point(alpha = 0.95, shape=22, color="#008ECC", fill="#008ECC")+geom_segment(aes(x=predictor,xend=predictor,y=0,yend=`%`), color="#868b8c", size=.75) + coord_flip() +labs(title="Missing Data Per Variable", subtitle ="N/A Values >1% of All Observations", x="", y="Percentage")+theme_bw()+theme()

# UNUSED MODELS

#SVM
##For SVM, we choose to work with a non-linear, radial kernel because many of our data's features did not appear to be linearly separable. SVM models work well in maintain a large amount of features and can make distinctions between class differences. 
##Our RMSE Cross Validation plots show us that both models performed similiary, but the second model performed slightly better. We choose this model, which applied zero-varience, box-cox, centering, and spread transformations, to be our preferred SVM model.