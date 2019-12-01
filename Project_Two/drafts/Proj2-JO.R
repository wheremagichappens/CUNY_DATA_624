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
library(gbm)


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
StudentData <- read_xlsx('C:/Users/jlobr/OneDrive/Learning/_CUNY_SPS_MSDS/2019_3_Autumn/DATA 624/Repo/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('C:/Users/jlobr/OneDrive/Learning/_CUNY_SPS_MSDS/2019_3_Autumn/DATA 624/Repo/Project_Two/data/StudentEvaluation.xlsx')

## Data Tidying
names(StudentData) <- gsub(" ", "", names(StudentData))
StudentData <- StudentData %>% mutate(BrandCode=as.factor(BrandCode))

## Summary Stats
summary_stats <- describe(StudentData)

## Missing Data Analysis
MissingData <- StudentData %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% as.data.frame() %>% rename("n" = V1) %>% rownames_to_column("predictor") %>% arrange(desc(n)) %>% mutate(`%` = round((n / nrow(StudentData) * 100), 2)) 

## Outiers Analysis
outliers <- StudentData %>%  mutate(PH = as.factor(PH))%>% gather_if(is.numeric, 'key', 'value') %>% filter(!is.na(value)) %>%  group_by(key)  %>%  mutate(outlier_lower = quantile(value, probs=.25, na.rm = T)-1.5 * IQR(value, na.rm = T), outlier_upper = 1.5 * IQR(value, na.rm = T) + quantile(value, probs = .75, na.rm = T), outlier = ifelse(value < outlier_lower, "TRUE", ifelse(value > outlier_upper, "TRUE", "FALSE"))) 
outlier_with <- outliers %>% filter(any(outlier == "TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_wo <- outliers %>% filter(all(outlier != "TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_freq <- outliers %>% select(key, outlier) %>% table() %>% as.data.frame.array() %>% rownames_to_column("variable") %>% arrange(desc(`TRUE`)) %>% mutate(`%` = round(`TRUE` / (`FALSE`+`TRUE`) * 100, 2)) %>% top_n(5, `%`)

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


## BOOSTED

boost_grid <- expand.grid(interaction.depth = seq(1, 7, by = 2), n.trees = seq(100, 1000, by = 50), shrinkage = c(.01, .1), n.minobsinnode = 10)

boost_fit1 <- train(PH ~ ., data = train, method = 'gbm', trControl = trC, tuneGrid = boost_grid2, tuneLength = t1, verbose = F)
boost_fit2 <- train(PH ~ ., data = train, method = 'gbm', preProcess = c('center', 'scale'), trControl = trC, tuneGrid = boost_grid2, tuneLength = tl, verbose = F)
boost_fit3 <- train(PH ~ ., data = train, method = 'gbm', preProcess = c('nzv', 'zv', 'BoxCox'), trControl = trC, tuneGrid = boost_grid2, tuneLength = tl, verbose = F)

boost_fit1$bestTune; min(boost_fit1$results$RMSE)
boost_fit2$bestTune; min(boost_fit2$results$RMSE)  # Boost winner (untuned): 250 trees with depth of 11, shrinkage of .1, and node of 10 for RMSE of .1040737
boost_fit3$bestTune; min(boost_fit3$results$RMSE)

boost_test_pred1 <- predict(boost_fit1, newdata = test)
boost_test_pred2 <- predict(boost_fit2, newdata = test)
boost_test_pred3 <- predict(boost_fit3, newdata = test)

boost_fit4 <- gbm(PH ~ ., data = train, distribution = 'gaussian', n.trees = 250, interaction.depth = 11, shrinkage = .1, cv.folds = 3)
boost_optntree_cv <- gbm.perf(boost_fit4, method = 'cv')
boost_optntree_oob <- gbm.perf(boost_fit4, method = 'OOB')
boost_optntree_cv  # 
boost_optntree_oob

boost_grid_tune <- expand.grid(interaction.depth = 11, n.trees = 240, shrinkage = .01, n.minobsinnode = 10)
boost_fit5 <- train(PH ~ ., data = train, method = 'gbm', preProcess = c('center', 'scale'), trControl = trC, tuneGrid = boost_grid_tune, tuneLength = tl, verbose = F)
boost_fit5$bestTune; min(boost_fit5$results)  # Boost winner (tuned): 240 trees with depth of 11, shrinkage of .01, and node of 10 for RMSE of .00420408
boost_test_pred5 <- predict(boost_fit5, newdata = test)


## CUBIST

cub_grid <- expand.grid(committees = c(1:10), neighbors = c(0, seq(1, 9, by = 2)))  # neighbors must be integer between 1 and 9

cub_fit1 <- train(PH ~ ., data = train, method = 'cubist', trCtrl = trC, tuneGrid = cub_grid, verbose = F)
cub_fit2 <- train(PH ~ ., data = train, method = 'cubist', preProcess = c('center', 'scale'), trCtrl = trC, tuneGrid = cub_grid)
cub_fit3 <- train(PH ~ ., data = train, method = 'cubist', preProcess = c('nzv', 'zv', 'BoxCox'), trCtrl = trC, tuneGrid = cub_grid)

cub_fit1$bestTune; min(cub_fit1$results$RMSE)  # Cubist winner: 10 committees, 9 neighbors, RMSE of .1064531
cub_fit2$bestTune; min(cub_fit2$results$RMSE)
cub_fit3$bestTune; min(cub_fit3$results$RMSE)

cub_test_pred1 <- predict(cub_fit1, newdata = test)
cub_test_pred2 <- predict(cub_fit2, newdata = test)
cub_test_pred3 <- predict(cub_fit3, newdata = test)


# ACCURACY 
MARS_MAPE_TRN <- Metrics::mape(mars_fit1$pred$obs, mars_fit1$pred$pred)
eNET_MAPE_TRN <- Metrics::mape(enet_fit3$pred$obs, enet_fit3$pred$pred)
BOOST_MAPE_TRN <- Metrics::mape(boost_fit5$pred$obs, train$PH)
CUB_MAPE_TRN <- Metrics::mape(predict(cub_fit1, newdata = train), train$PH)  # cubist train object does not return #pred object

MARS_MAPE_TST <- Metrics::mape(test$PH, mars_test_pred1)
eNET_MAPE_TST <- Metrics::mape(test$PH, enet_test_pred3)
BOOST_MAPE_TST <- Metrics::mape(test$PH, boost_test_pred5)
CUB_MAPE_TST <- Metrics::mape(test$PH, cub_test_pred1)

MARS_PERF_TRN <- mars_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% dplyr::select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="MARS_Train") %>% column_to_rownames("Variable") %>% t(); 
eNET_PERF_TRN <- enet_fit3$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% dplyr::select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="eNET_Train") %>% column_to_rownames("Variable") %>% t(); 
BOOST_PERF_TRN <- boost_fit5$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% dplyr::select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable = 'Boost_Train') %>% column_to_rownames('Variable') %>% t();
CUB_PERF_TRN <- cub_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% dplyr::select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable = 'Cubist_Train') %>% column_to_rownames('Variable') %>% t();

MARS_PERF_TST <- postResample(pred = mars_test_pred1, obs = test$PH)
eNET_PERF_TST <- postResample(pred = enet_test_pred3, obs = test$PH)
BOOST_PERF_TST <- postResample(pred = boost_test_pred5, obs = test$PH)
CUB_PERF_TST <- postResample(pred = cub_test_pred1, obs = test$PH)

bind1 <- cbind(MARS_PERF_TRN, 'MARS_Test' = MARS_PERF_TST, eNET_PERF_TRN, 'eNET_Test' = eNET_PERF_TST, BOOST_PERF_TRN, 'Boost_Test' = BOOST_PERF_TST, CUB_PERF_TRN, 'Cubist_Test' = CUB_PERF_TST)
bind2 <- cbind(MARS_MAPE_TRN, MARS_MAPE_TST, eNET_MAPE_TRN, eNET_MAPE_TST, BOOST_MAPE_TRN, BOOST_MAPE_TST, CUB_MAPE_TRN, CUB_MAPE_TST); row.names(bind2) <- 'MAPE'
Tbl_Accuracy <- rbind(bind1, bind2)

# VARIABLE IMPORTANCE 
MARS_VarImp <- varImp(mars_fit1, scale = T)
eNET_VarImp <- varImp(enet_fit3, scale = T)
BOOST_VarImp <- varImp(boost_fit5, scale = T)
CUB_VarImp <- varImp(cub_fit1, scale = T)
