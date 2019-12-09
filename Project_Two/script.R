library(tidyverse)
library(MLmetrics)
library(caret)
library(caretEnsemble)
library(psych)
library(stats)
library(data.table)
library(readxl)

## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx') 

## Clean/Standardize Variable Naming Conventions for Both Sets
names(StudentData) <- gsub(" ", "", names(StudentData))
names(StudentEvaluation) <- gsub(" ", "", names(StudentEvaluation))

# Load saved work space
train <- readRDS(file = "_train.rsd")
test <- readRDS(file = "_test.rsd")
eval<- readRDS(file = "_eval.rsd")
train_trans <- readRDS(file = "_train_trans.rsd")
test_trans <- readRDS(file = "_test_trans.rsd")
eval_trans <- readRDS(file = "_eval_trans.rsd")
fit_mars1 <- readRDS("_fit_mars1.rsd")
fit_mars2 <- readRDS("_fit_mars2.rsd")
fit_rf1 <- readRDS("_fit_rf1.rsd")
fit_rf2 <- readRDS("_fit_rf2.rsd")
fit_cub1 <- readRDS("_fit_cub1.rsd")
fit_cub2 <- readRDS("_fit_cub2.rsd")
fit_svm1 <- readRDS("_fit_svm1.rsd")
fit_svm2 <- readRDS("_fit_svm2.rsd")


# CUSTOM FUNCTIONS
gather_if <- function(data, FUN, key = "key", value = "value", na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  data %>% {gather(., key = key, value = value , names(.)[sapply(., FUN = FUN)], na.rm = na.rm, convert = convert, factor_key = factor_key )}} 

flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.table(row = rownames(cormat)[row(cormat)[ut]], column = rownames(cormat)[col(cormat)[ut]], cor  = (cormat)[ut])}

# DATA EXPLORATION


## Missing Data Analysis
MissingData <- StudentData %>% summarise_all(funs(sum(is.na(.)))) %>%  t() %>% as.data.frame() %>% rename("n"=V1) %>% rownames_to_column("predictor") %>% arrange(desc(n)) %>% mutate(`%`=round((n/nrow(StudentData)*100),2)) 

## Outiers Analysis
outliers <-StudentData %>%  mutate(PH=as.factor(PH))%>% gather_if(is.numeric, 'key', 'value') %>% filter(!is.na(value)) %>%  group_by(key)  %>%  mutate(outlier_lower = quantile(value, probs=.25, na.rm = T)-1.5 * IQR(value, na.rm = T), outlier_upper = 1.5 * IQR(value, na.rm = T)+quantile(value, probs = .75, na.rm = T), outlier=ifelse(value<outlier_lower, "TRUE", ifelse(value>outlier_upper, "TRUE", "FALSE"))) 
outlier_with <- outliers %>% filter(any(outlier=="TRUE")) 
outlier_wo <- outliers %>% filter(all(outlier!="TRUE")) 
outlier_freq <- outliers %>% select(key, outlier) %>% table() %>% as.data.frame.array() %>% rownames_to_column("variable") %>% arrange(desc(`TRUE`)) %>% mutate(`%`=round(`TRUE`/(`FALSE`+`TRUE`)*100,2)) %>% top_n(5,`%`)

## Correlation
cor <- StudentData %>% select(-PH, -BrandCode) %>% cor(use = "pairwise.complete.obs")
cor_freq <- findCorrelation(cor, .7, names = T)
cor_flat <- flattenCorrMatrix(cor) %>% arrange(row, desc(cor)) %>% filter(cor >.75 | cor < -0.75) 
cor_flat_left <- cor_flat %>% slice(1:10) %>% mutate(id = row_number())
cor_flat_right <- cor_flat %>% slice(11:20) %>% mutate(id = row_number())


# MODEL GROUP 1 (No Pre-Process; Caret - Bag Impute)


# TRAIN PERFORMANCE
mape.train1 <- rbind(
  MAPE(fit_mars1$pred$pred, fit_mars1$pred$obs),
  MAPE(fit_rf1$pred$pred, fit_rf1$pred$obs),
  MAPE(fit_svm1$pred$pred, fit_svm1$pred$obs),
  MAPE(fit_cub1$pred$pred, fit_cub1$pred$obs))

perf.train1 <- lapply(c(fit_mars1, fit_rf1, fit_svm1, fit_cub1), getTrainPerf)
tbl.perf.train1 <- purrr::flatten_df(data.frame(cbind(perf.train1))) %>%
  cbind(mape.train1) %>% 
  rename(MAPE = mape.train1,
         RMSE = TrainRMSE,
         RSquared = TrainRsquared,
         MAE = TrainMAE,
         Method=method) %>%
  select(5, 1:3,4) %>% 
  arrange(MAPE) 

# TEST PERFORMANCE

pred.test1 <- lapply(c(fit_mars1, fit_rf1, fit_svm1, fit_cub1), predict, newdata=test)

mape.test1 <- rbind(
  MAPE(pred.test1$mars, test$PH),
  MAPE(pred.test1$rf, test$PH),
  MAPE(pred.test1$svm, test$PH),
  MAPE(pred.test1$cub, test$PH))
colnames(mape.test1) <- "MAPE"
perf.test1 <- lapply(pred.test1, postResample, obs=test$PH)

tbl.perf.test1 <- cbind(mape.test1,
                        rbind(cbind(rbind(perf.test1$earth), Method="earth"),
                              cbind(rbind(perf.test1$rf2), "rf"),
                              cbind(rbind(perf.test1$svmRadial3), "svmRadial"),
                              cbind(rbind(perf.test1$cubist4), "cubist")))  %>%
  as.data.frame() %>%
  mutate_at(vars(-Method), funs(as.numeric(as.character(.)))) %>% 
  arrange(MAPE)


# MODEL GROUP 2 (Extra Pre-Process; Caret - Bag Impute)

# TRAIN PERFORMANCE
mape.train2 <- rbind(
  MAPE(fit_mars2$pred$pred, fit_mars2$pred$obs),
  MAPE(fit_rf2$pred$pred, fit_rf2$pred$obs),
  MAPE(fit_svm2$pred$pred, fit_svm2$pred$obs),
  MAPE(fit_cub2$pred$pred, fit_cub2$pred$obs))

perf.train2 <- lapply(c(fit_mars2, fit_rf2, fit_svm2, fit_cub2), getTrainPerf)
tbl.perf.train2 <- purrr::flatten_df(data.frame(cbind(perf.train2))) %>%
  cbind(mape.train2) %>% 
  rename(MAPE = mape.train2,
         RMSE = TrainRMSE,
         RSquared = TrainRsquared,
         MAE = TrainMAE,
         Method=method) %>%
  select(5, 1:3,4) %>% 
  arrange(MAPE) 

# TEST PERFORMANCE

pred.test2 <- lapply(c(fit_mars2, fit_rf2, fit_svm2, fit_cub2), predict, test_trans)

mape.test2 <- rbind(
  MAPE(pred.test2$mars, test$PH),
  MAPE(pred.test2$rf, test$PH),
  MAPE(pred.test2$svm, test$PH),
  MAPE(pred.test2$cub, test$PH))
colnames(mape.test2) <- "MAPE"
perf.test2 <- lapply(pred.test2, postResample, obs=test$PH)

tbl.perf.test2 <- cbind(mape.test2,
                        rbind(cbind(rbind(perf.test2$earth), Method="earth"),
                              cbind(rbind(perf.test2$rf2), "rf"),
                              cbind(rbind(perf.test2$svmRadial3), "svmRadial"),
                              cbind(rbind(perf.test2$cubist4), "cubist")))  %>%
  as.data.frame() %>%
  mutate_at(vars(-Method), funs(as.numeric(as.character(.)))) %>% 
  arrange(MAPE)

# VARIABLE IMPORTANCE
varImp.MARS <- varImp(fit_mars2, scale = T)
#varImp.SVM <- varImp(fit_svm2, scale = T)
varImp.Cub <- varImp(fit_cub2, scale = T)
varImp.RF <- varImp(fit_rf2, scale = T)


