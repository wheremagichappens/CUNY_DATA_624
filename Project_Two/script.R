library(tidyverse)
library(MLmetrics)
library(caret)
library(caretEnsemble)

load(file = "model_prep.RData")

# MODEL GROUP 1 (No Pre-Process; Caret - Bag Impute)
# TRAIN PERFORMANCE
mape.train1 <- rbind(
  MAPE(fit_models1$mars$pred$pred, fit_models1$mars$pred$obs),
  MAPE(fit_models1$rf$pred$pred, fit_models1$rf$pred$obs),
  MAPE(fit_models1$svm$pred$pred, fit_models1$svm$pred$obs),
  MAPE(fit_models1$cub$pred$pred, fit_models1$cub$pred$obs))

perf.train1 <- lapply(fit_models1, getTrainPerf)
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

pred.test1 <- lapply(fit_models1, predict, newdata=test)

mape.test1 <- rbind(
  MAPE(pred.test1$mars, test$PH),
  MAPE(pred.test1$rf, test$PH),
  MAPE(pred.test1$svm, test$PH),
  MAPE(pred.test1$cub, test$PH))
colnames(mape.test1) <- "MAPE"
perf.test1 <- lapply(pred.test1, postResample, obs=test$PH)
tbl.perf.test1 <- cbind(mape.test1,
                        rbind(cbind(rbind(perf.test1$mars), Method="earth"),
                              cbind(rbind(perf.test1$rf), "rf"),
                              cbind(rbind(perf.test1$svm), "svmRadial"),
                              cbind(rbind(perf.test1$cub), "cubist")))  %>%
  as.data.frame() %>%
  mutate_at(vars(-Method), funs(as.numeric(as.character(.)))) %>% 
  arrange(MAPE)


# MODEL GROUP 2 (Extra Pre-Process; Caret - Bag Impute)

# TRAIN PERFORMANCE
mape.train2 <- rbind(
  MAPE(fit_models2$mars$pred$pred, fit_models2$mars$pred$obs),
  MAPE(fit_models2$rf$pred$pred, fit_models2$rf$pred$obs),
  MAPE(fit_models2$svm$pred$pred, fit_models2$svm$pred$obs),
  MAPE(fit_models2$cub$pred$pred, fit_models2$cub$pred$obs))

perf.train2 <- lapply(fit_models2, getTrainPerf)
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

pred.test2 <- lapply(fit_models2, predict, newdata=test_trans)

mape.test2 <- rbind(
  MAPE(pred.test2$mars, test_trans$PH),
  MAPE(pred.test2$rf, test_trans$PH),
  MAPE(pred.test2$svm, test_trans$PH),
  MAPE(pred.test2$cub, test_trans$PH))
colnames(mape.test2) <- "MAPE"
perf.test2 <- lapply(pred.test2, postResample, obs=test_trans$PH)
tbl.perf.test2 <- cbind(mape.test2,
                        rbind(cbind(rbind(perf.test2$mars), Method="earth"),
                              cbind(rbind(perf.test2$rf), "rf"),
                              cbind(rbind(perf.test2$svm), "svmRadial"),
                              cbind(rbind(perf.test2$cub), "cubist")))  %>%
  as.data.frame() %>%
  mutate_at(vars(-Method), funs(as.numeric(as.character(.)))) %>% 
  arrange(MAPE)
