# Libraries
library(mlbench)
library(AppliedPredictiveModeling)
library(party)
library(randomForest)
library(caret)
library(tidyverse)
library(gbm)

# Set Seed 
set.seed(200)

# (8.1)
simulated <- mlbench.friedman1(200, sd = 1) 
simulated <- cbind(simulated$x, simulated$y)
simulated <- as.data.frame(simulated) 
colnames(simulated)[ncol(simulated)] <- "y"

# (8.1a)
model1 <- randomForest(y ~ ., data = simulated, importance = TRUE, ntree = 1000)
rfImp1 <- varImp(model1, scale = FALSE)

# (8.1b)
simulated$duplicate1 <- simulated$V1 + rnorm(200) * .1 
cor(simulated$duplicate1, simulated$V1)
model2 <- randomForest(y ~ ., data = simulated, importance = T, ntree = 1000)
rfImp2 <- varImp(model2, scale = F)

# (8.1c)
# Rename variables for ease of reference
sim_original <- select(simulated, -duplicate1); sim_duplicate <- simulated
model3 <-  cforest(y ~ ., data=sim_original, controls = cforest_unbiased(ntree = 1000))
cfImp3 <- varimp(model3, conditional = T)
model4 <- cforest(y ~ ., data = sim_duplicate, controls = cforest_unbiased(ntree = 1000))
cfImp4 <- varimp(model4, conditional = T)

# (8.1d)
# Create tuning parameters
fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)
model5 <-  train(y ~ ., data = sim_original,  method = "gbm", trControl = fitControl, verbose = FALSE)
gbmImp5 <- varImp(model5, conditional = T, useModel=T)
model6 <- train(y ~ ., data = sim_duplicate,  method = "gbm", trControl = fitControl, verbose = FALSE)
gbmImp6 <- varImp(model6, conditional = T, useModel = T)