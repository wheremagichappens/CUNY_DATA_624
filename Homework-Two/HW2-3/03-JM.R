# Libraries
library(mlbench)
library(AppliedPredictiveModeling)
library(party)
library(randomForest)
library(caret)
library(tidyverse)
library(gbm)
library(mice)
library(recipes)
library(Cubist)

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
model2 <- randomForest(y ~ ., data = simulated, importance = T, ntree = 1000)
rfImp2 <- varImp(model2, scale = F)

# (8.1c)
# Rename variables for ease of reference
sim_original <- select(simulated, -duplicate1); sim_duplicate <- simulated

model3 <-  cforest(y ~ ., data=sim_original, controls = cforest_unbiased(ntree = 1000))
cfImp3 <- varimp(model3, conditional = T)
cfImp3df <- as.data.frame(cfImp3) %>% rownames_to_column("Variable") %>% rename("Overall"="cfImp3")
model4 <- cforest(y ~ ., data = sim_duplicate, controls = cforest_unbiased(ntree = 1000))
cfImp4 <- varimp(model4, conditional = T) 
cfImp4df <- as.data.frame(cfImp4) %>% rownames_to_column("Variable") %>% rename("Overall"="cfImp4")
cfImpTbl <- right_join(cfImp3df, cfImp4df, by="Variable") %>% column_to_rownames("Variable") %>% rename("Original"=Overall.x, "Duplicate"=Overall.y) 

# (8.1d)
model5 <-  gbm(y ~ .,data = sim_original,distribution='gaussian',n.trees=1000, cv.folds=5)
gbmImp5 <- summary(model5, plot=F) %>% mutate(var=as.character(var))
gbmImp5df <-  as.data.frame(gbmImp5) %>% arrange(desc(rel.inf)) 
model6 <- gbm(y ~ .,data = sim_duplicate,distribution='gaussian',n.trees=1000, cv.folds=5)
gbmImp6 <- summary(model6, plot=F) %>% mutate(var=as.character(var))
gbmImp6df <- as.data.frame(gbmImp6) %>% arrange(desc(rel.inf)) 
gbmImpTbl <- right_join(gbmImp5df, gbmImp6df, by="var") %>%column_to_rownames("var") %>% rename("Original"=rel.inf.x, "Duplicate"=rel.inf.y) 

# (8.2)
random_predictor <- data.frame(V1=sample(1:2, 100, replace=TRUE), V2=sample(1:100, 100, replace=TRUE),V3=sample(1:1000, 100, replace=TRUE), V4=sample(1:5000, 100, replace=TRUE))
sim_df <- random_predictor %>% mutate(y=V1*V2*V3+rnorm(100))
sim_rf <- randomForest(y ~ ., data = sim_df, importance = TRUE, ntree = 1000)
sim_varImp <- varImp(sim_rf, scale=T)

# (8.7)

# FOR FINAL HW SUBMISSION, DO NOT REPEAT STEPS: JUST CALL VARIABLES FROM PRIOR ASSIGNMENT.

data(ChemicalManufacturingProcess)
CMP_Impute <- mice(ChemicalManufacturingProcess, m=5, printFlag=F)
CMP_DF <- mice::complete(CMP_Impute, 2)

# Create Partition for Train/Test Splits
trainingRows <- createDataPartition(CMP_DF$Yield, p = .80, list= FALSE)

# Split Train/Test Data 
train <- CMP_DF[trainingRows, ] 
test <- CMP_DF[-trainingRows, ] 

# Pre-Process Recipe 
rec <- recipes::recipe(CMP_DF, Yield~.)
rec <- rec %>% step_nzv( all_predictors(), options = list(freq_cut = 95/5, unique_cut = 10 ) )
prep_rec = prep(rec, training = CMP_DF)
CMP_DF_TF = bake( prep_rec, CMP_DF)

# Create Partition for Train/Test Splits
trainingRows <- createDataPartition(CMP_DF_TF$Yield, p = .80, list= FALSE)

# Split Train/Test Data 
train <- CMP_DF_TF[trainingRows, ] 
test <- CMP_DF_TF[-trainingRows, ] 

# (8.7a)
# Controls
tr_control <- trainControl(method = "cv", number = 10, repeats = 10)
# Random Foreset
rfModel <- train(Yield~., data=train, method = 'rf',  importance=T,trControl = tr_control, tuneLength = 5)
rfTrainMetric <- data.frame("RMSE"=rfModel$results$RMSE, "RSquared"=rfModel$results$Rsquared, "MAE"=rfModel$results$MAE) %>% filter(RMSE == min(RMSE))
rfPred <- predict(rfModel, newdata = test) 
rfPerf <- postResample(pred = rfPred, obs = test$Yield)
rfPlot <- ggplot(rfModel)+labs(title="Random Forest Model Cross-Validated RMSE Profile")+ theme(legend.position="bottom")

# Gradient Boosted
gbmGrid <- expand.grid(n.trees=c(50, 100, 1000), interaction.depth=c(2, 7, 10), shrinkage=c(.1, .5), n.minobsinnode=c(5,10))
gbmModel <- train(Yield~., data=train, method = 'gbm',  trControl = tr_control, tuneGrid=gbmGrid, tuneLength = 5)
gbmTrainMetric <- data.frame("RMSE"=gbmModel$results$RMSE, "RSquared"=gbmModel$results$Rsquared, "MAE"=gbmModel$results$MAE) %>% filter(RMSE == min(RMSE))
gbmPred <- predict(gbmModel, newdata = test) 
gbmPerf <- postResample(pred = gbmPred, obs = test$Yield)
gbmPlot<-ggplot(gbmModel)+labs(title="GBM Cross-Validated RMSE")+ theme(legend.position="bottom")

# Cubist
cbGrid <- expand.grid(committees = c(10, 50, 100), neighbors = c(1, 5, 9))
cbModel <- train(Yield~., data=train, method = 'cubist',trControl = tr_control, tuneGrid=cbGrid, tuneLength = 5)
cbTrainMetric <- data.frame("RMSE"=cbModel$results$RMSE, "RSquared"=cbModel$results$Rsquared, "MAE"=cbModel$results$MAE) %>% filter(RMSE == min(RMSE))
cbPred <- predict(gbmModel, newdata = test) 
cbPerf <- postResample(pred = gbmPred, obs = test$Yield)
cbPlot<-ggplot(cbModel)+labs(title="Cubist Cross-Validated RMSE")+ theme(legend.position="bottom")

# Metrics Table
performance <- rbind("rfTrain"=rfTrainMetric, "rfTest"=rfPerf,"gbmTrain"=gbmTrainMetric, "gbmTest"=gbmPerf, "cbTrain"=cbTrainMetric,"cbTest"=cbPerf)

# (8.7b)
cbImp <- varImp(cbModel, scale = F)
cbImpPlot <- cbImp$importance %>% as.data.frame()%>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="Cubist Model Overall Variable Importance", subtitle="Top 10 Predictor Variables")

# (8.7c)
cbDotPlot<- dotplot(cbModel$finalModel,scales = list(Yield = list(cex = .10)))