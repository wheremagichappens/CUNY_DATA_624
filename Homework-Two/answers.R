## MERGE HW QUESTIONS HERE:

## Ensure constistent variable naming convention per question
## Look out for duplicate variable names between questions

# DEPENDENCIES

# Predicitve Modeling
libraries('AppliedPredictiveModeling', 'mice','caret', 'tidyverse','impute','pls','caTools','mlbench')
# Formatting Libraries
libraries('default', 'knitr', 'kableExtra','gridExtra','sqldf')
# Plotting Libraries
libraries('ggplot2', 'grid', 'ggfortify')

set.seed(58677)

# ASSIGNMENT 1 
# KJ 6.3
data("ChemicalManufacturingProcess")

# (6.3a) 
yield_plot<-ggplot(ChemicalManufacturingProcess, aes(x = Yield))+
  geom_histogram(colour ='black', fill = 'violetred4')+
  labs(title="Distribution of Yield",
       subtitle="Chemical Manufacturing Data Set ")

# (6.3b) 
#code
# Total NA Values
#na_table<- table(is.na(ChemicalManufacturingProcess))
total_na<-sapply(ChemicalManufacturingProcess[2:57], function(x) sum(is.na(x)))
na_table<-sapply(ChemicalManufacturingProcess, function(x) table(is.na(x)))
total_na<- data.frame(sort(total_na, decreasing = TRUE))
total_na<- cbind(Variable = rownames(total_na), total_na)
rownames(total_na) <- 1:nrow(total_na)
colnames(total_na)<-  c("Variable", "Count")
total_na<-cbind(total_na[1:28,],total_na[29:56,])

#code
# save df
df <- ChemicalManufacturingProcess

# set seed for split to allow for reproducibility
set.seed(58677)

# use mice w/ default settings to impute missing data
miceImput <- mice(df, printFlag = FALSE)

# add imputed data to original data set 
df_mice <-mice::complete(miceImput)

# Look for any features with no variance:
zero_cols <- nearZeroVar( df_mice )
df_final <- df_mice[,-zero_cols] # drop these zero variance columns 

# (6.3c)
#code
set.seed(58677)   #  set seed to ensure you always have same random numbers generated

sample = sample.split(df_final, SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE

chem_train =subset(df_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE

chem_test=subset(df_final, sample==FALSE)

#code
pls_model <- plsr(Yield~., data=chem_train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE)


pls_model2 <- plsr(Yield~., data=chem_train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE,
            ncomp =41)


#  Train Metrics
train_eval=data.frame('obs' = chem_train$Yield, 'pred' =pls_model$fitted.values)
colnames(train_eval) <- c('obs', 'pred')

# (6.3d)
#code
# #Test Predictions & Metrics
pls2_pred<- predict(pls_model2, chem_test, ncomp=41)

pls2test_eval=data.frame('obs' = chem_test$Yield, 'pred' =pls2_pred)

colnames(pls2test_eval) <- c('obs', 'pred')



caret::defaultSummary(pls2test_eval)%>% kable(caption="PLS Performance Metrics on Test Subset") %>% kable_styling()# %>% row_spec()

eval_plot <- ggplot(pls2test_eval, aes(obs, pred)) + 
  labs(title="Observed vs. Predicted Results for Test Data",
       subtitle="Partial Least Squares Model")+ 
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme()

# (6.3e) 
#code
importance <- caret::varImp(pls_model2, scale=FALSE)
importance<-importance%>%
    mutate(Variable = row.names(importance))%>%
    remove_rownames()%>%
    select(Variable, Overall)%>%
    arrange(desc(Overall))
    
imp_plot <- ggplot(head(importance, 15), aes(x=reorder(Variable, Overall), y=Overall)) + 
    geom_point(colour = 'violetred4') + 
    geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall),colour = 'violetred4') + 
    labs(title="Variable Importance", 
         subtitle="PSL Model for Chemical Manufacturing Process Data Set", x="Variable", y="Importance")+ 
    coord_flip()+
    theme_bw()+
    theme()

# (6.3f)
#code
# F Comparison
p1 <-qplot(ManufacturingProcess32,Yield,  data =ChemicalManufacturingProcess)+ 
  geom_smooth(method = "loess", se =FALSE)+
    labs(title="Manufacturing process 32 vs Yield")

p2 <-qplot(ManufacturingProcess13,Yield,   data =ChemicalManufacturingProcess)+ 
  geom_smooth(method = "loess", se =FALSE)+
   labs(title="Manufacturing process 13 vs Yield")

p3 <-qplot( ManufacturingProcess17, Yield, data =ChemicalManufacturingProcess)+ 
  geom_smooth(method = "loess", se =FALSE)+
   labs(title="Manufacturing process 17 vs Yield")

 #code
imp_train <- df_final %>%select(Yield, ManufacturingProcess32,ManufacturingProcess17, ManufacturingProcess13, ManufacturingProcess36, ManufacturingProcess09 )
cor_df<-as.data.frame(as.matrix(cor(imp_train)))


# ASSIGNMENT 2
# KJ 7.2; KJ 7.5
# The package `mlbench` contains a function called `mlbench.friedman1` 
# that simulates these data:
set.seed(200) 
trainingData <- mlbench.friedman1(200, sd = 1)
## We convert the 'x' data from a matrix to a data frame 
## One reason is that this will give the columns names.
trainingData$x <- data.frame(trainingData$x) 
## Look at the data using 
#featurePlot(trainingData$x, trainingData$y) 
## or other methods. 
## This creates a list with a vector 'y' and a matrix 
## of predictors 'x'. Also simulate a large test set to 
## estimate the true error rate with good precision: 
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x) 


#over ride set seed from sample data simulation
set.seed(58677)

#KNN provided by literature
knnModel <- train(x = trainingData$x,
                  y = trainingData$y, 
                  method = "knn",
                  preProc = c("center", "scale"), 
                  tuneLength = 10) 
knnModel 
knnPred <- predict(knnModel, newdata = testData$x) 
## The function 'postResample' can be used to get the test set performance values
postResample(pred = knnPred, obs = testData$y)

# (7.2a)
#mars
marsGrid <- expand.grid(degree =1:2, nprune=seq(2,14,by=2))
hw2mars_mod <- train(x = trainingData$x, y = trainingData$y, method='earth', tuneGrid = marsGrid, trControl = trainControl(method = "cv"))
hw2mars_modplot<- ggplot(hw2mars_mod)+theme_bw()+theme()+labs(title="MARS Cross-Validated RMSE Profile")

#svm
hw2svm_mod <- train(x = trainingData$x, y = trainingData$y, method='svmRadial', tuneLength = 14, trControl = trainControl(method = "cv"))
#hw2svm_mod$finalModel
hw2svm_modplot<- ggplot(hw2svm_mod)+theme_bw()+theme()+labs(title="SVM Cross-Validated RMSE Profile")


##nnet (Taken from Andy)
# hyperparameter tuning for nnet
nnetGrid <- expand.grid(.size = c(1:10), .decay = c(0, 0.01, .1))

hw2nnet_mod <- train(trainingData$x, trainingData$y,method = "nnet", tuneGrid = nnetGrid,trControl = trainControl(method="cv"),
 ## Automatically standardize data prior to modeling and prediction
 preProc = c("center", "scale"),
 linout = TRUE,
 trace = FALSE,
 MaxNWts = 10 * (ncol(trainingData$x) + 1)  + 10 +  1,
 maxit = 500)

hw2nnet_modplot<- ggplot(hw2nnet_mod)+theme_bw()+theme()+labs(title="NNET Cross-Validated RMSE Profile")


# (7.2b)
#knn pred given to us 
hw2marsPred <- predict(hw2mars_mod, newdata = testData$x) 
hw2svmPred <- predict(hw2svm_mod, newdata = testData$x) 
hw2nnetPred <- predict(hw2nnet_mod, newdata = testData$x) 

knn_performance <- postResample(pred = knnPred, obs = testData$y)
hw2marsPerf <- postResample(pred = hw2marsPred, obs = testData$y)
hw2svmPerf <- postResample(pred = hw2svmPred, obs = testData$y)
hw2nnetPerf <- postResample(pred = hw2nnetPred, obs = testData$y)

hw2.2.bperformance_table <- rbind("knnTrain"=c("RMSE"=max(knnModel$results$RMSE),
  "RSquared"=max(knnModel$results$RMSE),
  "MAE"=max(knnModel$results$RMSE)),
"knnTest"=knn_performance, 
"MARSTrain"=c("RMSE"=max(hw2mars_mod$results$RMSE),
  "RSquared"=max(hw2mars_mod$results$Rsquared),
  "MAE"=max(hw2mars_mod$results$MAE)),
   "MARSTest"=hw2marsPerf,
    "SVMTrain"=c(max(hw2svm_mod$results$RMSE),
      max(hw2svm_mod$results$Rsquared),
      max(hw2svm_mod$results$MAE)),
    "SVMTest"=hw2svmPerf,
    "NNETTrain"=c(max(hw2nnet_mod$results$RMSE),
  max(hw2nnet_mod$results$Rsquared),
  max(hw2nnet_mod$results$MAE)),
   "NNETTest"=hw2nnetPerf) %>% kable(caption="Model Performance", digits=4) %>% kable_styling() %>% row_spec() %>% row_spec(row=3:4, background ="#d9f2e6")

#hw2marsImp <- varImp(hw2mars_mod)
#hw2marsImptbl <- hw2marsImp$importance %>% kable(caption="MARS Model - Variable Importance", digits=2) %>% kable_styling()

hw2marsImp <- caret::varImp(hw2mars_mod)
#hw2marsImp<-hw2marsImp%>%
#    mutate(Variable = row.names(hw2marsImp))%>%
#    remove_rownames()%>%
#    select(Variable, Overall)%>%
#    arrange(desc(Overall))

hw2marsImp<-hw2marsImp$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname ))
    
hwmarsimp_plot <- ggplot(head(hw2marsImp, 15), aes(x=reorder(rowname, Overall), y=Overall)) + 
    geom_point(colour = 'violetred4') + 
    geom_segment(aes(x=rowname,xend=rowname,y=0,yend=Overall),colour = 'violetred4') + 
    labs(title="Variable Importance", 
         subtitle="MARS for Simulated Data Set", x="Variable", y="Importance")+ 
    coord_flip()+
    theme_bw()+
    theme()

# (7.5a)
##knn 
hw2knn_mod2 <- train(Yield~.,
                  data=chem_train,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10)

#nnet
nnetGrid_75 <- expand.grid(.decay = c(0, 0.01, .1),
                        .size = c(1:10),
                        .bag = FALSE)

hw2nnet_mod2 <- train(Yield~.,
                  data=chem_train,
                  method = "avNNet",
                  tuneGrid = nnetGrid_75,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  MaxNWts = 10 * (ncol(chem_train) + 1) + 5 + 1,
                  maxit = 500)

#MARS 
# Define the candidate models to test
marsGrid_75 <- expand.grid(.degree = 1:2, .nprune = 2:38)

hw2mars_mod2 <- train(Yield~.,
                  data=chem_train,
                   method = "earth",
                   tuneGrid = marsGrid_75,
                   trControl = trainControl(method = "cv"))

#SVM
hw2svm_mod2 <- train(Yield~.,
                  data=chem_train,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneLength = 14,
                   trControl = trainControl(method = "cv"))


#model performances 
#knn pred given to us 
hw2knnPred2 <- predict(hw2knn_mod2, newdata = chem_test) 
hw2nnetPred2 <- predict(hw2nnet_mod2, newdata = chem_test) 
hw2marsPred2 <- predict(hw2mars_mod2, newdata = chem_test) 
hw2svmPred2 <- predict(hw2svm_mod2, newdata = chem_test) 

hw2knnPerf2 <- postResample(pred = hw2knnPred2, obs = chem_test$Yield)
hw2marsPerf2 <- postResample(pred = hw2marsPred2, obs = chem_test$Yield)
hw2svmPerf2 <- postResample(pred = hw2svmPred2, obs = chem_test$Yield)
hw2nnetPerf2 <- postResample(pred = hw2nnetPred2, obs = chem_test$Yield)

hw2.2.cperformance_table <- rbind("knnTrain"=c("RMSE"=max(hw2knn_mod2$results$RMSE),
  "RSquared"=max(hw2knn_mod2$results$Rsquared),
  "MAE"=max(hw2knn_mod2$results$MAE)),
"knnTest"=hw2knnPerf2, 
"MARSTrain"=c("RMSE"=max(hw2mars_mod2$results$RMSE),
  "RSquared"=max(hw2mars_mod2$results$Rsquared),
  "MAE"=max(hw2mars_mod2$results$MAE)),
   "MARSTest"=hw2marsPerf2,
    "SVMTrain"=c(max(hw2svm_mod2$results$RMSE),
      max(hw2svm_mod2$results$Rsquared),
      max(hw2svm_mod2$results$MAE)),
    "SVMTest"=hw2svmPerf2,
    "NNETTrain"=c(max(hw2nnet_mod2$results$RMSE),
  max(hw2nnet_mod2$results$Rsquared),
  max(hw2nnet_mod2$results$MAE)),
   "NNETTest"=hw2nnetPerf2) %>% kable(caption="Model Performance on ChemicalManufacturing Data", digits=4) %>% kable_styling() %>% row_spec() %>% row_spec(row=5:6, background ="#d9f2e6")

# (7.5b)
#hw2svmImp2 <- varImp(hw2svm_mod2)
#hw2svmImptbl2 <- hw2svmImp2$importance %>% kable(caption="SVM Model - Variable Importance", digits=2) %>% kable_styling()

hw2svmImp2 <- caret::varImp(hw2svm_mod2) 

hw2svmImp2<-hw2svmImp2$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) 
    
hwsvmimp_plot2 <- ggplot(head(hw2svmImp2, 15), aes(x=reorder(rowname, Overall), y=Overall)) + 
    geom_point(colour = 'violetred4') + 
    geom_segment(aes(x=rowname,xend=rowname,y=0,yend=Overall),colour = 'violetred4') + 
    labs(title="Variable Importance", 
         subtitle="SVM Model Importance for ChemicalManufacturing Data", x="Variable", y="Importance")+ 
    coord_flip()+
    theme_bw()+
    theme()

# (7.5c)
#alterate apprach (use plot importance to identify top few important features)
hw2imp <- df_final %>%select(Yield, 
  ManufacturingProcess14,
  ManufacturingProcess02, 
  ManufacturingProcess03,
   ManufacturingProcess38, 
   ManufacturingProcess37 )

hw2cor_pre_df<- as.data.frame(as.matrix(cor(hw2imp)))

hw2cor_df<-tibble::rownames_to_column(hw2cor_pre_df, "VALUE")

hw2cor_df2<-sqldf("select VALUE, Yield from hw2cor_df order by Yield desc")%>% 
kable(caption="Correlation") %>% 
kable_styling(latex_options="scale_down")


# ASSIGNMENT 3
# KJ 8.1-8.3; KJ 8.7

# (8.1a)

# (8.1b)

# (8.1c)

# (8.1d)

# (8.2)

# (8.3a)

# (8.3b)

# (8.3c)

# (8.7a)

# (8.7b)

# (8.7c)