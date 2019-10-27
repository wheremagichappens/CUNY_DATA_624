require(AppliedPredictiveModeling)
require(tidyverse)
require(impute)
require(caTools)
require(pls)
require(kableExtra)
require(ggplot2)
require(stargazer)
require(caret)
require(tidyverse)
require(gridExtra)
options(scipen = 999)
# a.
data("ChemicalManufacturingProcess")

# Total NA Values
#na_table<- table(is.na(ChemicalManufacturingProcess))
total_na<-sapply(ChemicalManufacturingProcess[2:57], function(x) sum(is.na(x)))
na_table<-sapply(ChemicalManufacturingProcess, function(x) table(is.na(x)))

total_na<- data.frame(sort(total_na, decreasing = TRUE))
total_na<- cbind(Variable = rownames(total_na), total_na)
rownames(total_na) <- 1:nrow(total_na)
colnames(total_na)<-  c("Variable", "Count")
total_na<-cbind(total_na[1:28,],total_na[29:56,])

#histogram
hist_yield <-ggplot(ChemicalManufacturingProcess, aes(x = Yield))+
    geom_histogram(colour ='black', fill = 'violetred4') +
    ggtitle('Distribution of Yield Chemical Manufacturing Process Data')

# b. Imputing Values

imputed_data = data.frame(impute.knn(as.matrix(ChemicalManufacturingProcess),
       k =10,
       rowmax =.30,
       colmax =.85,
       rng.seed =1942)$data)



imp_proc_34 <- summary(imputed_data$ManufacturingProcess34)
proc_34 <- summary(ChemicalManufacturingProcess$ManufacturingProcess34)
imp_proc_03 <- summary(imputed_data$ManufacturingProcess03)
proc_03 <- summary(ChemicalManufacturingProcess$ManufacturingProcess03)

# c. tts, train and evaluate
set.seed(1492)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(imputed_data, SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(imputed_data,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(imputed_data, sample==FALSE)


# 
fit <- plsr(Yield~., data=train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE)


# plot(fit)
# abline(0, 1, col="red")
# #
# validationplot(fit, val.type="RMSEP")
# #
# #
# rmse_pls <- RMSEP(fit)


# best_comps <- which.min(rmse_pls$val)
# points(best_comps, min(rmse_pls$val), pch=1, col="red", cex=1.5)
# 57 is the best lowest number of components

# Best Train

fit_41 <- plsr(Yield~., data=train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE,
            ncomp =41)

#  Train Metrics
train_eval=data.frame('obs' = train$Yield, 'pred' =fit$fitted.values)
colnames(train_eval) <- c('obs', 'pred')
caret::defaultSummary(train_eval)
#     RMSE  Rsquared       MAE
# 1.3757981 0.4598005 1.1110483
# 
# # d.
# 
# #Test Predictions & Metrics
test_pred_41 <- predict(fit_41, test, ncomp=41)
test_eval_41=data.frame('obs' = test$Yield, 'pred' =test_pred_41)
colnames(test_eval_41) <- c('obs', 'pred')
caret::defaultSummary(test_eval_41)


eval_plot <- ggplot(test_eval_41, aes(obs, pred)) + labs(title="Observed vs. Predicted Results for Test Data", subtitle="Partial Least Squares Model")+ geom_point()+coord_flip()+theme_bw()+theme()
# e Importance

importance <- caret::varImp(fit_41, scale=FALSE)

importance<-importance%>%
    mutate(Variable = row.names(importance))%>%
    remove_rownames()%>%
    select(Variable, Overall)%>%
    arrange(desc(Overall))
    
imp_plot <- ggplot(head(importance, 15), aes(x=reorder(Variable, Overall), y=Overall)) + 
    geom_point(colour = 'violetred4') + 
    geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall),colour = 'violetred4') + 
    labs(title="Variable Importance", subtitle="PSL Model for Chemical Manufacturing Process Data Set", x="Variable", y="Importance")+ 
    coord_flip()+
    theme_bw()+
    theme()


# F Comparison

p1 <-qplot(ManufacturingProcess32,Yield,  data =ChemicalManufacturingProcess)+ 
    geom_smooth(method = "loess", se =FALSE, colour ='violetred4')
p2 <-qplot(ManufacturingProcess13,Yield,   data =ChemicalManufacturingProcess)+ 
    geom_smooth(method = "loess", se =FALSE, colour ='violetred4')
p3 <-qplot( ManufacturingProcess17, Yield, data =ChemicalManufacturingProcess)+ 
    geom_smooth(method = "loess", se =FALSE, colour ='violetred4')
