
## Ensure constistent variable naming convention per question
## Look out for duplicate variable names between questions

# DEPENDENCIES
# Data Wrangling 
library(AppliedPredictiveModeling); library(mice); library(caret); library(tidyverse); library(pls); library(caTools); library(mlbench); library(stringr);
# Formatting
library(default); library(knitr); library(kableExtra); 
# Plotting
library(ggplot2); library(grid); library(ggfortify)

# THEME COLORS
dark_gold <- "#745010"
medium_gold <- "#cbbda5"
light_gold <- "#dcd3c3"

# SET SEED
set.seed(58677)

# ASSIGNMENT 1 
# KJ 6.3
data("ChemicalManufacturingProcess")

# (6.3a) 
Plt_CMP.Yield <-ggplot(ChemicalManufacturingProcess, aes(x = Yield))+
  geom_histogram(color=dark_gold, fill = light_gold)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  labs(title="Distribution of Yield",
       subtitle="Chemical Manufacturing Data Set")+
  theme_bw()+theme(axis.title.y = element_blank())

# (6.3b) 

# Total NA Values
#na_table<- table(is.na(ChemicalManufacturingProcess))
CMP_na <- ChemicalManufacturingProcess %>% select(-Yield) %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% as.data.frame() %>% rownames_to_column("Predictor") %>% filter(V1 > 0) %>% arrange(desc(V1)) %>% rename(n=V1)
CMP_na_left <-CMP_na %>% slice(1:14); CMP_na_right <- CMP_na %>% slice(15:28); CMP.total_na <- cbind(CMP_na_left, ` `=" ", CMP_na_right)

# use mice w/ default settings to impute missing data
miceImp <- mice(ChemicalManufacturingProcess, printFlag = FALSE)

# add imputed data to original data set 
CMP_DF <-mice::complete(miceImp)

# (6.3c)
CMP.sample = sample.split(CMP_DF, SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
CMP.train = subset(CMP_DF, CMP.sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
CMP.test = subset(CMP_DF, CMP.sample==FALSE)

# Train model 
CMP.pls.fit <- train(Yield~., data=CMP.train, method = 'pls', preProcess=c('zv', 'nzv', 'center', 'scale'),trControl = trainControl(method = "cv", number = 5, savePredictions = T), tuneLength=10)
CMP.pls.fit.obs_vs_pred <- cbind(Observed = CMP.pls.fit$finalModel$model$.outcome, Predicted = CMP.pls.fit$finalModel$fitted.values) %>% as.data.frame() 
Plt_CMP.fit.obs_vs_pred <- ggplot(CMP.pls.fit.obs_vs_pred, aes(Observed, Predicted)) + 
  geom_point(color=medium_gold) + 
  geom_smooth(method="lm", color=dark_gold, fill=light_gold)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(title="Train Set: Observed vs. Predicted Values")+ 
  theme_bw()+
  theme()

#  Train Metrics
CMP.pls.train.perf <- CMP.pls.fit$results %>% as.data.frame() %>% filter(RMSE==min(RMSE)) %>% select(RMSE, Rsquared, MAE)
Plt_CMP.RMSE <- ggplot(CMP.pls.fit) + geom_line(color=dark_gold) + geom_point(color=medium_gold)+ theme_bw()+theme()+labs(title="PLS Cross-Validation", y="RMSE", x="Components")+scale_x_continuous(labels = scales::number_format(accuracy = 1))

# (6.3d)
## Test Predictions & Metrics
CMP.pls.pred <- predict(CMP.pls.fit, CMP.test)
CMP.pls.test.perf <- postResample(pred = CMP.pls.pred, obs = CMP.test$Yield) %>% t() %>% as.data.frame() 
CMP.pls.test.obs_vs_pred <- cbind(Predicted = CMP.pls.pred, Observed = CMP.test$Yield) %>% as.data.frame() 
Plt_CMP.test.obs_vs_pred <- ggplot(CMP.pls.test.obs_vs_pred, aes(Observed, Predicted)) + 
  geom_point(color=medium_gold) + 
  geom_smooth(method="lm", color=dark_gold, fill=light_gold)+
  labs(title="Test Set: Observed vs. Predicted Values")+ 
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  theme_bw()+
  theme()

# (6.3e) 
CMP.pls.imp <- caret::varImp(CMP.pls.fit, scale=T)

CMP.pls.imp.df <- CMP.pls.imp$importance %>% as.data.frame() %>%
    rownames_to_column("Variable")%>%
    arrange(desc(Overall))
    
Plt_CMP.pls.imp <- CMP.pls.imp.df %>% top_n(15, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + 
    geom_point(colour = medium_gold) + 
    geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall),colour = dark_gold) + 
    labs(title="Variable Importance", subtitle="Top 15 Predictors", x="Variable", y="Scaled Importance")+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  coord_flip()+
    theme_bw()+
    theme(axis.title.y = element_blank())

# (6.3f)
# Scatter Plot Comparison
CMP.varImp.top5 <- CMP.pls.imp.df %>% top_n(5, Overall) 
CMP_DF.gather <- CMP_DF %>% gather(Variable, Value, -Yield) 
Plt_CMP.Scatter <- CMP_DF.gather %>% filter(Variable %in% CMP.varImp.top5$Variable) %>% 
  ggplot(aes(x=Value, y=Yield)) +
  geom_point(color=medium_gold)+
  geom_smooth(method = "lm", color=dark_gold, fill=light_gold)+
  labs(title="Scatter Plots of Top 5 Predictors Against Yield")+
  facet_wrap(~Variable, scales = "free_x", nrow = 1)+
  theme_bw()+
  theme()

# Correlation
CMP_DF.subset <- CMP_DF[(names(CMP_DF) %in% c(CMP.varImp.top5$Variable, "Yield"))]
CMP_DF.corr <-as.data.frame(as.matrix(cor(CMP_DF.subset)))
CMP_DF.corr.tbl <- CMP_DF.corr %>% select(Yield) %>% rownames_to_column('Variable') %>% filter(Variable!="Yield")%>%arrange(desc(Yield))

# ASSIGNMENT 2
# KJ 7.2; KJ 7.5

# instructions from text 
set.seed(200) 
trainingData <- mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x) 
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x) 
#featurePlot(trainingData$x, trainingData$y) 

# created ggplot instead of featurePlot()
Plt_Sim.featurePlot <- trainingData %>% as.data.frame() %>% gather(x, value, -y) %>% mutate(x = str_replace(x, "x.","")) %>% arrange(desc(x)) %>% mutate(x = as.factor(x)) %>% ggplot(aes(value, y)) + geom_point(color=dark_gold)+facet_wrap(~x, nrow=2, scales = "fixed")+theme_bw()+theme()+labs(title="Feature Plot")

# revert seed back to our set group number: 
set.seed(58677)

# (7.2a)

# (7.2b)

# (7.5a)

# (7.5b)

# (7.5c)

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