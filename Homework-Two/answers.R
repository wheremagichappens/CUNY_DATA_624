## MERGE HW QUESTIONS HERE:

## Ensure constistent variable naming convention per question
## Look out for duplicate variable names between questions

# DEPENDENCIES

# Predicitve Modeling
libraries('AppliedPredictiveModeling', 'mice','caret', 'tidyverse','impute','pls','caTools')
# Formatting Libraries
libraries('default', 'knitr', 'kableExtra')
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

train =subset(df_final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE

test=subset(df_final, sample==FALSE)

#code
pls_model <- plsr(Yield~., data=train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE)


pls_model2 <- plsr(Yield~., data=train,
            method = 'kernelpls',
            scale = TRUE,
            center = TRUE,
            ncomp =41)


#  Train Metrics
train_eval=data.frame('obs' = train$Yield, 'pred' =pls_model$fitted.values)
colnames(train_eval) <- c('obs', 'pred')

# (6.3d)
#code
# #Test Predictions & Metrics
pls2_pred<- predict(pls_model2, test, ncomp=41)

pls2test_eval=data.frame('obs' = test$Yield, 'pred' =pls2_pred)

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