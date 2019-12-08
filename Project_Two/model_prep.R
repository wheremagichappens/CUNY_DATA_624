library(tidyverse)
library(readxl)
library(readr)
library(MLmetrics)
library(caret)
library(fastDummies)
library(caretEnsemble)
library(caTools)

# SEEDING
set.seed(58677)

# DATA PREPARATION

## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx') %>% 
  dplyr::select(-PH)

## Train/Test Splits 
sample = sample.split(StudentData$PH, SplitRatio = .8)
train_split = subset(StudentData, sample == TRUE)
test_split  = subset(StudentData, sample == FALSE)

## PREPROCESSING 

### Apply prep function to all datasets 
proj.prep <- function(df, transform = F){
  # Clean Variable Names
  names(df) <- gsub(" ", "", names(df))
  df <- mutate(df, BrandCode=as.factor(BrandCode))
  # Build Dummy Columns 
  dummy <- dummy_cols(df)
  if ("BrandCode_NA" %in% names(dummy) == TRUE){
    dummy <- dummy %>% dplyr::select(-BrandCode, -BrandCode_NA)
  } else {
    dummy <- dummy %>% dplyr::select(-BrandCode)
  }
  # Impute with bagImpute
  imp <- preProcess(dummy, method = 'bagImpute')
  imp <- as.data.frame(predict(imp, dummy))
  # Tranformations
  trans <- preProcess(imp, method = c('center', 'scale', 'nzv', 'BoxCox'))
  trans <- as.data.frame(predict(trans, imp))
  if (!transform) return(trans)
  if (transform) return(imp)
}
 
### store TTSet without additional preprocessing
train <- proj.prep(train_split, F) 
test <-  proj.prep(train_split, F) 
eval <- proj.prep(StudentEvaluation, F) 

### store TTSet with preprocessing = c('center', 'scale', 'nzv', 'BoxCox')
train_trans <- proj.prep(train_split, T) 
test_trans <-  proj.prep(train_split, T)
eval_trans <- proj.prep(StudentEvaluation, T)



# MODELING
## parameters
tl <- 5  ## tuneLength = number of parameter to be evaluated
trC <- trainControl(method = "cv", ## define resampling method 
                    number = 10, 
                    returnData = T,
                    savePredictions="final")

## Tune Grids 
grid_mars <- expand.grid(degree=1:3, 
                         nprune = seq(5, 50, by = 10))
grid_rf <- expand.grid(.mtry=c(3:6))
grid_svm <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))
grid_cub <- expand.grid(committees = c(5, 10, 50), 
                        neighbors = c(1, 5, 9))

# MODELING (JUST RUNNING STANDARD - ADD preProc if desired)

## Without Additional PreProccessing
fit_models1 <- caretList(
  PH~., 
  data=train,
  metric="RMSE",
  tuneLength=tl, 
  trControl=trC,
  tuneList=list(
    mars=caretModelSpec(method = 'earth', tuneGrid = grid_mars),
    rf=caretModelSpec(method="rf", tuneGrid = grid_rf),
    svm=caretModelSpec(method = "svmRadial", tuneGrid = grid_svm),
    cub=caretModelSpec(method = 'cubist', tuneGrid = grid_cub)
  )
)

## With Additional PreProccessing
fit_models2 <- caretList(
  PH~., 
  data=train_trans, 
  tuneLength=tl, 
  trControl=trC,
  tuneList=list(
    mars=caretModelSpec(method = 'earth', tuneGrid = grid_mars),
    rf=caretModelSpec(method="rf", tuneGrid = grid_rf),
    svm=caretModelSpec(method = "svmRadial", tuneGrid = grid_svm),
    cub=caretModelSpec(method = 'cubist', tuneGrid = grid_cub)
  )
)
save.image(file = "model_prep.RData")
