library(tidyverse)
library(readxl)
library(readr)
library(MLmetrics)
library(caret)
library(fastDummies)
library(caretEnsemble)
library(caTools)
library(recipes)

# SEEDING
set.seed(58677)

# DATA PREPARATION

## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx') 

## Clean/Standardize Variable Naming Conventions for Both Sets
names(StudentData) <- gsub(" ", "", names(StudentData))
names(StudentEvaluation) <- gsub(" ", "", names(StudentEvaluation))

## Train/Test Splits 
split <- StudentData %>% 
  filter(complete.cases(PH)) %>% 
  mutate(BrandCode=ifelse(is.na(BrandCode), "NA", BrandCode)) %>%
  dummy_cols(remove_selected_columns=T) %>%
  mutate_at(vars(starts_with("Brand")), funs(as.factor(.))) 
sample = sample.split(split$PH, SplitRatio = .8)
train_split = subset(split, sample == TRUE)
test_split  = subset(split, sample == FALSE)

## Apply Mutation to StudentEval
eval <- StudentEvaluation %>% 
  mutate(BrandCode=ifelse(is.na(BrandCode), "NA", BrandCode)) %>%
  dummy_cols(remove_selected_columns=T) %>%
  mutate_at(vars(starts_with("Brand")), funs(as.factor(.)))

## PREPROCESSING 
### Save Methods as Recipe
impute_rec_train <- recipes::recipe(train_split, PH~.) %>%
  recipes::step_bagimpute(all_numeric())

impute_rec_test <- recipes::recipe(test_split, PH~.) %>%
  recipes::step_bagimpute(all_numeric()) 

impute_rec_eval <- recipes::recipe(eval, PH~.) %>%
  recipes::step_bagimpute(all_numeric()) 

### Prep/Bake Recipe to Train/Test/Eval (no extra preproc)
prep_impute = prep(impute_rec_train, training = train_split)

train = bake(prep_impute, train_split)
test = bake(prep_impute, test_split)
eval = bake(prep_impute, eval)

### Add additional transformations
add_preproc <- recipes::recipe(train, PH~.) %>%
  recipes::step_nzv(all_numeric(), options = list(freq_cut = 95/5, unique_cut = 10 )) %>%
  recipes::step_center(all_numeric()) %>%
  recipes::step_scale(all_numeric()) %>% 
  recipes::step_BoxCox(all_numeric())

prep_trans = prep(add_preproc, training = train)

train_trans = bake(prep_trans, train)
test_trans = bake(prep_trans, test)
eval_trans = bake(prep_trans, eval)

saveRDS(train, file = "train.rsd")
saveRDS(test, file = "test.rsd")
saveRDS(eval, file = "eval.rsd")
saveRDS(train_trans, file = "train_trans.rsd")
saveRDS(test_trans, file = "test_trans.rsd")
saveRDS(eval_trans, file = "eval_trans.rsd")

# MODELING
## parameters
tl <- 5  ## tuneLength = number of parameter to be evaluated
trC <- trainControl(method = "cv", ## define resampling method 
                    number = 10, 
                    returnData = T,
                    savePredictions="final")

## Tune Grids 
grid_mars <- expand.grid(degree=1:3, 
                         nprune = seq(5, 50, by = 5))
grid_rf <- expand.grid(.mtry= seq(1, 21, by=3))
grid_svm <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))
grid_cub <- expand.grid(committees = c(5, 10, 50, 75), 
                        neighbors = c(1, 5, 9))

# MODELING (JUST RUNNING STANDARD - ADD preProc if desired)

## Without Additional PreProccessing
fit_mars1 <- train(PH~., data=train, method = 'earth', tuneGrid = grid_mars, trControl=trC, tuneLength=tl, metric="RMSE")
fit_rf1 <- train(PH~., data=train, method="rf", tuneGrid = grid_rf, importance=T, trControl=trC, tuneLength=tl, metric="RMSE")
fit_svm1 <- train(PH~., data=train, method = "svmRadial", tuneGrid = grid_svm, trControl=trC, tuneLength=tl, metric="RMSE")
fit_cub1 <- train(PH~., data=train, method = 'cubist', tuneGrid = grid_cub, trControl=trC, tuneLength=tl, metric="RMSE")


## With Additional PreProccessing
## Without Additional PreProccessing
fit_mars2 <- train(PH~., data=train_trans, method = 'earth', tuneGrid = grid_mars, trControl=trC, tuneLength=tl, metric="RMSE")
fit_rf2 <- train(PH~., data=train_trans, method="rf", tuneGrid = grid_rf, importance=T, trControl=trC, tuneLength=tl, metric="RMSE")
fit_svm2 <- train(PH~., data=train_trans, method = "svmRadial", tuneGrid = grid_svm, trControl=trC, tuneLength=tl, metric="RMSE")
fit_cub2 <- train(PH~., data=train_trans, method = 'cubist', tuneGrid = grid_cub, trControl=trC, tuneLength=tl, metric="RMSE")

## Save Models
saveRDS(fit_mars1, "fit_mars1.rsd")
saveRDS(fit_mars2, "fit_mars2.rsd")
saveRDS(fit_rf1, "fit_rf1.rsd")
saveRDS(fit_rf2, "fit_rf2.rsd")
saveRDS(fit_cub1, "fit_cub1.rsd")
saveRDS(fit_cub2, "fit_cub2.rsd")
saveRDS(fit_svm1, "fit_svm1.rsd")
saveRDS(fit_svm2, "fit_svm2.rsd")