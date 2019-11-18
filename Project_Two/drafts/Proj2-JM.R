library(tidyverse)
library(readxl)
library(psych)
library(ggplot2)
library(mice)
library(xtable)
library(GGally)
library(ggstance)
library(grid)
library(gridExtra)
library(caret)
library(data.table)
library(recipes)
library(Metrics)
library(gbm)

# FORMATTING
source('~/GitHub/CUNY_DATA_624/Project_Two/defaults.R')

# SEEDING
set.seed(58677)

# CUSTOM FUNCTIONS
gather_if <- function(data, FUN, key = "key", value = "value", na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  data %>% {gather(., key = key, value = value , names(.)[sapply(., FUN = FUN)], na.rm = na.rm, convert = convert, factor_key = factor_key )}
} 

flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.table(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut]
  )
}

# DATA EXPLORATION
## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx')
## Data Tidying
names(StudentData) <- gsub(" ", "", names(StudentData))
StudentData <- StudentData %>% 
  #filter(complete.cases(PH)) %>% # drop data with missing PH value %>%
  mutate(BrandCode=as.factor(BrandCode)) # factor categorical variable 
## summary stats
summary_stats <- describe(StudentData)
## Missing Data
MissingData <- StudentData %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t() %>% as.data.frame() %>% 
  rename("n"=V1) %>% 
  rownames_to_column("predictor") %>% 
  arrange(desc(n)) %>% 
  mutate(`%`=round((n/nrow(StudentData)*100),2)) 
## Outiers
outliers <-StudentData %>% 
  mutate(PH=as.factor(PH))%>%
  gather_if(is.numeric, 'key', 'value') %>%
  filter(!is.na(value)) %>% 
  group_by(key)  %>% 
  mutate(outlier_lower = quantile(value, probs=.25, na.rm = T)-1.5 * IQR(value, na.rm = T), 
         outlier_upper = 1.5 * IQR(value, na.rm = T)+quantile(value, probs = .75, na.rm = T), 
         outlier=ifelse(value<outlier_lower, "TRUE", ifelse(value>outlier_upper, "TRUE", "FALSE"))) 

outlier_with <- outliers %>% filter(any(outlier=="TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_wo <- outliers %>% filter(all(outlier!="TRUE")) %>% filter(!is.na(BrandCode)) 
outlier_freq <- outliers %>% select(key, outlier) %>% table() %>% as.data.frame.array() %>% rownames_to_column("variable") %>% arrange(desc(`TRUE`)) %>% mutate(`%`=round(`TRUE`/(`FALSE`+`TRUE`)*100,2)) %>% top_n(5,`%`)

## Correlation
cor <- StudentData %>% select(-PH, -BrandCode) %>% cor(use = "pairwise.complete.obs")
cor_freq <- findCorrelation(cor, .75, names = T)
cor_flat <- flattenCorrMatrix(cor) %>% arrange(row, desc(cor)) %>% filter(cor >.75 | cor < -0.75) 
cor_flat_left <- cor_flat %>% slice(1:10) %>% mutate(id = row_number())
cor_flat_right <- cor_flat %>% slice(11:20) %>% mutate(id = row_number())

# DATA PREPARATION
## Imputation
init <- mice(StudentData, maxit=0); meth <- init$method; predM <- init$predictorMatrix; meth[c("BrandCode")]="polyreg"
imputed <- mice(StudentData, method=meth, predictorMatrix=predM, m=5, printFlag = F, seed = 58677)
BevData <- complete(imputed)
## Train/Test Splits 
trainingRows <- createDataPartition(BevData$PH, p = .80, list= FALSE)
# Split Train/Test Data 
train <- BevData[trainingRows, ]; test <- BevData[-trainingRows, ] 
## Pre-Process Recipe 
rec <- recipes::recipe(train, PH~.)
rec <- rec %>% 
  step_zv(all_predictors(), -BrandCode,  trained = T) %>%
  step_nzv(all_predictors(), -BrandCode,  trained = T, 
           options = list(freq_cut = 95/5, unique_cut = 10 )) %>% 
  step_corr(all_predictors(), -BrandCode, trained = T, threshold = 0.9,
            use = "pairwise.complete.obs", method = "pearson", id = rand_id("corr")) %>%
  step_center(all_predictors(), -BrandCode,trained = T)%>%
  step_scale(all_predictors(), -BrandCode,trained = T)
prep_rec = prep(rec, train=train, fresh=T, verbose = F, retain = T, strings_as_factors=F)
Train_Baked = bake(prep_rec, train)
Test_Baked = bake(prep_rec, test)

# MODELING 
# grids
tl <- 5; trC <- trainControl(method = "cv", number = 10)

# LINEAR MODEL
glm_fit1 <- train(PH~., data=train, method = "glm", preProcess = "pca", trControl = trC, tuneLength=tl)
glm_fit2 <- train(PH~., data=train, method = "glm", preProcess = c("center", "scale", "corr", "zv", "nzv"), trControl = trC, tuneLength=tl)
glm_fit3 <- train(PH~., data=Train_Baked, method = "glm", trControl = trC, tuneLength=tl)
glm_test_pred1 <- predict(glm_fit1, newdata = test) 
glm_test_pred2 <- predict(glm_fit2, newdata = test) 
glm_test_pred3 <- predict(glm_fit3, newdata = Test_Baked) 
glm_perf1 <- postResample(pred = glm_test_pred1, obs = test$PH)
glm_perf2 <- postResample(pred = glm_test_pred2, obs = test$PH)
glm_perf3 <- postResample(pred = glm_test_pred3, obs = Test_Baked$PH)


# NON_LINEAR MODELING
mars_grid <- expand.grid(degree=1:3, nprune = seq(2, 100, length.out = 10) %>% floor())
mars_fit1 <- train(PH~., data=train, method = 'earth', tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
mars_fit2 <- train(PH~., data=train, method = 'earth', preProcess = "BoxCox", tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
mars_fit3 <- train(PH~., data=Train_Baked, method = 'earth', tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
mars_test_pred1 <- predict(mars_fit1, newdata = test) 
mars_test_pred2 <- predict(mars_fit2, newdata = test) 
mars_test_pred3 <- predict(mars_fit3, newdata = Test_Baked) 
mars_perf1 <- postResample(pred = mars_test_pred1, obs = test$PH)
mars_perf2 <- postResample(pred = mars_test_pred2, obs = test$PH)
mars_perf3 <- postResample(pred = mars_test_pred3, obs = Test_Baked$PH)

# TREE BASED MODELING
gbmGrid <- expand.grid(n.trees=c(500, 1000), interaction.depth=c(2,4,6), shrinkage=c(.01, .05), n.minobsinnode=c(1, 2, 3, 6))
gbm_fit1 <- train(PH~., data=train, method = 'gbm',  trControl = trC, tuneGrid=gbmGrid, tuneLength = tl)
gbm_fit2 <- train(PH~., data=train, method = 'gbm',  preProcess = "BoxCox", trControl = trC, tuneGrid=gbmGrid, tuneLength = tl)
gbm_fit3 <- train(PH~., data=Train_Baked, method = 'gbm', trControl = trC, tuneGrid=gbmGrid, tuneLength = tl)
gbm_test_pred1 <- predict(gbm_fit1, newdata = test) 
gbm_test_pred2 <- predict(gbm_fit2, newdata = test) 
gbm_test_pred3 <- predict(gbm_fit3, newdata = Test_Baked) 
gbm_perf1 <- postResample(pred = gbm_test_pred1, obs = test$PH)
gbm_perf2 <- postResample(pred = gbm_test_pred2, obs = test$PH)
gbm_perf3 <- postResample(pred = gbm_test_pred3, obs = Test_Baked$PH)

# ACCURACY METRICS
actual_test<- test$PH; baked_test <- Test_Baked$PH
GLM1<-fortify(glm_fit1$finalModel); GLM2<-fortify(glm_fit2$finalModel); GLM3<-fortify(glm_fit3$finalModel) 
glm_train_mape1 <- Metrics::mape(GLM1$.outcome, GLM1$.fitted); glm_train_mape2 <- Metrics::mape(GLM2$.outcome, GLM2$.fitted); glm_train_mape3 <- Metrics::mape(GLM3$.outcome, GLM3$.fitted); glm_test_mape1 <- Metrics::mape(actual_test, glm_test_pred1); glm_test_mape2 <- Metrics::mape(actual_test, glm_test_pred2); glm_test_mape3 <- Metrics::mape(baked_test, glm_test_pred3);
glm_train_acc1 <- glm_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="glm1_Train") %>% column_to_rownames("Variable") %>% t(); glm_train_acc2 <- glm_fit2$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="glm2_Train") %>% column_to_rownames("Variable") %>% t(); glm_train_acc3 <- glm_fit3$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="glm3_Train") %>% column_to_rownames("Variable") %>% t() 
GLM_PERF <- cbind(glm_train_acc1, glm_train_acc2, glm_train_acc3, "glm1_Test"=glm_perf1, "glm2_Test"=glm_perf2,  "glm3_Test"=glm_perf3)
GLM_MAPE <- cbind(glm_train_mape1, glm_train_mape2, glm_train_mape3, glm_test_mape1, glm_test_mape2,glm_test_mape3); row.names(GLM_MAPE) <- 'MAPE'

mars_train_mape1 <- Metrics::mape(mars_fit1$finalModel$y, mars_fit1$finalModel$fitted.values); mars_train_mape2 <- Metrics::mape(mars_fit2$finalModel$y, mars_fit2$finalModel$fitted.values); mars_train_mape3 <- Metrics::mape(mars_fit3$finalModel$y, mars_fit3$finalModel$fitted.values); mars_test_mape1 <- Metrics::mape(actual_test, mars_test_pred1); mars_test_mape2 <- Metrics::mape(actual_test, mars_test_pred2); mars_test_mape3 <- Metrics::mape(baked_test, mars_test_pred3);
mars_train_acc1 <- mars_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="MARS1_Train") %>% column_to_rownames("Variable") %>% t(); mars_train_acc2 <- mars_fit2$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="MARS2_Train") %>% column_to_rownames("Variable") %>% t(); mars_train_acc3 <- mars_fit3$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="MARS3_Train") %>% column_to_rownames("Variable") %>% t() 
MARS_PERF <- cbind(mars_train_acc1, mars_train_acc2, mars_train_acc3, "MARS1_Test"=mars_perf1, "MARS2_Test"=mars_perf2, "MARS3_Test"=mars_perf3)
MARS_MAPE <- cbind(mars_train_mape1, mars_train_mape2, mars_train_mape3, mars_test_mape1, mars_test_mape2,  mars_test_mape3); row.names(MARS_MAPE) <- 'MAPE'

gbm_train_mape1 <- Metrics::mape(gbm_fit1$trainingData$.outcome, gbm_fit1$finalModel$fit); gbm_train_mape2 <- Metrics::mape(gbm_fit2$trainingData$.outcome, gbm_fit2$finalModel$fit); gbm_train_mape3 <- Metrics::mape(gbm_fit3$trainingData$.outcome, gbm_fit3$finalModel$fit); gbm_test_mape1 <- Metrics::mape(actual_test, gbm_test_pred1); gbm_test_mape2 <- Metrics::mape(actual_test, gbm_test_pred2); gbm_test_mape3 <- Metrics::mape(baked_test, gbm_test_pred3)
gbm_train_acc1 <- gbm_fit1$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="gbm1_Train") %>% column_to_rownames("Variable") %>% t(); gbm_train_acc2 <- gbm_fit2$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="gbm2_Train") %>% column_to_rownames("Variable") %>% t(); gbm_train_acc3 <- gbm_fit3$results %>% as.data.frame() %>% filter(RMSE == min(RMSE)) %>% select(RMSE, Rsquared, MAE) %>% distinct() %>% mutate(Variable="gbm3_Train") %>% column_to_rownames("Variable") %>% t() 
GBM_PERF <- cbind(gbm_train_acc1, gbm_train_acc2, gbm_train_acc3, "gbm1_Test"=gbm_perf1, "gbm2_Test"=gbm_perf2, "gbm3_Test"=gbm_perf3)
GBM_MAPE <- cbind(gbm_train_mape1,gbm_train_mape2,gbm_train_mape3, gbm_test_mape1, gbm_test_mape2,  gbm_test_mape3); row.names(GBM_MAPE) <- 'MAPE'

# save for plots
glm_final_model<- GLM3 

# VARIABLE IMPORTANCE 
mars1_VarImp <- varImp(mars_fit1, scale = T); mars2_VarImp <- varImp(mars_fit2, scale = T); mars3_VarImp <- varImp(mars_fit3, scale = T)
gbm1_VarImp <- varImp(gbm_fit1, scale = T); gbm2_VarImp <- varImp(gbm_fit2, scale = T); gbm3_VarImp <- varImp(gbm_fit3, scale = T)

# TABLES 
Tbl_Top_MissingData <- MissingData %>% top_n(11, n)  %>%  column_to_rownames("predictor")%>%t() %>% kable(caption="Variables with Highest Frequency of NA Values", booktabs = T, digits = 1)%>% kable_styling() %>% row_spec() 
Tbl_Corr <- full_join(cor_flat_left, cor_flat_right, by="id") %>% rename(`|`=id, "V1"=row.x, "V1  "="row.y", "V2"=column.x, "V2 "=column.y, "COR"=cor.x, "COR "=cor.y) %>% mutate(`|`="|")
Tbl_summary_stats <- summary_stats %>% kable(digits = 1, booktabs=T) %>% kable_styling()
Tbl_GLM_ACC <-rbind(GLM_PERF, MAPE) %>%kable(digits=4, booktabs=T, caption = "GLM Accuracy Measures")%>%kable_styling()
Tbl_MARS_ACC <- rbind(MARS_PERF, MARS_MAPE) %>%kable(digits=4,booktabs=T, caption="MARS Accuracy Measures")%>%kable_styling()
Tbl_GBM_ACC <- rbind(GBM_PERF, GBM_MAPE) %>%kable(caption="GBM Accuracy Measures", digits=5, booktabs=T)%>%kable_styling()

# PLOTS
Plot_MissingData <- MissingData %>% filter(`%` > 1) %>% ggplot(aes(x=reorder(predictor, `%`), y=`%`)) + geom_point(alpha = 0.95, shape=22, color="#008ECC", fill="#008ECC")+geom_segment(aes(x=predictor,xend=predictor,y=0,yend=`%`), color="#868b8c", size=.75) + coord_flip() +labs(title="Missing Data Per Variable", subtitle ="N/A Values >1% of All Observations", x="", y="Percentage")+theme_bw()+theme()

Plt_pH_lay <- rbind(c(1,2,2), c(3,4,4))
Plt_pH1 <- StudentData %>% select(PH) %>% mutate(type="pH") %>% ggplot(aes(PH)) + geom_histogram(aes(y=..density..), bins=40, fill="#57A0D3", alpha=.65)+geom_density(alpha=.2, color="#000000",size=.65)+scale_x_continuous() + scale_y_continuous(limits = c(0,3.5)) + labs(x="",y="")+theme_bw()+theme(axis.title.x = element_blank(), axis.ticks.length.x = unit(0, "cm"))+facet_wrap(~type)
Plt_pH2 <- StudentData %>% select(PH) %>% mutate(type="pH") %>% ggplot(aes(PH,"")) + geom_boxploth(fill="#57A0D3", outlier.colour="#4682B4",alpha=.65)+ theme_bw()+theme(legend.title = element_blank(), strip.background = element_blank(), strip.text.x = element_blank(),axis.title.x = element_blank(), axis.ticks.length.x = unit(0, "cm"))+labs(x="",y="")+ scale_y_discrete(labels = 'pH')+ scale_x_continuous() + facet_wrap(~type,nrow=1, strip.position = "top")
Plt_pH3 <- StudentData %>% filter(!is.na(BrandCode)) %>% ggplot(aes(PH,"")) + geom_boxploth(aes(fill = BrandCode),outlier.colour="#4682B4",alpha=.3) + scale_fill_manual()+theme_bw()+theme(legend.position = "none", strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x = element_blank(), axis.text.y = element_blank())+ labs(x="", y="")+ scale_x_continuous() + facet_wrap(~BrandCode, nrow=1, strip.position = "top", scales = 'fixed')
Plt_pH4 <- StudentData %>% select(PH, BrandCode) %>% filter(!is.na(BrandCode)) %>% ggplot(aes(PH)) + geom_histogram(aes(y=..density..,fill=BrandCode), bins=20, alpha=.65)+geom_density(alpha=.2, color="#000000",size=.65)+scale_fill_manual()+scale_x_continuous() + scale_y_continuous(limits = c(0,3.5)) + labs(x="",y="")+facet_wrap(~BrandCode, nrow=1)+theme_bw()+theme(axis.title.x = element_blank(),axis.ticks.length.x = unit(0, "cm"), axis.text.y = element_blank(), legend.position = "none")

Plt_Outlier1 <- ggplot(outlier_with, aes(value)) + geom_density(aes(fill=BrandCode), color="#999999", alpha=.3) + labs(subtitle="With Outliers", x="", y="")+ geom_vline(data = outlier_with, mapping = aes(xintercept=outlier_lower), color="#ff8080")+geom_vline(data = outlier_with, mapping = aes(xintercept=outlier_upper),  color="#ff8080")+facet_wrap(~key, scales = 'free', nrow = 3)+theme_bw()+theme(axis.text.y = element_blank(), axis.title.x=element_blank(),axis.text.x = element_blank(), legend.position = "none")+scale_fill_manual()
Plt_Outlier2 <- ggplot(outlier_wo, aes(value)) + geom_density(aes(fill=BrandCode), color="#999999", alpha=.3)+ labs(subtitle="Without Outliers", x="", y="")+ facet_wrap(~key, scales = 'free', nrow = 1)+ theme_bw()+theme(axis.text.y = element_blank(),axis.title.x=element_blank(),axis.text.x = element_blank(), legend.position = "bottom", legend.key.size = unit(.4, "cm"))+scale_fill_manual()
Plt_Outlier3 <- outlier_with%>%subset(key %in% outlier_freq$variable)%>%ggplot(aes(x=value, y=PH, color=outlier, fill=outlier)) + geom_jitter(alpha=.15) + facet_wrap(~key, scales="free_x", nrow=1) + scale_color_manual(values=c("#bfbfbf","#73C2FB")) + scale_y_discrete(breaks = scales::pretty_breaks(n=10)) + scale_x_continuous(breaks= scales::pretty_breaks(n=4)) + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(size=5), axis.text.y = element_text(size=5))+labs(caption='Response Variables with Highest Frequency of Outliers')

Plt_Corr <- StudentData %>% select_if(is.numeric)%>%  select(-PH)%>% ggcorr(method='pairwise.complete.obs', geom = "tile", label = F, hjust = .95, layout.exp = 7,  label_round =1, low="#95C8D8", mid="grey90",high="#034D92") +  theme_bw()+ theme(legend.key.size = unit(.6, "cm"), legend.justification=c(.05,.98), legend.position=c(.05,.98))

g = ggplot_build(Plt_Corr)
g$data[[2]]$size = 2


# GLM DIAGNOSTICS
# homoscedasticity / linear assumptions
Plt_GLM1<-ggplot(glm_final_model, aes(.fitted, .resid)) + geom_point(color="#999999", alpha=.2) + stat_smooth(method="loess", color="#0F52BA")+geom_hline(yintercept=0, color="#990033", linetype="dashed", size=1) + labs(title = "Residual vs. Fitted", y="Residuals", x="Predicted Values")+theme_bw()+theme()
# normality
Plt_GLM2<-ggplot(glm_final_model, aes(sample = .stdresid)) + stat_qq(color="#999999",alpha=.4)+geom_abline(color = "#0F52BA", size=1) + theme_bw() + theme() + labs(title="Normal Q-Q", x="Theoretical Quantiles", y="Std. Deviance Residuals")
#Homogeneity  scale-location / Standardized Residuals vs. Fitted Values
Plt_GLM3<-ggplot(glm_final_model, aes(x = .fitted, y = sqrt(abs(.stdresid))))+ geom_point(alpha=.2) + stat_smooth(method="loess", na.rm = TRUE, color="#0F52BA") + theme_bw() + theme() + labs(title="Scale-Location", y=expression(sqrt("|Std. Deviance Residuals|")), x="Predicted Values")
Plt_GLM4<-ggplot(glm_final_model, aes(x = .hat, y = .stdresid)) + geom_point(aes(size=.cooksd), na.rm=TRUE, alpha=.2) + stat_smooth(method="loess", na.rm=TRUE) + scale_size_continuous("Cook's Distance", range=c(1,5))+geom_abline(slope=seq(0,0), color="gray", linetype="dashed")+geom_vline(xintercept = 0, color="gray", linetype="dashed") + theme_bw() + theme(legend.position = c(0.725,0.07), legend.justification = c(.5,.5), legend.direction = 'horizontal', legend.key.size = unit(.25, "cm"), legend.box.background = element_rect(color="#999999", size=1), legend.text = element_text(color="#999999")) + labs(title="Residuals vs. Leverages", y="Standard Residuals", x="Leverage")

mars1_plot <- ggplot(mars_fit1)+theme_bw()+theme(legend.position = "none")+labs(title="MARS1")+scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.12,0.155, by=0.005), limits = c(0.12,0.155))
mars2_plot <- ggplot(mars_fit2)+theme_bw()+theme(axis.text.y = element_blank(), legend.position = "none", axis.title.y = element_blank())+labs(title="MARS2")+scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.125,0.155, by=0.005),limits = c(0.12,0.155))
mars3_plot <- ggplot(mars_fit3)+theme_bw()+theme(legend.justification=c(1,1), legend.position = c(.98,.98), axis.title.y = element_blank(), axis.text.y=element_blank(), legend.direction = 'horizontal', legend.key.size = unit(.25, "cm"), legend.box.background = element_rect(color="#999999", size=1), legend.text = element_text(color="#999999"))+labs(title="MARS3")+scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_shape(guide=FALSE)+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.125,0.155, by=0.005),limits = c(0.12,0.155))

gbm1_plot <- ggplot(gbm_fit1)+theme_bw()+theme(strip.text.y = element_blank(), legend.position = "none")+labs(title="GBM1", x="")+scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.09,0.155, by=0.01), limits = c(0.1,0.155))
gbm2_plot <- ggplot(gbm_fit2)+theme_bw()+theme(axis.text.y = element_blank(), legend.position = "none", axis.title.y = element_blank(), strip.text.y = element_blank()) + labs(title="GBM2")  + scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.09,0.155, by=0.01),limits = c(0.1,0.155))
gbm3_plot <- ggplot(gbm_fit3)+theme_bw()+theme(legend.justification=c(1,1), legend.position = c(.98,.98), axis.title.y = element_blank(), axis.text.y=element_blank(), legend.direction = 'horizontal', legend.key.size = unit(.25, "cm"), legend.box.background = element_rect(color="#999999", size=1), legend.text = element_text(color="#999999"))+labs(title="GBM3", x="")+scale_color_manual(values=c("#95C8D8", "#008081", "#034D92"))+scale_shape(guide=FALSE)+scale_y_continuous(labels = scales::number_format(accuracy = .001, scale=1, decimal.mark = '.'), breaks = seq(0.1,0.145, by=0.01),limits = c(0.09,0.155))

Plt_MARS_VarImp1 <- mars1_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="MARS1",  caption = "")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8)); Plt_MARS_VarImp2 <- mars2_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="MARS2", caption = "")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8)); Plt_MARS_VarImp3 <- mars3_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="MARS3", caption="Top 10 Predictor Variables")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8))
Plt_GBM_VarImp1 <- gbm1_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="GBM1",  caption = "")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8)); Plt_GBM_VarImp2 <- gbm2_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="GBM2", caption = "")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8)); Plt_GBM_VarImp3 <- gbm3_VarImp$importance %>% as.data.frame.array() %>% rownames_to_column("Variable") %>% top_n(10, Overall) %>% ggplot(aes(x=reorder(Variable, Overall), y=Overall)) + geom_point()+geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Overall)) + coord_flip() + labs(y="Overall", x="", title="GBM3", caption="Top 10 Predictor Variables")+scale_y_continuous(labels = scales::number_format(accuracy = 1,decimal.mark = '.'))+theme_bw()+theme(plot.caption = element_text(size = 8))



#glm %>% gather(key = "predictor", value = "x", -PH, -predicted, -residuals) %>%  ggplot(aes(x = x, y = PH)) +  geom_segment(aes(xend = x, yend = predicted), alpha = .2) + geom_point(aes(color = residuals)) + scale_color_gradient2(low = "blue", mid = "white", high = "red") + guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + facet_wrap(~ predictor, scales = "free_x", nrow=4) +  theme_bw() +theme(axis.ticks.x = element_blank())
