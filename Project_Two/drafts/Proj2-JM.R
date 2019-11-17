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
  filter(complete.cases(PH)) %>% # drop data with missing PH value %>%
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
outliers <- StudentData %>% select(-BrandCode) %>%
  gather() %>% 
  filter(!is.na(value)) %>% 
  group_by(key) %>% 
  mutate(outlier_lower = quantile(value, probs=.25)-1.5 * IQR(value, na.rm = T), 
         outlier_upper = 1.5 * IQR(value, na.rm = T)+quantile(value, probs = .75), 
         outlier=ifelse(value<outlier_lower, "TRUE", ifelse(value>outlier_upper, "TRUE", "FALSE"))) 
outlier_proportions <- outliers %>% select(key, outlier) %>% table() %>% as.data.frame.array() %>% rownames_to_column("variable") %>% arrange(desc(`TRUE`)) %>% mutate(`%`=round(`TRUE`/(`FALSE`+`TRUE`)*100,2)) 
outlier_ph <-StudentData %>% 
  mutate(PH=as.factor(PH))%>%
  gather_if(is.numeric, 'key', 'value') %>%
  group_by(key)  %>% 
  mutate(outlier_lower = quantile(value, probs=.25, na.rm = T)-1.5 * IQR(value, na.rm = T), 
         outlier_upper = 1.5 * IQR(value, na.rm = T)+quantile(value, probs = .75, na.rm = T), 
         outlier=ifelse(value<outlier_lower, "TRUE", ifelse(value>outlier_upper, "TRUE", "FALSE"))) %>%
  filter(any(outlier=="TRUE"))
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
  step_nzv(all_predictors(), trained = T, 
           options = list(freq_cut = 95/5, unique_cut = 10 )) %>% 
  step_corr(all_numeric(), trained = T, threshold = 0.9,
            use = "pairwise.complete.obs", method = "pearson", id = rand_id("corr"))
prep_rec = prep(rec, train=train, fresh=T, verbose = F, retain = T, strings_as_factors=F)
Train_Baked = bake(prep_rec, train)

# MODELING 
# grids
tl <- 5; trC <- trainControl(method = "cv", number = 10)
mars_grid <- expand.grid(degree=1:3, nprune = seq(2, 100, length.out = 10) %>% floor())
# train
mars_fit <- train(PH~., data=train, method = 'earth', tuneGrid = mars_grid, trControl = trC, tuneLength=tl)
# test 
mars_pred <- predict(mars_fit, newdata = test) 
mars_perf <- postResample(pred = mars_pred, obs = test$PH)



#metrics: MAPE
actual <- train$PH
mars_pred <- mars_fit$finalModel$fitted.values
Metrics::mape(actual, mars_pred)

# Tables 
Tbl_Top_MissingData <- MissingData %>% top_n(10, n)  %>%  column_to_rownames("predictor")%>%t() 
Tbl_Corr <- full_join(cor_flat_left, cor_flat_right, by="id") %>% rename(`|`=id, "V1"=row.x, "V1  "="row.y", "V2"=column.x, "V2 "=column.y, "COR"=cor.x, "COR "=cor.y) %>% mutate(`|`="|")

# PLOTS
Plot_MissingData <- MissingData %>% 
  filter(`%` > 1) %>% 
  ggplot(aes(x=reorder(predictor, `%`), y=`%`)) + 
  geom_point(alpha = 0.95, shape=22, color="#008ECC", fill="#008ECC")+
  geom_segment(aes(x=predictor,xend=predictor,y=0,yend=`%`), color="#868b8c", size=.75) + 
  coord_flip() + 
  labs(title="Missing Data Per Variable", subtitle ="N/A Values >1% of All Observations", x="", y="Percentage")+
  theme_bw()+
  theme()

Plt_pH1 <- ggplot(StudentData, aes(PH)) + 
  geom_histogram(bins=50, fill="#4682B4", color="#4682B4", alpha=.75)+ 
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1,decimal.mark = '.')) + 
  labs( x="",y="")+theme_bw()+theme()
Plt_pH2 <- ggplot(StudentData, aes("",PH)) + 
  geom_boxplot(color="#999999", fill="#999999", outlier.colour="#4682B4",alpha=.1)+ 
  scale_x_discrete(labels="pH")+
  theme_bw()+
  theme(legend.key.size = unit(.5,"cm"), legend.title = element_blank())+labs(x="",y="")+
  scale_y_continuous(breaks = round(seq(min(BevData$PH), max(BevData$PH), by = 0.3),1)) 
Plt_pH3 <- StudentData %>% filter(!is.na(BrandCode)) %>% ggplot(aes(BrandCode,PH)) + 
  geom_boxplot(aes(fill = BrandCode),outlier.colour="#4682B4",alpha=.3) + 
  scale_fill_manual(values=c("#999999", "#95C8D8", "#4682B4", "#034D92"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="",y="")+ 
  scale_y_continuous(breaks = round(seq(min(BevData$PH), max(BevData$PH), by = 0.3),1)) 



Plt_Outlier1 <- outlier_ph %>%
  filter(any(outlier=="TRUE"))%>%
  ggplot(aes(x = value, y= key)) + 
  geom_boxploth(color="#999999", fill="#999999", outlier.colour="#034D92",alpha=.1) + 
  labs(subtitle="With Outliers", x="", y="")+ 
  facet_wrap(~key, scales = 'free', nrow = 3)+ 
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=5))
scale_x_continuous(labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'),
                   breaks= scales::pretty_breaks(n=1))

Plt_Outlier2 <- outliers%>% filter(key!="PH") %>% 
  filter(all(outlier!="TRUE"))%>% ggplot(aes(x = value, y= key)) + 
  geom_boxploth(color="#999999", fill="#999999", outlier.colour="#4682B4",alpha=.1) + 
  labs(subtitle="Without Outliers", x="", y="")+ facet_wrap(~key, scales = 'free', nrow = 1)+ theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=5)) 
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'), 
                     breaks= scales::pretty_breaks(n=2))


Plt_Outlier3 <- outlier_ph %>%
  filter(!is.na(outlier))%>%
  ggplot(aes(x=value, y=PH, color=outlier, fill=outlier))+
  geom_jitter(alpha=.25)+
  facet_wrap(~key, scales="free_x", nrow=3)+ 
  scale_color_manual(values=c("grey","#034D92"))+
  scale_y_discrete(breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(breaks= scales::pretty_breaks(n=2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=5),
        axis.text.y = element_text(size=5))+
  labs(caption="pH~Predictor Scatterplots")

Plt_Corr <- StudentData %>% 
  select_if(is.numeric)%>% 
  select(-PH)%>%
  ggcorr(method='pairwise.complete.obs', geom = "tile", label = F, hjust = .95, layout.exp = 7,  label_round =1, low="#95C8D8", mid="grey90",high="#034D92") + 
  theme_bw()+
  theme(legend.key.size = unit(.6, "cm"), legend.justification=c(.05,.98), legend.position=c(.05,.98))

g = ggplot_build(Plt_Corr)
g$data[[2]]$size = 2

mars_plot <- ggplot(mars_fit)+theme_bw()+theme()+labs(title="MARS Cross-Validated RMSE Profile")
