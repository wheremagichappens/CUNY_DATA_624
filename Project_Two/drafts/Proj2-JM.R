library(tidyverse)
library(readxl)
library(psych)
library(ggplot2)
library(mice)

# FORMATTING
source('~/GitHub/CUNY_DATA_624/Project_Two/defaults.R')

# DATA EXPLORATION
## Import data
StudentData <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentData.xlsx')
StudentEvaluation <- read_xlsx('~/GitHub/CUNY_DATA_624/Project_Two/data/StudentEvaluation.xlsx')
## Data Tidying
names(StudentData) <- gsub(" ", "_", names(StudentData))
StudentData <- StudentData %>% 
  filter(complete.cases(PH)) %>% # drop data with missing PH value %>%
  mutate(Brand_Code=as.factor(Brand_Code)) # factor categorical variable 
## Missing Data
MissingData <- StudentData %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t() %>% as.data.frame() %>% 
  rename("n"=V1) %>% 
  rownames_to_column("predictor") %>% 
  arrange(desc(n)) %>% 
  mutate(`%`=round((n/nrow(StudentData)*100),2)) 
## Imputation
init <- mice(StudentData, maxit=0); meth <- init$method; predM <- init$predictorMatrix; meth[c("Brand_Code")]="polyreg"
imputed <- mice(StudentData, method=meth, predictorMatrix=predM, m=5, printFlag = F)
BevData <- complete(imputed)

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