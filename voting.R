
require(readxl)
require(tidyverse)
selections <- read_excel('/Users/bethany/Documents/Data-624/CUNY_DATA_624/Project Two Model Types (Responses).xlsx')

selections%>%
  separate_rows(`Pick Five Model Types`, sep =',')%>%
  group_by(`Pick Five Model Types`)%>%
  summarise(count =n())%>%
  arrange(desc(count))%>%knitr::kable()
