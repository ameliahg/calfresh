### explore.R ###
### basic descriptives and other fun stuff
### for CalFresh data
### AHG
### draft of 2022-08-26

stopifnot(basename(getwd())=='analysis')

library(tidyverse)
library(RCurl)

data_url <- "https://raw.githubusercontent.com/ameliahg/calfresh/main/analysis/input/exercise_data.csv?token=GHSAT0AAAAAABXKMY7XQADJJ2GBEAYEZT36YYI7YSA"

cf <- read_csv(data_url) %>% 
  mutate(app_id=as.character(app_id),
         zip=as.character(zip))
  
stargazer(as.data.frame(cf),
          summary=TRUE,type="html",
          out="output/calfresh_data.html")