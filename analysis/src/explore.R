### explore.R ###
### exploring CalFresh data
### AHG
### draft of 2022-08-26

stopifnot(basename(getwd())=='analysis')

 # ^ checking to make sure we're running from the main task directory

library(tidyverse)
library(RCurl)

irs <- read_csv('input/irs_data_CA.csv') %>% # downloaded from IRS website
  filter(zipcode!=0) %>%
  group_by(zipcode) %>%
  mutate(wts=N1/sum(N1,na.rm=TRUE)) %>%
  summarize(returns=sum(N1,na.rm=TRUE), # proxy for households
            pop=sum(N2,na.rm=TRUE), # sum of individuals on returns
            mean_hh_inc_cat=weighted.mean(agi_stub,wts,na.rm=TRUE), 
            # mean of truncated categories is OK proxy for median
            mean_hh_inc=weighted.mean(A02650,wts,na.rm=TRUE),
            mortgages=sum(N19300,na.rm=TRUE),
            n_hh_under25k=N1[agi_stub==1],
            n_hh_over200k=N1[agi_stub==6]) %>%
  mutate(zip=as.character(zipcode),
         pct_hh_under25k=n_hh_under25k/returns,
         pct_hh_over200k=n_hh_over200k/returns,
         pct_with_mortgage=mortgages/returns) %>%
  ungroup()

cf <- read_csv('input/exercise_data.csv') %>% 
  mutate(app_id=as.character(app_id),
         zip=as.character(zip)) %>%
  left_join(.,irs)

