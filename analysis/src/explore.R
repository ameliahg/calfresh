### explore.R ###
### exploring CalFresh data
### AHG
### draft of 2022-08-26

### setup
stopifnot(basename(getwd())=='analysis')
# ^ checking to make sure we're running from the main task directory

library(tidyverse)
library(stargazer)
library(viridisLite)

### functions
catt <- function(dset,c1name,c2name,outfile,isfirst){
  # c1 is the column we're looking for differences in
  # c2 is the column that's splitting the population
  cat(c1name,"\n",
      file=outfile,append=ifelse(isfirst=="yes",FALSE,TRUE))
  t <- t.test(dset[[c1name]]~dset[[c2name]])
  capture.output(t,file=outfile,append=TRUE)
  cat("\n",file=outfile,append=TRUE)
}

### data

# income, etc., by zip code
# these don't actually vary enough in our sample to make a difference
# but would probably add value in a more diverse sample

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

# ideally, we'd also have race, gender, and education data
# at person and zip level
# i'd particularly like zip code level race
# to look at effects of segregation

cf <- read_csv('input/exercise_data.csv') %>% 
  mutate(app_id=as.character(app_id),
         zip=as.character(zip),
         under5mins=ifelse(completion_time_mins<5,1,0),
         over20mins=ifelse(completion_time_mins>20,1,0),
         time_diff_frommedian=median(completion_time_mins)-completion_time_mins,
         logtime=log1p(completion_time_mins),
         total_docs=docs_with_app+docs_after_app,
         any_docs=ifelse(total_docs>0,1,0),
         pct_docs_with_app=docs_with_app/total_docs,
         unanswered=ifelse(is.na(had_interview),1,0),
         total_caringfor=under18_n+over_59_n,
         kid_proportion=round(under18_n/household_size),
         kids=ifelse(kid_proportion>0,1,0),
         income_percap=income/household_size,
         income_perkid=ifelse(under18_n==0,NA,income/under18_n),
         income_caredfor=ifelse(total_caringfor==0,NA,
                                income/total_caringfor)) %>%
  left_join(.,irs) %>%
  filter(completion_time_mins<=100) # all but 5 or so people

# a bunch of t-tests to see where people who were approved differed

catt(cf,'completion_time_mins','approved',
     'output/cf_basic_data.txt',"yes")

check_diffs <- c('docs_after_app','docs_with_app','time_diff_frommedian',
                 'household_size','income','had_interview',
                 'pct_docs_with_app','total_caringfor',
                 'kid_proportion', 'under5mins','over20mins',
                 'stable_housing','total_docs','unanswered',
                 'under18_n','over_59_n','income_caredfor',
                 'income_percap','income_perkid','kids','any_docs')

for (cd in check_diffs) {
  catt(cf,cd,'approved','output/cf_basic_data.txt',"no")
}

### visualize distributions for continuous  & count vars

for (cd in check_diffs) {
  l <- length(setdiff(unique(cf[[cd]]),NA))
  xupper <- quantile(cf[[cd]],0.975,na.rm=TRUE)
  p1 <- ggplot(data=cf,aes_string(x=cd,color='approved',fill='approved'))
  if (l>19) {
    p2 <- p1 + geom_boxplot(alpha=0.4) + xlim(0,xupper) +
      scale_color_viridis_d() + scale_fill_viridis_d()
    ggsave(plot=p2,filename=paste('output/',cd,'_box.png',sep=''))
  }
  else {
    p2 <- p1 + geom_bar(alpha=0.4,position='dodge')+
      scale_color_viridis_d() + scale_fill_viridis_d()
    ggsave(plot=p2,filename=paste('output/',cd,'_bar.png',sep=''))
  }
}

### modeling

# ok if i were feeling fancy i would do a two-stage logistic regression,
# but time is limited here and the following has the virtue of simplicity.

# start with submitting documents, which seems important to getting approved
m1 <- glm(any_docs~household_size+income_percap+kids+stable_housing+
            over20mins,
          family='binomial',data=cf)

m2 <- glm(approved~any_docs+household_size+income_percap+kids+stable_housing+
            over20mins,
          family='binomial',data=cf)

m3 <- glm(approved~any_docs+household_size+income_percap+kids+stable_housing+
            over20mins+had_interview,
          family='binomial',data=cf)

m4 <- glm(approved~any_docs+household_size+income_percap+kids+stable_housing+
            over20mins+unanswered,
          family='binomial',data=cf)

m5 <- glm(approved~any_docs+household_size+income_percap+kids+stable_housing+
            over20mins+unanswered+factor(zip),
          family='binomial',data=cf)

capture.output(stargazer(m1,m2,m3,m4,m5,
          type='html',
          omit='*zip*',
          add.lines=list(c('ZIP fixed effects','No','No','No','No','Yes')),
          out='output/main_logit_models.html'))

### subpopulations

# applicants with kids
m6 <- glm(approved~any_docs+household_size+income_percap+stable_housing+
            over20mins+unanswered,
          family='binomial',data=filter(cf,kids==1))

m6b <- glm(approved~any_docs+household_size+income_percap+stable_housing+
            over20mins+unanswered+factor(zip),
          family='binomial',data=filter(cf,kids==1))

# applicants with lowest income
m7 <- glm(approved~any_docs+household_size+kids+stable_housing+
            over20mins+unanswered,
          family='binomial',data=filter(cf,income<=15)) # bottom 3 deciles
m7b <- glm(approved~any_docs+household_size+kids+stable_housing+
            over20mins+unanswered+factor(zip),
          family='binomial',data=filter(cf,income<=15)) #bottom 3 deciles

# applicants with highest income
m7c <- glm(approved~any_docs+household_size+kids+stable_housing+
            over20mins+unanswered,
          family='binomial',data=filter(cf,income>=1424)) # top 3 deciles
m7d <- glm(approved~any_docs+household_size+kids+stable_housing+
             over20mins+unanswered+factor(zip),
           family='binomial',data=filter(cf,income>=1424)) # top 3 deciles

# applicants who submitted documents
m8 <- glm(approved~household_size+income_percap+kids+stable_housing+
            over20mins+unanswered,
          family='binomial',data=filter(cf,any_docs==1))
m8b <- glm(approved~household_size+income_percap+kids+stable_housing+
            over20mins+unanswered+factor(zip),
          family='binomial',data=filter(cf,any_docs==1))

# applicants with stable housing
m9 <- glm(approved~any_docs+household_size+income_percap+kids+
            over20mins+unanswered,
          family='binomial',data=filter(cf,stable_housing==1))
m9b <- glm(approved~any_docs+household_size+income_percap+kids+
            over20mins+unanswered+factor(zip),
          family='binomial',data=filter(cf,stable_housing==1))


capture.output(stargazer(m8,m7,m7c,m6,m9,
          type='html',
          column.labels = c('Documents','Lowest income',
                            'Highest income','Parents','Stable housing'),
          out='output/logit_models_subpops.html'))

capture.output(stargazer(m8b,m7b,m7d,m6b,m9b,
          type='html',
          column.labels = c('Documents','Lowest income',
                            'Highest income','Parents','Stable housing'),
          omit='*zip*',
          add.lines=list(c('ZIP fixed effects','Yes','Yes','Yes','Yes','Yes')),
          
          out='output/logit_models_subpops_fe.html'))

capture.output(stargazer(m8b,m7b,m7d,m6b,m9b,
                         type='html',
                         column.labels = c('Documents','Lowest income',
                                           'Highest income','Parents','Stable housing'),
                         
                         out='output/logit_models_subpops_fe_all.html'))

