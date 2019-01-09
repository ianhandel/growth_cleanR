#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an internally defined cut-off (cross-sectional z-score) to remove outliers

#METHOD NAME: Standard z-score cut-off (SZCO)

#LIBRARIES:
library(evaluate)

#Run the RD method on the data first
replay(evaluate(file('data_cleaning_methods/RD.r')))
dat <- master_RD

#define the z-scores for weight that are used to identify outliers
#The cut off for z-score is given as more than 3 or less than -3
  dat <- dat %>% 
    mutate(pop_mean = mean(weight),
           pop_sd = sd(weight),
           z_score = abs((weight - pop_mean) / pop_sd),
           z_score_outlier = z_score > 3)
  
#cut out outliers to clean the data
  master_SZCO <- dat %>%
    filter(z_score_outlier == FALSE)

