#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an externally defined cut-off to remove outliers

#METHOD NAME: General cut-off (GCO)

#LIBRARIES:
library(evaluate)

#Run the RD method on the data first
replay(evaluate(file('data_cleaning_methods/RD.r')))
dat <- master_RD

#define the externally defined cut-offs for weight that are used to identify outliers
  dat$cut_outlier <- (dat$new_weight < 0.5) | 
    ((dat$new_weight < 10) & (dat$age >= 1825)) | 
    (dat$new_weight > 250)

#remove values defined as outliers  
  master_GCO <- dat %>%
    filter(cut_outlier == FALSE)

