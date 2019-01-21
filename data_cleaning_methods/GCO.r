#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using an externally defined cut-off to remove outliers

#METHOD NAME: General cut-off (GCO)

#LIBRARIES:
library(evaluate)

#Run the RD method on the data first
if(exists('master_RD') == FALSE) {
replay(evaluate(file('/Users/s1576473/dev/growth_cleanR/data_cleaning_methods/RD.R')))
}

master_GCO <- master_RD

#define the externally defined cut-offs for weight that are used to identify outliers
  master_GCO$cut_outlier <- (master_GCO$new_weight < 0.5) | 
    ((master_GCO$new_weight < 10) & (master_GCO$age >= 1825)) | 
    (master_GCO$new_weight > 250)

#remove values defined as outliers  
  master_GCO <- master_GCO %>%
    filter(cut_outlier == FALSE)

