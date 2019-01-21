#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a 'null' method of data cleaning: not making
#any changes or modifications to the data

#METHOD NAME: DO NOT CLEAN (DNC)

#LIBRARIES:
library(tidyverse)
library(evaluate)

#read in data

if(exists('dat') == FALSE) {
  replay(evaluate(file('/Users/s1576473/dev/growth_cleanR/data_prep_and_error_simulation/simulate_errors_CLOSER_data.R')))
}

#Save cleaned data
master_DNC <- dat


