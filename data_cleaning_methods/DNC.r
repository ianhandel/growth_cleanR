#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a 'null' method of data cleaning: not making
#any changes or modifications to the data

#METHOD NAME: DO NOT CLEAN (DNC)

#LIBRARIES:
library(tidyverse)

#read in data
dat <- read_csv('data/error_simulated_CLOSER_data.csv')

#Save cleaned data
master_DNC <- dat


