#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using a non-linear regression model for growth to identify outliers

#METHOD NAME: Non-linear regression model (NLR)

#LIBRARIES:
library(evaluate)

#Run the GCO method on the data first, to obtain a cleaner subset of the data
#on which to run the model
  replay(evaluate(file('data_cleaning_methods/GCO.r')))
  sub_dat <- master_GCO
  dat <- master_DNC


#Formula for model        
  f1 <- new_weight ~ (Asym*exp(-exp(((growth_rate*exp(1))/Asym)*(lag_time-age)+1)))

#set up starting values for regression models based on the original data without simulated errors
#starting values for these models were defined by externally published values

  orig_dat <- read_csv('data/reformatted_CLOSER_data.csv')
  
  females0 <- subset(orig_dat, orig_dat$sex == 'Female')
  fem_mod0 <- nls(f1,
                  data = females0,
                  start = c(Asym = 70.2, lag_time = 0, growth_rate = 0.1))
  
  fem_start <- fem_mod0$m$getPars()
  
  males0 <- subset(orig_dat, orig_dat$sex == 'Male')
  mal_mod0 <- nls(f1,
                  data = males0,
                  start = c(Asym = 83.6, lag_time = 0, growth_rate = 0.1))
  
  mal_start <- mal_mod0$m$getPars()

#using these starting values, fit non-linear regression model to each sex
#of the subset of data (with duplicates and general cut off outliers removed)

  females <- subset(sub_dat, sub_dat$sex == 'Female')
  fem_mod <- nls(f1,
                 data = females,
                 start = c(Asym = fem_start[[1]], lag_time = fem_start[[2]],
                           growth_rate = fem_start[[3]]))
  
  fem_mod_pars <- fem_mod$m$getPars()  
    
  males <- subset(sub_dat, sub_dat$sex == 'Male')
  mal_mod <- nls(f1,
                 data = males,
                 start = c(Asym = mal_start[[1]], lag_time = mal_start[[2]], 
                           growth_rate = mal_start[[3]]))
  
  mal_mod_pars <- mal_mod$m$getPars()
  
  
#Apply these models to the uncleaned data and calculate prediction intervals that are 4
#times the standard deviation from the predicted measurement
  females2 <- subset(dat, dat$sex == 'Female')
  
  females2 <- females2 %>%
    mutate(NLR_pred=predict(fem_mod, newdata = females2),
           sd_pred = sd(NLR_pred),
           upper_int = NLR_pred + (4*(sd(NLR_pred))),
           lower_int = NLR_pred - (4*(sd(NLR_pred))))      
  
  males2 <- subset(dat, dat$sex == 'Male')
  
  males2 <- males2 %>%
    mutate(NLR_pred=predict(mal_mod, newdata = males2),
           sd_pred = sd(NLR_pred),
           upper_int = NLR_pred + (4*(sd(NLR_pred))),
           lower_int = NLR_pred - (4*(sd(NLR_pred)))) 
  
  dat <- rbind(males2, females2)
  
  #Function to find outliers
  get_outliers <- function(X, var1 = "new_weight", upper_limit = "upper_int", 
                           lower_limit = "lower_int", outlier_name = "outlier", print_results = TRUE) {
    X[[paste(var1, "_", outlier_name, sep = "")]] <- X[[var1]] > X[[upper_limit]] | X[[var1]] < X[[lower_limit]]
    if(print_results == TRUE) {
      print(paste(var1, " ", outlier_name, "s", sep = ""))
      print(sum(X[[paste(var1, "_", outlier_name, sep = "")]]), na.rm = TRUE)
    }
    return(X)
  }
  
  dat <- get_outliers(dat)
  
#Apply step 1 of algorithm to delete complete duplicates, keeping just the oldest
#duplicate in the set of data entries  
  
  dat <- get_duplications(dat)
  
  dat <- dat %>%
    group_by(dups_ID) %>%
    mutate(observation = row_number(),
           last_obs_num = tail(observation, 1),
           first_obs_num = head(observation, 1),
           last_observation = last_obs_num == observation,
           first_observation = first_obs_num == observation) %>%
    ungroup()
  
  dat <- dat %>%
    filter(complete_duplications == FALSE | complete_duplications == TRUE & first_observation == TRUE)
  
  #check the difference in duplications
  dat <- get_duplications(dat)
 
#Apply step 2 of algorithm to delete duplicates that are not complete duplicates
#by keeping only the duplicate that is closest to the predicted measurement    

#Work out which of the remaining duplicates are closest to the predicted measurements   
  dat <- dat %>%
    mutate(abs_diff_from_pred = abs(new_weight - NLR_pred)) %>%
    group_by(dups_ID) %>%
    mutate(min_diff = min(abs_diff_from_pred)) %>%
    ungroup() %>%
    mutate(smallest_diff_from_pred = case_when(min_diff == abs_diff_from_pred ~ TRUE,
                                               min_diff != abs_diff_from_pred ~ FALSE))
  
  dat <- dat %>%
    filter(duplications == FALSE | duplications == TRUE & smallest_diff_from_pred == TRUE)
  
  #check the difference in duplications and outliers
  dat <- get_duplications(dat)
  dat <- get_outliers(dat)
  
#cut out remaining outliers identified by the prediction intervals to clean the data
  master_NLR <- dat %>%
    filter(new_weight_outlier == FALSE)

