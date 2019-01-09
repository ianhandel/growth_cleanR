#AUTHOR: CHARLOTTE WOOLLEY

#DESCRIPTION: This script details a method of data cleaning: Removing duplicate data entries
#and using a non-linear mixed effects model for growth and a 5 step algorithm to identify outliers

#METHOD NAME: Non-linear mixed effects model with algorithm (NLR-A)

#LIBRARIES:
  library(evaluate)
  library(Rcpp)
  
#Run the NLME method on the data first to perform the first 2 steps of the algorithm
#(just removing the duplications)
  replay(evaluate(file('data_cleaning_methods/NLME.r')))
  
#Apply step 3 of algorithm to identify the cause of possible errors in dataset
  
#Function to transpose numbers  
  cppFunction('IntegerVector Reverse_CPP2(IntegerVector x) {
              int n = x.size();
              IntegerVector out(n);
              IntegerVector xx = clone(x);
              
              for (int i = 0; i < n; ++i){
              int reverse = 0;
              while(xx[i] != 0) {
              int remainder = xx[i]%10;
              reverse = reverse*10 + remainder;
              xx[i]/= 10;
              }
              out[i] = reverse;
              }
              
              return out;
              
              }')
   
#list of numbers that should not be transposed due to their difference with the
#original number being too small (9 in this case)
  bad_numbers <- c(12,21,23,32,34,43,45,54,56,65,67,76,78,87,89,98)
  
  dat <- dat %>%
    mutate(div10_weights = new_weight/10,
           mul10_weights = new_weight*10,
           div100_weights = new_weight/100,
           mul100_weights = new_weight*100,
           div1000_weights = new_weight/1000,
           mul1000_weights = new_weight*1000,
           min100_weights = new_weight - 100, 
           min1000_weights = new_weight - 1000, 
           add100_weights = new_weight + 100, 
           add1000_weights = new_weight + 1000, 
           kg_weights = round(as.numeric(weight*0.45359237), 2),
           lbs_weights = round(as.numeric(weight*2.2046226218), 2),
           weight_floored = floor(weight),
           reversed_weights1 = as.numeric(Reverse_CPP2(weight_floored)),
           reversed_weights = case_when(weight_floored %in% bad_numbers == TRUE ~ Inf,
                                        weight_floored %in% bad_numbers == FALSE ~ reversed_weights1 + (weight %% 1)),
           div10_weights_diff = abs(div10_weights - both_effects),
           mul10_weights_diff = abs(mul10_weights - both_effects),
           div100_weights_diff = abs(div100_weights - both_effects),
           mul100_weights_diff = abs(mul100_weights - both_effects),
           div1000_weights_diff = abs(div1000_weights - both_effects),
           mul1000_weights_diff = abs(mul1000_weights - both_effects),
           min100_weights_diff = abs(min100_weights - both_effects),
           min1000_weights_diff = abs(min1000_weights - both_effects),
           add100_weights_diff = abs(add100_weights - both_effects),
           add1000_weights_diff = abs(add1000_weights - both_effects),
           kg_weights_diff = abs(kg_weights - both_effects),
           lbs_weights_diff = abs(lbs_weights - both_effects),
           reversed_weights_diff = abs(reversed_weights - both_effects)) %>%
    transform(smallest_diff = pmin((div10_weights_diff),
                                   (mul10_weights_diff), (div100_weights_diff),
                                   (mul100_weights_diff), (div1000_weights_diff),
                                   (mul1000_weights_diff), (min100_weights_diff), (min1000_weights_diff),
                                   (add100_weights_diff), (add1000_weights_diff),
                                   (kg_weights_diff), (lbs_weights_diff),
                                   reversed_weights_diff)) %>%
    mutate(div10_smallest_diff = (div10_weights_diff) == smallest_diff,
           mul10_smallest_diff = (mul10_weights_diff) == smallest_diff,
           div100_smallest_diff = (div100_weights_diff) == smallest_diff,
           mul100_smallest_diff = (mul100_weights_diff) == smallest_diff,
           div1000_smallest_diff = (div1000_weights_diff) == smallest_diff,
           mul1000_smallest_diff = (mul1000_weights_diff) == smallest_diff,
           min100_smallest_diff = (min100_weights_diff) == smallest_diff,
           min1000_smallest_diff = (min1000_weights_diff) == smallest_diff,
           add100_smallest_diff = (add100_weights_diff) == smallest_diff,
           add1000_smallest_diff = (add1000_weights_diff) == smallest_diff,
           kg_smallest_diff = (kg_weights_diff) == smallest_diff,
           lbs_smallest_diff = (lbs_weights_diff) == smallest_diff,
           reversed_smallest_diff = (reversed_weights_diff) == smallest_diff)
  
  #function that replaces outliers with the correction that reults in the 
  #smallest difference from the predicted measurement
  
  replace_outliers <- function(X, var1, var2, print_results = TRUE) {
    a <- sum(X[[paste(var1, "_outlier", sep = "")]])
    X[[var1]] <- ifelse(X[[paste(var1, "_outlier", sep = "")]] == TRUE &
                          X[[paste(var2, "_weights_outlier", sep = "")]] == FALSE &
                          X[[paste(var2, "_smallest_diff", sep = "")]] == TRUE, 
                        X[[paste(var2, "_weights", sep = "")]], X[[var1]])
    X <- get_outliers(X, print_results = FALSE)
    b <- sum(X[[paste(var1, "_outlier", sep = "")]])
    if(print_results == TRUE) {
      print(paste("Total number of outliers corrected"))
      print(a-b)
    }
    return(X)
  }
  
  #get the outliers for each possible correction
  dat <- get_outliers(dat)
  dat <- get_outliers(dat, var1 = "div10_weights")
  dat <- get_outliers(dat, var1 = "mul10_weights")
  dat <- get_outliers(dat, var1 = "div100_weights")
  dat <- get_outliers(dat, var1 = "mul100_weights")
  dat <- get_outliers(dat, var1 = "div1000_weights")
  dat <- get_outliers(dat, var1 = "mul1000_weights")
  dat <- get_outliers(dat, var1 = "kg_weights")
  dat <- get_outliers(dat, var1 = "lbs_weights")
  dat <- get_outliers(dat, var1 = "reversed_weights")
  dat <- get_outliers(dat, var1 = "min100_weights")
  dat <- get_outliers(dat, var1 = "min1000_weights")
  dat <- get_outliers(dat, var1 = "add100_weights")
  dat <- get_outliers(dat, var1 = "add1000_weights")
  
  
  #Make corrections
  dat <- replace_outliers(dat, "new_weight", "div10")
  dat <- replace_outliers(dat, "new_weight", "div100")  
  dat <- replace_outliers(dat, "new_weight", "div1000")  
  dat <- replace_outliers(dat, "new_weight", "mul10")
  dat <- replace_outliers(dat, "new_weight", "mul100")  
  dat <- replace_outliers(dat, "new_weight", "mul1000")  
  dat <- replace_outliers(dat, "new_weight", "min100")
  dat <- replace_outliers(dat, "new_weight", "min1000")
  dat <- replace_outliers(dat, "new_weight", "add100")
  dat <- replace_outliers(dat, "new_weight", "add1000")
  dat <- replace_outliers(dat, "new_weight", "kg")
  dat <- replace_outliers(dat, "new_weight", "lbs")  
  dat <- replace_outliers(dat, "new_weight", "reversed")  
  
#Apply step 4 of algorithm to identify consecutive values that jump in size

  #Function that identifies consecutive values that jump in size more than
  #the maximum predicted amount that they could change

  get_jumpers <- function(X) {
    X <- X %>%
      group_by(ind_ID) %>%
      mutate(weight_diff_lag = abs(new_weight - lag(new_weight)),
             weight_diff_lead = abs(new_weight - lead(new_weight)),
             weight_diff_accum = weight_diff_lead + weight_diff_lag,
             pred_diff_lag = abs(upper - lag(lower)),
             pred_diff_lead = abs(lower - lead(upper)),
             pred_diff_accum = pred_diff_lag + pred_diff_lead,
             jumper = weight_diff_lag > pred_diff_lag |
               weight_diff_lead > pred_diff_lead) %>%
      ungroup()
    X <- subset(X, !(X$new_weight_outlier == TRUE & X$jumper == TRUE))
    return(X)
  }  
  
  success <- FALSE
  
  while (!success) {
    dat <- dat
    size_of_dataset_before <- length(dat$ind_ID)
    dat <- get_jumpers(dat)
    size_of_dataset_after <- length(dat$ind_ID)
    size_of_dataset_before == size_of_dataset_after
    success <- (size_of_dataset_before == size_of_dataset_after) == TRUE
  }
  
  #look at difference
  dat <- get_duplications(dat)
  dat <- get_outliers(dat) 
  
#Apply step 5 of algorithm to remove any remaining implausible values 

  #define the externally defined cut-offs for weight that are used to identify outliers
  dat$cut_outlier <- (dat$new_weight < 0.5) | 
    ((dat$new_weight < 10) & (dat$age >= 1825)) | 
    (dat$new_weight > 250)
  
  #remove values defined as outliers  
  master_NLME_A <- dat %>%
    filter(cut_outlier == FALSE)

  
  
  
