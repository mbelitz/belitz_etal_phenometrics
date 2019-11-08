library(ggplot2)
library(parallel)
library(dplyr)

# load in unimodal distributions

source("simulation_setup/unimodal_distribution_setup.R")

# lapply functions

onsetestimator_noci <- function(x){
  
  onset <- quantile(x = x, probs = 0)
  return(onset)
}

#onset

# 10 obs 10 sd
onset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = onsetestimator_noci,mc.cores = 20)) # already run
onset10obs_10sd_df <- as.data.frame(split(onset10obs_10sd, 1:1))

# 10 obs 20 sd
onset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = onsetestimator_noci, mc.cores = 20))
onset10obs_20sd_df <- as.data.frame(split(onset10obs_20sd, 1:1))

# 10 obs 40 sd
onset10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = onsetestimator_noci, mc.cores = 20))
onset10obs_40sd_df <- as.data.frame(split(onset10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
onset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = onsetestimator_noci, mc.cores = 20))
onset20obs_10sd_df <- as.data.frame(split(onset20obs_10sd, 1:1))

# 20 obs 20 sd
onset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = onsetestimator_noci, mc.cores = 20))
onset20obs_20sd_df <- as.data.frame(split(onset20obs_20sd, 1:1))

# 20 obs 40 sd
onset20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = onsetestimator_noci, mc.cores = 20))
onset20obs_40sd_df <- as.data.frame(split(onset20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
onset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_10sd_df <- as.data.frame(split(onset50obs_10sd, 1:1))

# 50 obs 20 sd
onset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_20sd_df <- as.data.frame(split(onset50obs_20sd, 1:1))

# 50 obs 40 sd
onset50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_40sd_df <-  as.data.frame(split(onset50obs_40sd, 1:1))

## Mutate dataframes 

onset10obs_10sd_df <- onset10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 162.68) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

onset20obs_10sd_df <- onset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 162.68) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

onset50obs_10sd_df <- onset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 162.68) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

onset10obs_20sd_df <- onset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

onset20obs_20sd_df <- onset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

onset50obs_20sd_df <- onset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

onset10obs_40sd_df <- onset10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

onset20obs_40sd_df <- onset20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

onset50obs_40sd_df <- onset50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naiveonset_df <- plyr::rbind.fill(onset10obs_10sd_df, onset10obs_20sd_df, onset10obs_40sd_df,
                                  onset20obs_10sd_df, onset20obs_20sd_df, onset20obs_40sd_df,
                                  onset50obs_10sd_df, onset50obs_20sd_df, onset50obs_40sd_df) %>% 
  mutate(perc = "onset", Q = 0)

# lapply functions

firstestimator_noci <- function(x){
  
  first <- quantile(x = x, probs = 0.01)
  return(first)
}

#first

# 10 obs 10 sd
first10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = firstestimator_noci,mc.cores = 20)) # already run
first10obs_10sd_df <- as.data.frame(split(first10obs_10sd, 1:1))

# 10 obs 20 sd
first10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = firstestimator_noci, mc.cores = 20))
first10obs_20sd_df <- as.data.frame(split(first10obs_20sd, 1:1))

# 10 obs 40 sd
first10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = firstestimator_noci, mc.cores = 20))
first10obs_40sd_df <- as.data.frame(split(first10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
first20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = firstestimator_noci, mc.cores = 20))
first20obs_10sd_df <- as.data.frame(split(first20obs_10sd, 1:1))

# 20 obs 20 sd
first20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = firstestimator_noci, mc.cores = 20))
first20obs_20sd_df <- as.data.frame(split(first20obs_20sd, 1:1))

# 20 obs 40 sd
first20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = firstestimator_noci, mc.cores = 20))
first20obs_40sd_df <- as.data.frame(split(first20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
first50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = firstestimator_noci, mc.cores = 20))
first50obs_10sd_df <- as.data.frame(split(first50obs_10sd, 1:1))

# 50 obs 20 sd
first50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = firstestimator_noci, mc.cores = 20))
first50obs_20sd_df <- as.data.frame(split(first50obs_20sd, 1:1))

# 50 obs 40 sd
first50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = firstestimator_noci, mc.cores = 20))
first50obs_40sd_df <-  as.data.frame(split(first50obs_40sd, 1:1))

## Mutate dataframes 

first10obs_10sd_df <- first10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 176.73) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

first20obs_10sd_df <- first20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 176.73) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

first50obs_10sd_df <- first50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 176.73) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

first10obs_20sd_df <- first10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

first20obs_20sd_df <- first20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

first50obs_20sd_df <- first50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

first10obs_40sd_df <- first10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

first20obs_40sd_df <- first20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

first50obs_40sd_df <- first50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivefirst_df <- plyr::rbind.fill(first10obs_10sd_df, first10obs_20sd_df, first10obs_40sd_df,
                                  first20obs_10sd_df, first20obs_20sd_df, first20obs_40sd_df,
                                  first50obs_10sd_df, first50obs_20sd_df, first50obs_40sd_df) %>% 
  mutate(perc = "first", Q = 1)

# lapply functions

fifthestimator_noci <- function(x){
  
  fifth <- quantile(x = x, probs = 0.05)
  return(fifth)
}

#fifth

# 10 obs 10 sd
fifth10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = fifthestimator_noci,mc.cores = 20)) # already run
fifth10obs_10sd_df <- as.data.frame(split(fifth10obs_10sd, 1:1))

# 10 obs 20 sd
fifth10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth10obs_20sd_df <- as.data.frame(split(fifth10obs_20sd, 1:1))

# 10 obs 40 sd
fifth10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth10obs_40sd_df <- as.data.frame(split(fifth10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
fifth20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth20obs_10sd_df <- as.data.frame(split(fifth20obs_10sd, 1:1))

# 20 obs 20 sd
fifth20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth20obs_20sd_df <- as.data.frame(split(fifth20obs_20sd, 1:1))

# 20 obs 40 sd
fifth20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth20obs_40sd_df <- as.data.frame(split(fifth20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
fifth50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth50obs_10sd_df <- as.data.frame(split(fifth50obs_10sd, 1:1))

# 50 obs 20 sd
fifth50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth50obs_20sd_df <- as.data.frame(split(fifth50obs_20sd, 1:1))

# 50 obs 40 sd
fifth50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = fifthestimator_noci, mc.cores = 20))
fifth50obs_40sd_df <-  as.data.frame(split(fifth50obs_40sd, 1:1))

## Mutate dataframes 

fifth10obs_10sd_df <- fifth10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 183.21) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

fifth20obs_10sd_df <- fifth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 183.21) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

fifth50obs_10sd_df <- fifth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 183.21) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

fifth10obs_20sd_df <- fifth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

fifth20obs_20sd_df <- fifth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

fifth50obs_20sd_df <- fifth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

fifth10obs_40sd_df <- fifth10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

fifth20obs_40sd_df <- fifth20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

fifth50obs_40sd_df <- fifth50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivefifth_df <- plyr::rbind.fill(fifth10obs_10sd_df, fifth10obs_20sd_df, fifth10obs_40sd_df,
                                  fifth20obs_10sd_df, fifth20obs_20sd_df, fifth20obs_40sd_df,
                                  fifth50obs_10sd_df, fifth50obs_20sd_df, fifth50obs_40sd_df) %>% 
  mutate(perc = "fifth", Q = 5)

# lapply functions

tenthestimator_noci <- function(x){
  
  tenth <-quantile(x = x, probs = 0.1)
  return(tenth)
}

#tenth

# 10 obs 10 sd
tenth10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = tenthestimator_noci,mc.cores = 20)) # already run
tenth10obs_10sd_df <- as.data.frame(split(tenth10obs_10sd, 1:1))

# 10 obs 20 sd
tenth10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth10obs_20sd_df <- as.data.frame(split(tenth10obs_20sd, 1:1))

# 10 obs 40 sd
tenth10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth10obs_40sd_df <- as.data.frame(split(tenth10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
tenth20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth20obs_10sd_df <- as.data.frame(split(tenth20obs_10sd, 1:1))

# 20 obs 20 sd
tenth20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth20obs_20sd_df <- as.data.frame(split(tenth20obs_20sd, 1:1))

# 20 obs 40 sd
tenth20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth20obs_40sd_df <- as.data.frame(split(tenth20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
tenth50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth50obs_10sd_df <- as.data.frame(split(tenth50obs_10sd, 1:1))

# 50 obs 20 sd
tenth50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth50obs_20sd_df <- as.data.frame(split(tenth50obs_20sd, 1:1))

# 50 obs 40 sd
tenth50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = tenthestimator_noci, mc.cores = 20))
tenth50obs_40sd_df <-  as.data.frame(split(tenth50obs_40sd, 1:1))

## Mutate dataframes 

tenth10obs_10sd_df <- tenth10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 186.99) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

tenth20obs_10sd_df <- tenth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 186.99) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

tenth50obs_10sd_df <- tenth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 186.99) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

tenth10obs_20sd_df <- tenth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

tenth20obs_20sd_df <- tenth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

tenth50obs_20sd_df <- tenth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

tenth10obs_40sd_df <- tenth10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

tenth20obs_40sd_df <- tenth20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

tenth50obs_40sd_df <- tenth50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivetenth_df <- plyr::rbind.fill(tenth10obs_10sd_df, tenth10obs_20sd_df, tenth10obs_40sd_df,
                                  tenth20obs_10sd_df, tenth20obs_20sd_df, tenth20obs_40sd_df,
                                  tenth50obs_10sd_df, tenth50obs_20sd_df, tenth50obs_40sd_df)%>% 
  mutate(perc = "tenth", Q = 10)

# lapply functions

fiftyestimator_noci <- function(x){
  
  fifty <- quantile(x = x, probs = 0.5)
  return(fifty)
}

#fifty

# 10 obs 10 sd
fifty10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = fiftyestimator_noci,mc.cores = 20)) # already run
fifty10obs_10sd_df <- as.data.frame(split(fifty10obs_10sd, 1:1))

# 10 obs 20 sd
fifty10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty10obs_20sd_df <- as.data.frame(split(fifty10obs_20sd, 1:1))

# 10 obs 40 sd
fifty10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty10obs_40sd_df <- as.data.frame(split(fifty10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
fifty20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty20obs_10sd_df <- as.data.frame(split(fifty20obs_10sd, 1:1))

# 20 obs 20 sd
fifty20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty20obs_20sd_df <- as.data.frame(split(fifty20obs_20sd, 1:1))

# 20 obs 40 sd
fifty20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty20obs_40sd_df <- as.data.frame(split(fifty20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
fifty50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty50obs_10sd_df <- as.data.frame(split(fifty50obs_10sd, 1:1))

# 50 obs 20 sd
fifty50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty50obs_20sd_df <- as.data.frame(split(fifty50obs_20sd, 1:1))

# 50 obs 40 sd
fifty50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = fiftyestimator_noci, mc.cores = 20))
fifty50obs_40sd_df <-  as.data.frame(split(fifty50obs_40sd, 1:1))

## Mutate dataframes 

fifty10obs_10sd_df <- fifty10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

fifty20obs_10sd_df <- fifty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

fifty50obs_10sd_df <- fifty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

fifty10obs_20sd_df <- fifty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

fifty20obs_20sd_df <- fifty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

fifty50obs_20sd_df <- fifty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

fifty10obs_40sd_df <- fifty10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

fifty20obs_40sd_df <- fifty20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

fifty50obs_40sd_df <- fifty50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivefifty_df <- plyr::rbind.fill(fifty10obs_10sd_df, fifty10obs_20sd_df, fifty10obs_40sd_df,
                                  fifty20obs_10sd_df, fifty20obs_20sd_df, fifty20obs_40sd_df,
                                  fifty50obs_10sd_df, fifty50obs_20sd_df, fifty50obs_40sd_df)%>% 
  mutate(perc = "fiftieth", Q = 50)

# lapply functions

nintyestimator_noci <- function(x){
  
  ninty <-quantile(x = x, probs = 0.9)
  return(ninty)
}

#ninty

# 10 obs 10 sd
ninty10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintyestimator_noci,mc.cores = 20)) # already run
ninty10obs_10sd_df <- as.data.frame(split(ninty10obs_10sd, 1:1))

# 10 obs 20 sd
ninty10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty10obs_20sd_df <- as.data.frame(split(ninty10obs_20sd, 1:1))

# 10 obs 40 sd
ninty10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty10obs_40sd_df <- as.data.frame(split(ninty10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
ninty20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty20obs_10sd_df <- as.data.frame(split(ninty20obs_10sd, 1:1))

# 20 obs 20 sd
ninty20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty20obs_20sd_df <- as.data.frame(split(ninty20obs_20sd, 1:1))

# 20 obs 40 sd
ninty20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty20obs_40sd_df <- as.data.frame(split(ninty20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
ninty50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty50obs_10sd_df <- as.data.frame(split(ninty50obs_10sd, 1:1))

# 50 obs 20 sd
ninty50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty50obs_20sd_df <- as.data.frame(split(ninty50obs_20sd, 1:1))

# 50 obs 40 sd
ninty50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = nintyestimator_noci, mc.cores = 20))
ninty50obs_40sd_df <-  as.data.frame(split(ninty50obs_40sd, 1:1))

## Mutate dataframes 

ninty10obs_10sd_df <- ninty10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 212.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

ninty20obs_10sd_df <- ninty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 212.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

ninty50obs_10sd_df <- ninty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 212.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

ninty10obs_20sd_df <- ninty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

ninty20obs_20sd_df <- ninty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

ninty50obs_20sd_df <- ninty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

ninty10obs_40sd_df <- ninty10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

ninty20obs_40sd_df <- ninty20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

ninty50obs_40sd_df <- ninty50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naiveninty_df <- plyr::rbind.fill(ninty10obs_10sd_df, ninty10obs_20sd_df, ninty10obs_40sd_df,
                                  ninty20obs_10sd_df, ninty20obs_20sd_df, ninty20obs_40sd_df,
                                  ninty50obs_10sd_df, ninty50obs_20sd_df, ninty50obs_40sd_df)%>% 
  mutate(perc = "ninetieth", Q = 90)


# lapply functions

nintyfiveestimator_noci <- function(x){
  
  nintyfive <- quantile(x = x, probs = 0.95)
  return(nintyfive)
}

#nintyfive

# 10 obs 10 sd
nintyfive10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintyfiveestimator_noci,mc.cores = 20)) # already run
nintyfive10obs_10sd_df <- as.data.frame(split(nintyfive10obs_10sd, 1:1))

# 10 obs 20 sd
nintyfive10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive10obs_20sd_df <- as.data.frame(split(nintyfive10obs_20sd, 1:1))

# 10 obs 40 sd
nintyfive10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive10obs_40sd_df <- as.data.frame(split(nintyfive10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
nintyfive20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive20obs_10sd_df <- as.data.frame(split(nintyfive20obs_10sd, 1:1))

# 20 obs 20 sd
nintyfive20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive20obs_20sd_df <- as.data.frame(split(nintyfive20obs_20sd, 1:1))

# 20 obs 40 sd
nintyfive20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive20obs_40sd_df <- as.data.frame(split(nintyfive20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
nintyfive50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive50obs_10sd_df <- as.data.frame(split(nintyfive50obs_10sd, 1:1))

# 50 obs 20 sd
nintyfive50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive50obs_20sd_df <- as.data.frame(split(nintyfive50obs_20sd, 1:1))

# 50 obs 40 sd
nintyfive50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = nintyfiveestimator_noci, mc.cores = 20))
nintyfive50obs_40sd_df <-  as.data.frame(split(nintyfive50obs_40sd, 1:1))

## Mutate dataframes 

nintyfive10obs_10sd_df <- nintyfive10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 216.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

nintyfive20obs_10sd_df <- nintyfive20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 216.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

nintyfive50obs_10sd_df <- nintyfive50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 216.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

nintyfive10obs_20sd_df <- nintyfive10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

nintyfive20obs_20sd_df <- nintyfive20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

nintyfive50obs_20sd_df <- nintyfive50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

nintyfive10obs_40sd_df <- nintyfive10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

nintyfive20obs_40sd_df <- nintyfive20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

nintyfive50obs_40sd_df <- nintyfive50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivenintyfive_df <- plyr::rbind.fill(nintyfive10obs_10sd_df, nintyfive10obs_20sd_df, nintyfive10obs_40sd_df,
                                      nintyfive20obs_10sd_df, nintyfive20obs_20sd_df, nintyfive20obs_40sd_df,
                                      nintyfive50obs_10sd_df, nintyfive50obs_20sd_df, nintyfive50obs_40sd_df)%>% 
  mutate(perc = "ninetyfifth", Q = 95)

# lapply functions

nintynineestimator_noci <- function(x){
  
  nintynine <- quantile(x = x, probs = 0.99)
  return(nintynine)
}

#nintynine

# 10 obs 10 sd
nintynine10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintynineestimator_noci,mc.cores = 20)) # already run
nintynine10obs_10sd_df <- as.data.frame(split(nintynine10obs_10sd, 1:1))

# 10 obs 20 sd
nintynine10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine10obs_20sd_df <- as.data.frame(split(nintynine10obs_20sd, 1:1))

# 10 obs 40 sd
nintynine10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine10obs_40sd_df <- as.data.frame(split(nintynine10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
nintynine20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine20obs_10sd_df <- as.data.frame(split(nintynine20obs_10sd, 1:1))

# 20 obs 20 sd
nintynine20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine20obs_20sd_df <- as.data.frame(split(nintynine20obs_20sd, 1:1))

# 20 obs 40 sd
nintynine20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine20obs_40sd_df <- as.data.frame(split(nintynine20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
nintynine50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine50obs_10sd_df <- as.data.frame(split(nintynine50obs_10sd, 1:1))

# 50 obs 20 sd
nintynine50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine50obs_20sd_df <- as.data.frame(split(nintynine50obs_20sd, 1:1))

# 50 obs 40 sd
nintynine50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = nintynineestimator_noci, mc.cores = 20))
nintynine50obs_40sd_df <-  as.data.frame(split(nintynine50obs_40sd, 1:1))

## Mutate dataframes 

nintynine10obs_10sd_df <- nintynine10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 223.52) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

nintynine20obs_10sd_df <- nintynine20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 223.52) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

nintynine50obs_10sd_df <- nintynine50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 223.52) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

nintynine10obs_20sd_df <- nintynine10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

nintynine20obs_20sd_df <- nintynine20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

nintynine50obs_20sd_df <- nintynine50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

nintynine10obs_40sd_df <- nintynine10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

nintynine20obs_40sd_df <- nintynine20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

nintynine50obs_40sd_df <- nintynine50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naivenintynine_df <- plyr::rbind.fill(nintynine10obs_10sd_df, nintynine10obs_20sd_df, nintynine10obs_40sd_df,
                                      nintynine20obs_10sd_df, nintynine20obs_20sd_df, nintynine20obs_40sd_df,
                                      nintynine50obs_10sd_df, nintynine50obs_20sd_df, nintynine50obs_40sd_df) %>% 
  mutate(perc = "ninetynineth", Q = 99)

# lapply functions

offsetestimator_noci <- function(x){
  
  offset <- quantile(x = x, probs = 1)
  return(offset)
}

#offset

# 10 obs 10 sd
offset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = offsetestimator_noci,mc.cores = 20)) # already run
offset10obs_10sd_df <- as.data.frame(split(offset10obs_10sd, 1:1))

# 10 obs 20 sd
offset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = offsetestimator_noci, mc.cores = 20))
offset10obs_20sd_df <- as.data.frame(split(offset10obs_20sd, 1:1))

# 10 obs 40 sd
offset10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = offsetestimator_noci, mc.cores = 20))
offset10obs_40sd_df <- as.data.frame(split(offset10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
offset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = offsetestimator_noci, mc.cores = 20))
offset20obs_10sd_df <- as.data.frame(split(offset20obs_10sd, 1:1))

# 20 obs 20 sd
offset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = offsetestimator_noci, mc.cores = 20))
offset20obs_20sd_df <- as.data.frame(split(offset20obs_20sd, 1:1))

# 20 obs 40 sd
offset20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = offsetestimator_noci, mc.cores = 20))
offset20obs_40sd_df <- as.data.frame(split(offset20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
offset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = offsetestimator_noci, mc.cores = 20))
offset50obs_10sd_df <- as.data.frame(split(offset50obs_10sd, 1:1))

# 50 obs 20 sd
offset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = offsetestimator_noci, mc.cores = 20))
offset50obs_20sd_df <- as.data.frame(split(offset50obs_20sd, 1:1))

# 50 obs 40 sd
offset50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = offsetestimator_noci, mc.cores = 20))
offset50obs_40sd_df <-  as.data.frame(split(offset50obs_40sd, 1:1))

## Mutate dataframes 

offset10obs_10sd_df <- offset10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 238.1) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "naive")

offset20obs_10sd_df <- offset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 238.1) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "naive")

offset50obs_10sd_df <- offset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 238.1) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "naive")

# 20 sd

offset10obs_20sd_df <- offset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "naive")

offset20obs_20sd_df <- offset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "naive")

offset50obs_20sd_df <- offset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "naive")

# 40 sd

offset10obs_40sd_df <- offset10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "naive")

offset20obs_40sd_df <- offset20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "naive")

offset50obs_40sd_df <- offset50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "naive")

# rbind these together

naiveoffset_df <- plyr::rbind.fill(offset10obs_10sd_df, offset10obs_20sd_df, offset10obs_40sd_df,
                                   offset20obs_10sd_df, offset20obs_20sd_df, offset20obs_40sd_df,
                                   offset50obs_10sd_df, offset50obs_20sd_df, offset50obs_40sd_df)%>% 
  mutate(perc = "offset", Q = 100)

unimodal_sims_all <- plyr::rbind.fill(naiveonset_df, naivefirst_df, naivefifth_df, naivetenth_df,
                                      naivefifty_df, naiveninty_df, naivenintyfive_df, naivenintynine_df,
                                      naiveoffset_df)


write.csv(unimodal_sims_all, file = "results/unimodal_quantile.csv", row.names = FALSE)