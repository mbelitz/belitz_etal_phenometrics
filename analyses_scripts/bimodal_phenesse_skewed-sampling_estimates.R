### NOTE TO USERS ###

#' The following code uses parellel computation with up to 30 cores being used at once.
#' Parallelization was completed using the mclapply function. To run this script locally,
#' replace mclapply with lapply and remove the mc.cores parameter. WARNING! 
#' This script without parallelization would take a long time to run. 

# load libraries
library(parallel)
library(dplyr)
library(devtools)
library(phenesse)

# load in bimodal distributions

source("simulation_setup/bimodal_skewed_sampling_setup.R")

# lapply functions

onsetestimator_noci <- function(x){
  
  onset <- weib_percentile(observations = x, iterations = 500, percentile = 0)
  return(onset)
}


#onset

# 10 obs 10 sd
onset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = onsetestimator_noci, mc.cores = 25))
onset10obs_10sd_df <- as.data.frame(split(onset10obs_10sd, 1:1))

# 10 obs 20 sd
onset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset10obs_20sd_df <- as.data.frame(split(onset10obs_20sd, 1:1))

# 20 obs 10 sd
onset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = onsetestimator_noci, mc.cores = 30))
onset20obs_10sd_df <- as.data.frame(split(onset20obs_10sd, 1:1))

# 20 obs 20 sd
onset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset20obs_20sd_df <- as.data.frame(split(onset20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
onset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_10sd_df <- as.data.frame(split(onset50obs_10sd, 1:1))

# 50 obs 20 sd
onset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_20sd_df <- as.data.frame(split(onset50obs_20sd, 1:1))


## Mutate dataframes 

onset10obs_10sd_df <- onset10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

onset20obs_10sd_df <- onset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

onset50obs_10sd_df <- onset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

onset10obs_20sd_df <- onset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

onset20obs_20sd_df <- onset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

onset50obs_20sd_df <- onset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzonset_df <- rbind(onset10obs_10sd_df, onset10obs_20sd_df,
                        onset20obs_10sd_df, onset20obs_20sd_df,
                        onset50obs_10sd_df, onset50obs_20sd_df) %>% 
  mutate(perc = "onset", Q = 0)


# lapply functions

firstestimator_noci <- function(x){
  
  first <- weib_percentile(observations = x, iterations = 500, percentile = 0.01)
  return(first)
}


#first

# 10 obs 10 sd
first10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = firstestimator_noci, mc.cores = 30))
first10obs_10sd_df <- as.data.frame(split(first10obs_10sd, 1:1))

# 10 obs 20 sd
first10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = firstestimator_noci, mc.cores = 30))
first10obs_20sd_df <- as.data.frame(split(first10obs_20sd, 1:1))

# 20 obs 10 sd
first20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = firstestimator_noci, mc.cores = 30))
first20obs_10sd_df <- as.data.frame(split(first20obs_10sd, 1:1))

# 20 obs 20 sd
first20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = firstestimator_noci, mc.cores = 30))
first20obs_20sd_df <- as.data.frame(split(first20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
first50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = firstestimator_noci, mc.cores = 30))
first50obs_10sd_df <- as.data.frame(split(first50obs_10sd, 1:1))

# 50 obs 20 sd
first50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = firstestimator_noci, mc.cores = 30))
first50obs_20sd_df <- as.data.frame(split(first50obs_20sd, 1:1))


## Mutate dataframes 

first10obs_10sd_df <- first10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 130.55) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

first20obs_10sd_df <- first20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 130.55) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

first50obs_10sd_df <- first50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 130.55) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

first10obs_20sd_df <- first10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 111.10) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

first20obs_20sd_df <- first20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 111.10) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

first50obs_20sd_df <- first50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_first = 111.10) %>% 
  mutate(distance = estimate - true_first) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzfirst_df <- rbind(first10obs_10sd_df, first10obs_20sd_df,
                        first20obs_10sd_df, first20obs_20sd_df,
                        first50obs_10sd_df, first50obs_20sd_df) %>% 
  mutate(perc = "first", Q = 1)

# lapply function


fifthestimator_noci <- function(x){
  
  fifth <- weib_percentile(observations = x, iterations = 500, percentile = 0.05)
  return(fifth)
}


#fifth

# 10 obs 10 sd
fifth10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth10obs_10sd_df <- as.data.frame(split(fifth10obs_10sd, 1:1))

# 10 obs 20 sd
fifth10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth10obs_20sd_df <- as.data.frame(split(fifth10obs_20sd, 1:1))

# 20 obs 10 sd
fifth20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth20obs_10sd_df <- as.data.frame(split(fifth20obs_10sd, 1:1))

# 20 obs 20 sd
fifth20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth20obs_20sd_df <- as.data.frame(split(fifth20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
fifth50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth50obs_10sd_df <- as.data.frame(split(fifth50obs_10sd, 1:1))

# 50 obs 20 sd
fifth50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = fifthestimator_noci, mc.cores = 30))
fifth50obs_20sd_df <- as.data.frame(split(fifth50obs_20sd, 1:1))


## Mutate dataframes 

fifth10obs_10sd_df <- fifth10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 139.48) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

fifth20obs_10sd_df <- fifth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 139.48) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

fifth50obs_10sd_df <- fifth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 139.48) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

fifth10obs_20sd_df <- fifth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 128.96) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

fifth20obs_20sd_df <- fifth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 128.96) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

fifth50obs_20sd_df <- fifth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifth = 128.96) %>% 
  mutate(distance = estimate - true_fifth) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzfifth_df <- rbind(fifth10obs_10sd_df, fifth10obs_20sd_df,
                        fifth20obs_10sd_df, fifth20obs_20sd_df,
                        fifth50obs_10sd_df, fifth50obs_20sd_df) %>% 
  mutate(perc = "fifth", Q = 5)

#tenth

# lapply function


tenthestimator_noci <- function(x){
  
  tenth <- weib_percentile(observations = x, iterations = 500, percentile = 0.1)
  return(tenth)
}

# 10 obs 10 sd
tenth10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth10obs_10sd_df <- as.data.frame(split(tenth10obs_10sd, 1:1))

# 10 obs 20 sd
tenth10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth10obs_20sd_df <- as.data.frame(split(tenth10obs_20sd, 1:1))

# 20 obs 10 sd
tenth20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth20obs_10sd_df <- as.data.frame(split(tenth20obs_10sd, 1:1))

# 20 obs 20 sd
tenth20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth20obs_20sd_df <- as.data.frame(split(tenth20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
tenth50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth50obs_10sd_df <- as.data.frame(split(tenth50obs_10sd, 1:1))

# 50 obs 20 sd
tenth50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = tenthestimator_noci, mc.cores = 30))
tenth50obs_20sd_df <- as.data.frame(split(tenth50obs_20sd, 1:1))


## Mutate dataframes 

tenth10obs_10sd_df <- tenth10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 144.65) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

tenth20obs_10sd_df <- tenth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 144.65) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

tenth50obs_10sd_df <- tenth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 144.65) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

tenth10obs_20sd_df <- tenth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 139.29) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

tenth20obs_20sd_df <- tenth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 139.29) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

tenth50obs_20sd_df <- tenth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_tenth = 139.29) %>% 
  mutate(distance = estimate - true_tenth) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitztenth_df <- rbind(tenth10obs_10sd_df, tenth10obs_20sd_df,
                        tenth20obs_10sd_df, tenth20obs_20sd_df,
                        tenth50obs_10sd_df, tenth50obs_20sd_df) %>% 
  mutate(perc = "tenth", Q = 10)

#fifty

# lapply function


fiftyestimator_noci <- function(x){
  
  fifty <- weib_percentile(observations = x, iterations = 500, percentile = 0.5)
  return(fifty)
}

# 10 obs 10 sd
fifty10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty10obs_10sd_df <- as.data.frame(split(fifty10obs_10sd, 1:1))

# 10 obs 20 sd
fifty10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty10obs_20sd_df <- as.data.frame(split(fifty10obs_20sd, 1:1))

# 20 obs 10 sd
fifty20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty20obs_10sd_df <- as.data.frame(split(fifty20obs_10sd, 1:1))

# 20 obs 20 sd
fifty20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty20obs_20sd_df <- as.data.frame(split(fifty20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
fifty50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty50obs_10sd_df <- as.data.frame(split(fifty50obs_10sd, 1:1))

# 50 obs 20 sd
fifty50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = fiftyestimator_noci, mc.cores = 30))
fifty50obs_20sd_df <- as.data.frame(split(fifty50obs_20sd, 1:1))


## Mutate dataframes 

fifty10obs_10sd_df <- fifty10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 213.21) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

fifty20obs_10sd_df <- fifty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 213.21) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

fifty50obs_10sd_df <- fifty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 213.21) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

fifty10obs_20sd_df <- fifty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 206.48) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

fifty20obs_20sd_df <- fifty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 206.48) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

fifty50obs_20sd_df <- fifty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_fifty = 206.48) %>% 
  mutate(distance = estimate - true_fifty) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzfifty_df <- rbind(fifty10obs_10sd_df, fifty10obs_20sd_df,
                        fifty20obs_10sd_df, fifty20obs_20sd_df,
                        fifty50obs_10sd_df, fifty50obs_20sd_df) %>% 
  mutate(perc = "fiftieth", Q = 50)

#ninty

# lapply functions

nintyestimator_noci <- function(x){
  
  ninty <- weib_percentile(observations = x, iterations = 500, percentile = 0.9)
  return(ninty)
}


# 10 obs 10 sd
ninty10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty10obs_10sd_df <- as.data.frame(split(ninty10obs_10sd, 1:1))

# 10 obs 20 sd
ninty10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty10obs_20sd_df <- as.data.frame(split(ninty10obs_20sd, 1:1))

# 20 obs 10 sd
ninty20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty20obs_10sd_df <- as.data.frame(split(ninty20obs_10sd, 1:1))

# 20 obs 20 sd
ninty20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty20obs_20sd_df <- as.data.frame(split(ninty20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
ninty50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty50obs_10sd_df <- as.data.frame(split(ninty50obs_10sd, 1:1))

# 50 obs 20 sd
ninty50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintyestimator_noci, mc.cores = 30))
ninty50obs_20sd_df <- as.data.frame(split(ninty50obs_20sd, 1:1))


## Mutate dataframes 

ninty10obs_10sd_df <- ninty10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 230.27) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

ninty20obs_10sd_df <- ninty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 230.27) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

ninty50obs_10sd_df <- ninty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 230.27) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

ninty10obs_20sd_df <- ninty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 240.55) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

ninty20obs_20sd_df <- ninty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 240.55) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

ninty50obs_20sd_df <- ninty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_ninty = 240.55) %>% 
  mutate(distance = estimate - true_ninty) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzninty_df <- rbind(ninty10obs_10sd_df, ninty10obs_20sd_df,
                        ninty20obs_10sd_df, ninty20obs_20sd_df,
                        ninty50obs_10sd_df, ninty50obs_20sd_df) %>% 
  mutate(perc = "nintieth", Q = 90)

#nintyfifth

# lapply functions

nintyfifthestimator_noci <- function(x){
  
  nintyfifth <- weib_percentile(observations = x, iterations = 500, percentile = 0.95)
  return(nintyfifth)
}


# 10 obs 10 sd
nintyfifth10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth10obs_10sd_df <- as.data.frame(split(nintyfifth10obs_10sd, 1:1))

# 10 obs 20 sd
nintyfifth10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth10obs_20sd_df <- as.data.frame(split(nintyfifth10obs_20sd, 1:1))

# 20 obs 10 sd
nintyfifth20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth20obs_10sd_df <- as.data.frame(split(nintyfifth20obs_10sd, 1:1))

# 20 obs 20 sd
nintyfifth20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth20obs_20sd_df <- as.data.frame(split(nintyfifth20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
nintyfifth50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth50obs_10sd_df <- as.data.frame(split(nintyfifth50obs_10sd, 1:1))

# 50 obs 20 sd
nintyfifth50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintyfifthestimator_noci, mc.cores = 30))
nintyfifth50obs_20sd_df <- as.data.frame(split(nintyfifth50obs_20sd, 1:1))


## Mutate dataframes 

nintyfifth10obs_10sd_df <- nintyfifth10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 234.23) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

nintyfifth20obs_10sd_df <- nintyfifth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 234.23) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

nintyfifth50obs_10sd_df <- nintyfifth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 234.23) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

nintyfifth10obs_20sd_df <- nintyfifth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 248.46) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

nintyfifth20obs_20sd_df <- nintyfifth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 248.46) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

nintyfifth50obs_20sd_df <- nintyfifth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintyfifth = 248.46) %>% 
  mutate(distance = estimate - true_nintyfifth) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitznintyfifth_df <- rbind(nintyfifth10obs_10sd_df, nintyfifth10obs_20sd_df,
                             nintyfifth20obs_10sd_df, nintyfifth20obs_20sd_df,
                             nintyfifth50obs_10sd_df, nintyfifth50obs_20sd_df) %>% 
  mutate(perc = "nintyfifth", Q = 95)


#nintynintynine

# lapply functions

nintynineestimator_noci <- function(x){
  
  nintynine <- weib_percentile(observations = x, iterations = 500, percentile = 0.99)
  return(nintynine)
}

# 10 obs 10 sd
nintynine10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine10obs_10sd_df <- as.data.frame(split(nintynine10obs_10sd, 1:1))

# 10 obs 20 sd
nintynine10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine10obs_20sd_df <- as.data.frame(split(nintynine10obs_20sd, 1:1))

# 20 obs 10 sd
nintynine20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine20obs_10sd_df <- as.data.frame(split(nintynine20obs_10sd, 1:1))

# 20 obs 20 sd
nintynine20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine20obs_20sd_df <- as.data.frame(split(nintynine20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
nintynine50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine50obs_10sd_df <- as.data.frame(split(nintynine50obs_10sd, 1:1))

# 50 obs 20 sd
nintynine50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = nintynineestimator_noci, mc.cores = 30))
nintynine50obs_20sd_df <- as.data.frame(split(nintynine50obs_20sd, 1:1))


## Mutate dataframes 

nintynine10obs_10sd_df <- nintynine10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 242) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

nintynine20obs_10sd_df <- nintynine20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 242) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

nintynine50obs_10sd_df <- nintynine50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 242) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

nintynine10obs_20sd_df <- nintynine10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 264.01) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

nintynine20obs_20sd_df <- nintynine20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 264.01) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

nintynine50obs_20sd_df <- nintynine50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_nintynine = 264.01) %>% 
  mutate(distance = estimate - true_nintynine) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitznintynine_df <- rbind(nintynine10obs_10sd_df, nintynine10obs_20sd_df,
                            nintynine20obs_10sd_df, nintynine20obs_20sd_df,
                            nintynine50obs_10sd_df, nintynine50obs_20sd_df) %>% 
  mutate(perc = "nintyninth", Q = 99)


#offset

# lapply functions

offsetestimator_noci <- function(x){
  
  offset <- weib_percentile(observations = x, iterations = 500, percentile = 1)
  return(offset)
}

# 10 obs 10 sd
offset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset10obs_10sd_df <- as.data.frame(split(offset10obs_10sd, 1:1))

# 10 obs 20 sd
offset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset10obs_20sd_df <- as.data.frame(split(offset10obs_20sd, 1:1))

# 20 obs 10 sd
offset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset20obs_10sd_df <- as.data.frame(split(offset20obs_10sd, 1:1))

# 20 obs 20 sd
offset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset20obs_20sd_df <- as.data.frame(split(offset20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
offset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset50obs_10sd_df <- as.data.frame(split(offset50obs_10sd, 1:1))

# 50 obs 20 sd
offset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset50obs_20sd_df <- as.data.frame(split(offset50obs_20sd, 1:1))


## Mutate dataframes 

offset10obs_10sd_df <- offset10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "belitz")

offset20obs_10sd_df <- offset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

offset50obs_10sd_df <- offset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

offset10obs_20sd_df <- offset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

offset20obs_20sd_df <- offset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

offset50obs_20sd_df <- offset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# # rbind these together

belitzoffset_df <- rbind(offset10obs_10sd_df, offset10obs_20sd_df,
                         offset20obs_10sd_df, offset20obs_20sd_df,
                         offset50obs_10sd_df, offset50obs_20sd_df) %>% 
  mutate(perc = "offset", Q = 100)

belitzonset_df <- dplyr::rename(belitzonset_df, true_value = true_onset)
belitzfirst_df <- dplyr::rename(belitzfirst_df, true_value = true_first)
belitzfifth_df <- dplyr::rename(belitzfifth_df, true_value = true_fifth)
belitztenth_df <- dplyr::rename(belitztenth_df, true_value = true_tenth)
belitzfifty_df <- dplyr::rename(belitzfifty_df, true_value = true_fifty)
belitzninty_df <- dplyr::rename(belitzninty_df, true_value = true_ninty)
belitznintyfifth_df <- dplyr::rename(belitznintyfifth_df, true_value = true_nintyfifth)
belitznintynine_df <- dplyr::rename(belitznintynine_df, true_value = true_nintynine)
belitzoffset_df <- dplyr::rename(belitzoffset_df, true_value = true_offset)

bimodal_sims_all <- rbind(belitzonset_df, belitzfirst_df, belitzfifth_df, belitztenth_df,
                          belitzfifty_df, belitzninty_df, belitznintyfifth_df, belitznintynine_df,
                          belitzoffset_df)

# Save Results
write.csv(bimodal_sims_all, file = "results/bimodal_skewed_phenesse.csv", row.names = FALSE)
