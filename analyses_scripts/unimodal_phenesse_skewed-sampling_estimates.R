# load libraries
library(parallel)
library(dplyr)
library(devtools)
install_github("mbelitz/phenesse")
library(phenesse)

# load in unimodal distributions

source("simulation_setup/unimodal_skewed_sampling_setup.R")

# lapply functions

onsetestimator_noci <- function(x){
  
  onset <- phenesse::weib_percentile(observations = x, iterations = 500, percentile = 0)
  return(onset)
}

#onset

# 10 obs 10 sd
onset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = onsetestimator_noci,mc.cores = 10)) # already run
onset10obs_10sd_df <- as.data.frame(split(onset10obs_10sd, 1:1))

# 10 obs 20 sd
onset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = onsetestimator_noci, mc.cores = 10))
onset10obs_20sd_df <- as.data.frame(split(onset10obs_20sd, 1:1))

# 10 obs 40 sd
onset10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = onsetestimator_noci, mc.cores = 10))
onset10obs_40sd_df <- as.data.frame(split(onset10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
onset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = onsetestimator_noci, mc.cores = 15))
onset20obs_10sd_df <- as.data.frame(split(onset20obs_10sd, 1:1))

# 20 obs 20 sd
onset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = onsetestimator_noci, mc.cores = 15))
onset20obs_20sd_df <- as.data.frame(split(onset20obs_20sd, 1:1))

# 20 obs 40 sd
onset20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = onsetestimator_noci, mc.cores = 15))
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
  mutate(estimator = "belitz")

onset20obs_10sd_df <- onset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 162.68) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

onset50obs_10sd_df <- onset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 162.68) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

onset10obs_20sd_df <- onset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

onset20obs_20sd_df <- onset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

onset50obs_20sd_df <- onset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 119.92) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

onset10obs_40sd_df <- onset10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

onset20obs_40sd_df <- onset20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

onset50obs_40sd_df <- onset50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 35.88) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzonset_df <- plyr::rbind.fill(onset10obs_10sd_df, onset10obs_20sd_df, onset10obs_40sd_df,
                                   onset20obs_10sd_df, onset20obs_20sd_df, onset20obs_40sd_df,
                                   onset50obs_10sd_df, onset50obs_20sd_df, onset50obs_40sd_df) %>% 
  mutate(perc = "onset", Q = 0)

# lapply functions

firstestimator_noci <- function(x){
  
  first <- phenesse::weib_percentile(observations = x, iterations = 500, percentile = 0.01)
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
  mutate(estimator = "belitz")

first20obs_10sd_df <- first20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 176.73) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

first50obs_10sd_df <- first50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 176.73) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

first10obs_20sd_df <- first10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

first20obs_20sd_df <- first20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

first50obs_20sd_df <- first50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 153.74) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

first10obs_40sd_df <- first10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

first20obs_40sd_df <- first20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

first50obs_40sd_df <- first50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 107.64) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzfirst_df <- plyr::rbind.fill(first10obs_10sd_df, first10obs_20sd_df, first10obs_40sd_df,
                                   first20obs_10sd_df, first20obs_20sd_df, first20obs_40sd_df,
                                   first50obs_10sd_df, first50obs_20sd_df, first50obs_40sd_df) %>% 
  mutate(perc = "first", Q = 1)

# lapply functions

fifthestimator_noci <- function(x){
  
  fifth <- phenesse::weib_percentile(observations = x, iterations = 500, percentile = 0.05)
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
  mutate(estimator = "belitz")

fifth20obs_10sd_df <- fifth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 183.21) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

fifth50obs_10sd_df <- fifth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 183.21) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

fifth10obs_20sd_df <- fifth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

fifth20obs_20sd_df <- fifth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

fifth50obs_20sd_df <- fifth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 167.65) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

fifth10obs_40sd_df <- fifth10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

fifth20obs_40sd_df <- fifth20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

fifth50obs_40sd_df <- fifth50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 134.35) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzfifth_df <- plyr::rbind.fill(fifth10obs_10sd_df, fifth10obs_20sd_df, fifth10obs_40sd_df,
                                   fifth20obs_10sd_df, fifth20obs_20sd_df, fifth20obs_40sd_df,
                                   fifth50obs_10sd_df, fifth50obs_20sd_df, fifth50obs_40sd_df) %>% 
  mutate(perc = "fifth", Q = 5)

# lapply functions

tenthestimator_noci <- function(x){
  
  tenth <- phenesse::weib_percentile(observations = x, iterations = 500, percentile = 0.1)
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
  mutate(estimator = "belitz")

tenth20obs_10sd_df <- tenth20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 186.99) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

tenth50obs_10sd_df <- tenth50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 186.99) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

tenth10obs_20sd_df <- tenth10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

tenth20obs_20sd_df <- tenth20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

tenth50obs_20sd_df <- tenth50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 174.82) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

tenth10obs_40sd_df <- tenth10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

tenth20obs_40sd_df <- tenth20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

tenth50obs_40sd_df <- tenth50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 137.17) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitztenth_df <- plyr::rbind.fill(tenth10obs_10sd_df, tenth10obs_20sd_df, tenth10obs_40sd_df,
                                   tenth20obs_10sd_df, tenth20obs_20sd_df, tenth20obs_40sd_df,
                                   tenth50obs_10sd_df, tenth50obs_20sd_df, tenth50obs_40sd_df)%>% 
  mutate(perc = "tenth", Q = 10)

# lapply functions

fiftyestimator_noci <- function(x){
  
  fifty <- phenesse::weib_percentile(observations = x, iterations = 500, percentile = 0.5)
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
  mutate(estimator = "belitz")

fifty20obs_10sd_df <- fifty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

fifty50obs_10sd_df <- fifty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

fifty10obs_20sd_df <- fifty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

fifty20obs_20sd_df <- fifty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

fifty50obs_20sd_df <- fifty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.18) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

fifty10obs_40sd_df <- fifty10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

fifty20obs_40sd_df <- fifty20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

fifty50obs_40sd_df <- fifty50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.80) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzfifty_df <- plyr::rbind.fill(fifty10obs_10sd_df, fifty10obs_20sd_df, fifty10obs_40sd_df,
                                   fifty20obs_10sd_df, fifty20obs_20sd_df, fifty20obs_40sd_df,
                                   fifty50obs_10sd_df, fifty50obs_20sd_df, fifty50obs_40sd_df)%>% 
  mutate(perc = "fiftieth", Q = 50)

# rbind these together
unimodal_sims_all <- plyr::rbind.fill(belitzonset_df, belitzfirst_df, belitzfifth_df, belitztenth_df,
                                      belitzfifty_df)


write.csv(unimodal_sims_all, file = "results/unimodal_phenesse_skewed_sampling.csv", row.names = FALSE)