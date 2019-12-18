# load libraries
library(parallel)
library(dplyr)

# load in bimodal distributions

source("simulation_setup/bimodal_skewed_sampling_setup.R")

# lapply functions

meanestimator_noci <- function(x){
  
  meanx <- mean(x = x)
  return(meanx)
}


### Mean Bimodal Estimates ###

# 10 obs 10 sd
mean10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = meanestimator_noci, mc.cores = 25))
mean10obs_10sd_df <- as.data.frame(split(mean10obs_10sd, 1:1))

# 10 obs 20 sd
mean10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = meanestimator_noci, mc.cores = 30))
mean10obs_20sd_df <- as.data.frame(split(mean10obs_20sd, 1:1))

# 20 obs 10 sd
mean20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = meanestimator_noci, mc.cores = 30))
mean20obs_10sd_df <- as.data.frame(split(mean20obs_10sd, 1:1))

# 20 obs 20 sd
mean20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = meanestimator_noci, mc.cores = 30))
mean20obs_20sd_df <- as.data.frame(split(mean20obs_20sd, 1:1))

########## 50 obs now y'all

# 50 obs 10 sd
mean50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = meanestimator_noci, mc.cores = 30))
mean50obs_10sd_df <- as.data.frame(split(mean50obs_10sd, 1:1))

# 50 obs 20 sd
mean50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = meanestimator_noci, mc.cores = 30))
mean50obs_20sd_df <- as.data.frame(split(mean50obs_20sd, 1:1))


## Mutate dataframes 

mean10obs_10sd_df <- mean10obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.61) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "mean")

mean20obs_10sd_df <- mean20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.61) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "mean")

mean50obs_10sd_df <- mean50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.61) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "mean")

# 20 sd

mean10obs_20sd_df <- mean10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.56) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "mean")

mean20obs_20sd_df <- mean20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.56) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "mean")

mean50obs_20sd_df <- mean50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 196.56) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "mean")

# # rbind these together

bimodal_mean_df <- rbind(mean10obs_10sd_df, mean10obs_20sd_df,
                         mean20obs_10sd_df, mean20obs_20sd_df,
                         mean50obs_10sd_df, mean50obs_20sd_df) %>% 
  mutate(perc = "Mean", Q = 50.50)

write.csv(bimodal_mean_df, file = "results/bimodal_skewed_mean.csv", row.names = FALSE)