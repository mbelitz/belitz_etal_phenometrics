# load libraries
library(parallel)
library(dplyr)

# load in unimodal distributions

source("simulation_setup/unimodal_skewed_sampling_setup.R")


# lapply functions

meanestimator_noci <- function(x){
  
  meanx <- mean(x = x)
  return(meanx)
}

### Mean Estimates ###

# 10 obs 10 sd
mean10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = meanestimator_noci,mc.cores = 20)) # already run
mean10obs_10sd_df <- as.data.frame(split(mean10obs_10sd, 1:1))

# 10 obs 20 sd
mean10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = meanestimator_noci, mc.cores = 20))
mean10obs_20sd_df <- as.data.frame(split(mean10obs_20sd, 1:1))

# 10 obs 40 sd
mean10obs_40sd <- unlist(mclapply(list_10obs_40sd, FUN = meanestimator_noci, mc.cores = 20))
mean10obs_40sd_df <- as.data.frame(split(mean10obs_40sd, 1:1))

###### 20 Obs ####

# 20 obs 10 sd
mean20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = meanestimator_noci, mc.cores = 20))
mean20obs_10sd_df <- as.data.frame(split(mean20obs_10sd, 1:1))

# 20 obs 20 sd
mean20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = meanestimator_noci, mc.cores = 20))
mean20obs_20sd_df <- as.data.frame(split(mean20obs_20sd, 1:1))

# 20 obs 40 sd
mean20obs_40sd <- unlist(mclapply(list_20obs_40sd, FUN = meanestimator_noci, mc.cores = 20))
mean20obs_40sd_df <- as.data.frame(split(mean20obs_40sd, 1:1))

########## 50 obs now y'all ########

# 50 obs 10 sd
mean50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = meanestimator_noci, mc.cores = 30))
mean50obs_10sd_df <- as.data.frame(split(mean50obs_10sd, 1:1))

# 50 obs 20 sd
mean50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = meanestimator_noci, mc.cores = 30))
mean50obs_20sd_df <- as.data.frame(split(mean50obs_20sd, 1:1))

# 50 obs 40 sd
mean50obs_40sd <- unlist(mclapply(list_50obs_40sd, FUN = meanestimator_noci, mc.cores = 30))
mean50obs_40sd_df <-  as.data.frame(split(mean50obs_40sd, 1:1))

## Mutate dataframes 

mean10obs_10sd_df <- mean10obs_10sd_df %>% 
  rename(estimate = X1) %>%  
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "mean")

mean20obs_10sd_df <- mean20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "mean")

mean50obs_10sd_df <- mean50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 199.89) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "mean")

# 20 sd

mean10obs_20sd_df <- mean10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.16) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "mean")

mean20obs_20sd_df <- mean20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.16) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "mean")

mean50obs_20sd_df <- mean50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.16) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "mean")

# 40 sd

mean10obs_40sd_df <- mean10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "mean")

mean20obs_40sd_df <- mean20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "mean")

mean50obs_40sd_df <- mean50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 200.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "mean")

# rbind these together

unimodal_mean_df <- plyr::rbind.fill(mean10obs_10sd_df, mean10obs_20sd_df, mean10obs_40sd_df,
                                     mean20obs_10sd_df, mean20obs_20sd_df, mean20obs_40sd_df,
                                     mean50obs_10sd_df, mean50obs_20sd_df, mean50obs_40sd_df) %>% 
  mutate(perc = "Mean", Q = 50.50)

write.csv(unimodal_mean_df, file = "results/unimodal_skewed_mean.csv", row.names = FALSE)