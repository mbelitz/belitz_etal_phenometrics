### NOTE TO USERS ###

#' The following code uses parellel computation with up to 30 cores being used at once.
#' Parallelization was completed using the mclapply function. To run this script locally,
#' replace mclapply with lapply and remove the mc.cores parameter. This phest
#' estimator should be able to run locally without too much computational pain, as 
#' its slick analytical solution makes it so speedy :)

# load libraries
library(parallel)
library(phest)
library(dplyr)

# load in unimodal distributions

source("simulation_setup/bimodal_skewed_sampling_setup.R")

# lapply functions

onsetestimator_noci <- function(x){
  
  onset <- phest::weib.limit(x = x, upper = FALSE)
  return(onset)
}


#onset

# 10 obs 10 sd
onset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = onsetestimator_noci, mc.cores = 25))
onset10obs_10sd_df <- as.data.frame(split(onset10obs_10sd, 1:3))

# 10 obs 20 sd
onset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset10obs_20sd_df <- as.data.frame(split(onset10obs_20sd, 1:3))

# 20 obs 10 sd
onset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = onsetestimator_noci, mc.cores = 30))
onset20obs_10sd_df <- as.data.frame(split(onset20obs_10sd, 1:3))

# 20 obs 20 sd
onset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset20obs_20sd_df <- as.data.frame(split(onset20obs_20sd, 1:3))

########## 50 obs now y'all

# 50 obs 10 sd
onset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_10sd_df <- as.data.frame(split(onset50obs_10sd, 1:3))

# 50 obs 20 sd
onset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = onsetestimator_noci, mc.cores = 30))
onset50obs_20sd_df <- as.data.frame(split(onset50obs_20sd, 1:3))


## Mutate dataframes 

onset10obs_10sd_df <- onset10obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "pearse")

onset20obs_10sd_df <- onset20obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "pearse")

onset50obs_10sd_df <- onset50obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 113.29) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "pearse")

# 20 sd

onset10obs_20sd_df <- onset10obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "pearse")

onset20obs_20sd_df <- onset20obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "pearse")

onset50obs_20sd_df <- onset50obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_onset = 76.58) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "pearse")

# # rbind these together

pearseonset_df <- rbind(onset10obs_10sd_df, onset10obs_20sd_df,
                        onset20obs_10sd_df, onset20obs_20sd_df,
                        onset50obs_10sd_df, onset50obs_20sd_df) %>% 
  mutate(perc = "onset", Q = 0)

#offset

# lapply functions

offsetestimator_noci <- function(x){
  
  offset <- phest::weib.limit(x = x, uppe = TRUE) 
  return(offset)
}

# 10 obs 10 sd
offset10obs_10sd <- unlist(mclapply(list_10obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset10obs_10sd_df <- as.data.frame(split(offset10obs_10sd, 1:3))

# 10 obs 20 sd
offset10obs_20sd <- unlist(mclapply(list_10obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset10obs_20sd_df <- as.data.frame(split(offset10obs_20sd, 1:3))

# 20 obs 10 sd
offset20obs_10sd <- unlist(mclapply(list_20obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset20obs_10sd_df <- as.data.frame(split(offset20obs_10sd, 1:3))

# 20 obs 20 sd
offset20obs_20sd <- unlist(mclapply(list_20obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset20obs_20sd_df <- as.data.frame(split(offset20obs_20sd, 1:3))

########## 50 obs now y'all

# 50 obs 10 sd
offset50obs_10sd <- unlist(mclapply(list_50obs_10sd, FUN = offsetestimator_noci, mc.cores = 30))
offset50obs_10sd_df <- as.data.frame(split(offset50obs_10sd, 1:3))

# 50 obs 20 sd
offset50obs_20sd <- unlist(mclapply(list_50obs_20sd, FUN = offsetestimator_noci, mc.cores = 30))
offset50obs_20sd_df <- as.data.frame(split(offset50obs_20sd, 1:3))


## Mutate dataframes 

offset10obs_10sd_df <- offset10obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 10, sd = 10) %>% 
  mutate(estimator = "pearse")

offset20obs_10sd_df <- offset20obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "pearse")

offset50obs_10sd_df <- offset50obs_10sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 257.28) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "pearse")

# 20 sd

offset10obs_20sd_df <- offset10obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "pearse")

offset20obs_20sd_df <- offset20obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "pearse")

offset50obs_20sd_df <- offset50obs_20sd_df %>% 
  rename(estimate = X1, lowCI = X2, highCI = X3) %>% 
  mutate(true_offset = 294.56) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "pearse")

# # rbind these togethe

pearseoffset_df <- rbind(offset10obs_10sd_df, offset10obs_20sd_df,
                         offset20obs_10sd_df, offset20obs_20sd_df,
                         offset50obs_10sd_df, offset50obs_20sd_df) %>% 
  mutate(perc = "offset", Q = 100)

pearseonset_df <- dplyr::rename(pearseonset_df, true_value = true_onset)
pearseoffset_df <- dplyr::rename(pearseoffset_df, true_value = true_offset)

bimodal_sims_all <- rbind(pearseonset_df,
                          pearseoffset_df)

# Save Results
write.csv(bimodal_sims_all, file = "results/bimodal_skewed_phest.csv", row.names = FALSE)