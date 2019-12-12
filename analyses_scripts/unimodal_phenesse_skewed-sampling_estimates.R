# load libraries
library(parallel)
library(dplyr)
library(devtools)
#install_github("mbelitz/phenesse")
#library(phenesse)

# load in function

weib_percentile <- function(observations, percentile = 0.9, iterations = 500){
  
  curve_intersect <- function(curve1, curve2, empirical=TRUE, domain=NULL) {
    if (!empirical & missing(domain)) {
      stop("'domain' must be provided with non-empirical curves")
    }
    
    if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
      stop("'domain' must be a two-value numeric vector, like c(0, 10)")
    }
    
    if (empirical) {
      # Approximate the functional form of both curves
      curve1_f <- stats::approxfun(curve1$x, curve1$y, rule = 2)
      curve2_f <- stats::approxfun(curve2$x, curve2$y, rule = 2)
      
      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      point_x <- stats::uniroot(function(x) curve1_f(x) - curve2_f(x),
                                c(min(curve1$x), max(curve1$x)))$root
      
      # Find where point_x is in curve 2
      point_y <- curve2_f(point_x)
    } else {
      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      # within the given domain
      point_x <- stats::uniroot(function(x) curve1(x) - curve2(x), domain)$root
      
      # Find where point_x is in curve 2
      point_y <- curve2(point_x)
    }
    
    return(list(x = point_x, y = point_y))
  }
  
  create_cdf_ends <- function(observations){
    weib <- fitdistrplus::fitdist(observations, distr = "weibull",
                                  method = "mle")
    cdf0 <- as.numeric(weib$estimate['scale']*
                         (-log(1-0.01))^(1/weib$estimate['shape']))
    cdf100 <- as.numeric(weib$estimate['scale']*
                           (-log(1-0.99))^(1/weib$estimate['shape']))
    
    added_vec <- sort(append(observations, values = c(cdf0, cdf100)),
                      decreasing = FALSE)
    cdfadded <- 1 - exp(-(added_vec/weib$estimate['scale'])
                        ^weib$estimate['shape'])
    return(added_vec)
  }
  
  create_predict_df <- function(observations){
    
    added_vec <- create_cdf_ends(observations)
    vec_start <- min(added_vec)
    vec_end <- max(added_vec)
    new_vec <- seq(from = vec_start, to = vec_end, by = 0.5)
    
    weib <- fitdistrplus::fitdist(observations, distr = "weibull",
                                  method = "mle")
    cdfadded <- 1 - exp(-(new_vec/weib$estimate['scale'])
                        ^weib$estimate['shape'])
    
    cdf_df <- data.frame(x = new_vec, y = cdfadded)
    ends <- data.frame(x = c(min(added_vec - 1),
                             max(added_vec+ 1)), y = c(-0.001,1.001))
    cdf_df <- rbind(cdf_df, ends)
    cdf_df <- cdf_df[order(cdf_df$x, decreasing = FALSE),]
    
    return(cdf_df)
    
  }
  
  get_theta_hat_i <- function(observations, percentile = percentile){
    
    emptyvec <- vector(mode = "numeric", length = length(observations))
    
    for(i in seq_along(observations)){
      df1 <- create_predict_df(observations)
      sim_vector <- stats::runif(n = length(observations),min = 0, max = 1)
      df2 <- data.frame(x = observations, y = sim_vector[i])
      emptyvec[i] <- curve_intersect(df1, df2)$x
    }
    
    new_vector <- sort(emptyvec, decreasing = FALSE)
    
    new_df1 <- create_predict_df(new_vector)
    
    new_df2 <- data.frame(x = observations, y = percentile)
    
    theta_hat_i <- curve_intersect(new_df1, new_df2)$x
    
    return(theta_hat_i)
  }
  
  theta_hat_df <- data.frame(x = observations, y = percentile)
  
  theta_hat <- curve_intersect(create_predict_df(observations),
                               theta_hat_df)[['x']]
  
  bias <- mean(replicate(n = iterations,
                         expr = get_theta_hat_i(observations = observations,
                                                percentile = percentile)))
  
  2 * theta_hat - bias
}

# load in unimodal distributions

source("simulation_setup/unimodal_skewed_sampling_setup.R")

# lapply functions

onsetestimator_noci <- function(x){
  
  onset <- weib_percentile(observations = x, iterations = 500, percentile = 0)
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
  
  first <- weib_percentile(observations = x, iterations = 500, percentile = 0.01)
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
  
  fifth <- weib_percentile(observations = x, iterations = 500, percentile = 0.05)
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
  
  tenth <- weib_percentile(observations = x, iterations = 500, percentile = 0.1)
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
  
  fifty <- weib_percentile(observations = x, iterations = 500, percentile = 0.5)
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

# lapply functions

nintyestimator_noci <- function(x){
  
  ninty <- weib_percentile(observations = x, iterations = 500, percentile = 0.9)
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
  mutate(estimator = "belitz")

ninty20obs_10sd_df <- ninty20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 212.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

ninty50obs_10sd_df <- ninty50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 212.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

ninty10obs_20sd_df <- ninty10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

ninty20obs_20sd_df <- ninty20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

ninty50obs_20sd_df <- ninty50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 225.60) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

ninty10obs_40sd_df <- ninty10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

ninty20obs_40sd_df <- ninty20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

ninty50obs_40sd_df <- ninty50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 251.79) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzninty_df <- plyr::rbind.fill(ninty10obs_10sd_df, ninty10obs_20sd_df, ninty10obs_40sd_df,
                                   ninty20obs_10sd_df, ninty20obs_20sd_df, ninty20obs_40sd_df,
                                   ninty50obs_10sd_df, ninty50obs_20sd_df, ninty50obs_40sd_df)%>% 
  mutate(perc = "ninetieth", Q = 90)


# lapply functions

nintyfiveestimator_noci <- function(x){
  
  nintyfive <- weib_percentile(observations = x, iterations = 500, percentile = 0.95)
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
  mutate(estimator = "belitz")

nintyfive20obs_10sd_df <- nintyfive20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 216.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

nintyfive50obs_10sd_df <- nintyfive50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 216.19) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

nintyfive10obs_20sd_df <- nintyfive10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

nintyfive20obs_20sd_df <- nintyfive20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

nintyfive50obs_20sd_df <- nintyfive50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 232.76) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

nintyfive10obs_40sd_df <- nintyfive10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

nintyfive20obs_40sd_df <- nintyfive20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

nintyfive50obs_40sd_df <- nintyfive50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 266.36) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitznintyfifth_df <- plyr::rbind.fill(nintyfive10obs_10sd_df, nintyfive10obs_20sd_df, nintyfive10obs_40sd_df,
                                       nintyfive20obs_10sd_df, nintyfive20obs_20sd_df, nintyfive20obs_40sd_df,
                                       nintyfive50obs_10sd_df, nintyfive50obs_20sd_df, nintyfive50obs_40sd_df)%>% 
  mutate(perc = "ninetyfifth", Q = 95)

# lapply functions

nintynineestimator_noci <- function(x){
  
  nintynine <- weib_percentile(observations = x, iterations = 500, percentile = 0.99)
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
  mutate(estimator = "belitz")

nintynine20obs_10sd_df <- nintynine20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 223.52) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

nintynine50obs_10sd_df <- nintynine50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 223.52) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

nintynine10obs_20sd_df <- nintynine10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

nintynine20obs_20sd_df <- nintynine20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

nintynine50obs_20sd_df <- nintynine50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 246.44) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

nintynine10obs_40sd_df <- nintynine10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

nintynine20obs_40sd_df <- nintynine20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

nintynine50obs_40sd_df <- nintynine50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 293.43) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitznintynine_df <- plyr::rbind.fill(nintynine10obs_10sd_df, nintynine10obs_20sd_df, nintynine10obs_40sd_df,
                                       nintynine20obs_10sd_df, nintynine20obs_20sd_df, nintynine20obs_40sd_df,
                                       nintynine50obs_10sd_df, nintynine50obs_20sd_df, nintynine50obs_40sd_df) %>% 
  mutate(perc = "ninetynineth", Q = 99)

# lapply functions

offsetestimator_noci <- function(x){
  
  offset <- weib_percentile(observations = x, iterations = 500, percentile = 1)
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
  mutate(estimator = "belitz")

offset20obs_10sd_df <- offset20obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 238.1) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 10) %>% 
  mutate(estimator = "belitz")

offset50obs_10sd_df <- offset50obs_10sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 238.1) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 10) %>% 
  mutate(estimator = "belitz")

# 20 sd

offset10obs_20sd_df <- offset10obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 20) %>% 
  mutate(estimator = "belitz")

offset20obs_20sd_df <- offset20obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 20) %>% 
  mutate(estimator = "belitz")

offset50obs_20sd_df <- offset50obs_20sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 279.34) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 20) %>% 
  mutate(estimator = "belitz")

# 40 sd

offset10obs_40sd_df <- offset10obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 10, sd = 40) %>% 
  mutate(estimator = "belitz")

offset20obs_40sd_df <- offset20obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "belitz")

offset50obs_40sd_df <- offset50obs_40sd_df %>% 
  rename(estimate = X1) %>% 
  mutate(true_value = 361.38) %>% 
  mutate(distance = estimate - true_value) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "belitz")

# rbind these together

belitzoffset_df <- plyr::rbind.fill(offset10obs_10sd_df, offset10obs_20sd_df, offset10obs_40sd_df,
                                    offset20obs_10sd_df, offset20obs_20sd_df, offset20obs_40sd_df,
                                    offset50obs_10sd_df, offset50obs_20sd_df, offset50obs_40sd_df)%>% 
  mutate(perc = "offset", Q = 100)

unimodal_sims_all <- plyr::rbind.fill(belitzonset_df, belitzfirst_df, belitzfifth_df, belitztenth_df,
                                      belitzfifty_df, belitzninty_df, belitznintyfifth_df, belitznintynine_df,
                                      belitzoffset_df)


write.csv(unimodal_sims_all, file = "results/unimodal_skewed_phenesse.csv", row.names = FALSE)
