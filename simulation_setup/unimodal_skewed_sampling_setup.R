# Make the skewed distribution analyses

# libraries
library(dplyr)
library(ggplot2)

### Set up simulated unimodal distributions ###


# set total number of individuals to be in our simulated "population"
nn <- 20000

#10 SD
set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)
ggplot(data= to_10sd, aes(x = obs_10sd)) + geom_histogram(bins = 150)
min(to_10sd) ## 199.8918
max(to_10sd) # 199.89258
quantile(obs_10sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) 
mean(obs_10sd) # 199.89

# function to make weighted probs - this is used to skew the sampling earlier
make_weigted_probs <- function(x){
  df <- data.frame(x = x, y = NA)
  q1 <- quantile(x, probs = 0.1)
  q2 <- quantile(x, probs = 0.5)
  m <- mean(x)
  df <- df %>% 
    mutate(y = ifelse(x < q1, 0.6, 
                      ifelse(x > q1 & x < m, 0.3, 0.1)))
  return(df)
}

# now create a list of 100 different vectors of 10, 20, and 50 observations
# these vectors are weighted to skew the sampling early as directed in the 
# make weighted probs function
obs_10sd_wp <- make_weigted_probs(obs_10sd)
set.seed(1)
rep_10obs_10sd <- replicate(n = 100, expr = sample(obs_10sd_wp$x, size = 10, 
                                                   prob = obs_10sd_wp$y, replace = FALSE))
list_10obs_10sd <- split(rep_10obs_10sd, rep(1:ncol(rep_10obs_10sd), each = nrow(rep_10obs_10sd)))

set.seed(2)
rep_20obs_10sd <- replicate(n = 100, expr = sample(obs_10sd_wp$x, size = 20, 
                                                   prob = obs_10sd_wp$y, replace = FALSE))
list_20obs_10sd <- split(rep_20obs_10sd, rep(1:ncol(rep_20obs_10sd), each = nrow(rep_20obs_10sd)))

set.seed(5)
rep_50obs_10sd <- replicate(n = 100, expr = sample(obs_10sd_wp$x, size = 50, 
                                                   prob = obs_10sd_wp$y, replace = FALSE))
list_50obs_10sd <- split(rep_50obs_10sd, rep(1:ncol(rep_50obs_10sd), each = nrow(rep_50obs_10sd)))


#20 SD
set.seed(365)
obs_20sd <- rnorm(20000, mean = 200, sd = 20)
to_20sd <- as.data.frame(obs_20sd)
ggplot(data= to_20sd, aes(x = obs_20sd)) + geom_histogram(bins = 150)
min(to_20sd) 
max(to_20sd) 
quantile(obs_20sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) 
mean(obs_20sd)
# 200.16

# now create a list of 100 different vectors of 10, 20, and 50 observations
# these vectors are weighted to skew the sampling early as directed in the 
# make weighted probs function
obs_20sd_wp <- make_weigted_probs(obs_20sd)
set.seed(1)
rep_10obs_20sd <- replicate(n = 100, expr = sample(obs_20sd_wp$x, size = 10, 
                                                   prob = obs_20sd_wp$y, replace = FALSE))
list_10obs_20sd <- split(rep_10obs_20sd, rep(1:ncol(rep_10obs_20sd), each = nrow(rep_10obs_20sd)))

set.seed(2)
rep_20obs_20sd <- replicate(n = 100, expr = sample(obs_20sd_wp$x, size = 20, 
                                                   prob = obs_20sd_wp$y, replace = FALSE))
list_20obs_20sd <- split(rep_20obs_20sd, rep(1:ncol(rep_20obs_20sd), each = nrow(rep_20obs_20sd)))

set.seed(5)
rep_50obs_20sd <- replicate(n = 100, expr = sample(obs_20sd_wp$x, size = 50, 
                                                   prob = obs_20sd_wp$y, replace = FALSE))
list_50obs_20sd <- split(rep_50obs_20sd, rep(1:ncol(rep_50obs_20sd), each = nrow(rep_50obs_20sd)))


#40 SD
set.seed(40)
obs_40sd <- rnorm(20000, mean = 200, sd = 40)
to_40sd <- as.data.frame(obs_40sd)
ggplot(data= to_40sd, aes(x = obs_40sd)) + geom_histogram(bins = 150)
min(to_40sd) 
max(to_40sd)
quantile(obs_40sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1))
mean(obs_40sd)

# now create a list of 100 different vectors of 10, 20, and 50 observations
# these vectors are weighted to skew the sampling early as directed in the 
# make weighted probs function
obs_40sd_wp <- make_weigted_probs(obs_40sd)
set.seed(1)
rep_10obs_40sd <- replicate(n = 100, expr = sample(obs_40sd_wp$x, size = 10, 
                                                   prob = obs_40sd_wp$y, replace = FALSE))
list_10obs_40sd <- split(rep_10obs_40sd, rep(1:ncol(rep_10obs_40sd), each = nrow(rep_10obs_40sd)))

set.seed(2)
rep_20obs_40sd <- replicate(n = 100, expr = sample(obs_40sd_wp$x, size = 20, 
                                                   prob = obs_40sd_wp$y, replace = FALSE))
list_20obs_40sd <- split(rep_20obs_40sd, rep(1:ncol(rep_20obs_40sd), each = nrow(rep_20obs_40sd)))

set.seed(5)
rep_50obs_40sd <- replicate(n = 100, expr = sample(obs_40sd_wp$x, size = 50, 
                                                   prob = obs_40sd_wp$y, replace = FALSE))
list_50obs_40sd <- split(rep_50obs_40sd, rep(1:ncol(rep_50obs_40sd), each = nrow(rep_50obs_40sd)))