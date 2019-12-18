library(truncnorm)
library(ggplot2)

### Set up simulated bimodal distributions ###

nn <- 20000

#10 SD
set.seed(1)
sims_10sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=10),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=10))
sim_10sd_df <- as.data.frame(sims_10sd)
ggplot(data= sim_10sd_df, aes(x = sims_10sd)) + geom_histogram(bins = 150)
quantile(sims_10sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) 
#min = 113.29, 10% - 144.65, 50% - 213.21, #90% - 230.27, 100% - 257.28
mean(sims_10sd)
# 196.6 

# function to make weighted probs
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

# now create 100 lists per observation and standard deviation combination
sims_10sd_wp <- make_weigted_probs(sims_10sd)
set.seed(1)
rep_10obs_10sd <- replicate(n = 100, expr = sample(sims_10sd_wp$x, size = 10,
                                                   prob = sims_10sd_wp$y, replace = FALSE))
list_10obs_10sd <- split(rep_10obs_10sd, rep(1:ncol(rep_10obs_10sd), each = nrow(rep_10obs_10sd)))

set.seed(2)
rep_20obs_10sd <- replicate(n = 100, expr = sample(sims_10sd_wp$x, size = 20,
                                                   prob = sims_10sd_wp$y, replace = FALSE))
list_20obs_10sd <- split(rep_20obs_10sd, rep(1:ncol(rep_20obs_10sd), each = nrow(rep_20obs_10sd)))

set.seed(5)
rep_50obs_10sd <- replicate(n = 100, expr = sample(sims_10sd_wp$x, size = 50,
                                                   prob = sims_10sd_wp$y, replace = FALSE))
list_50obs_10sd <- split(rep_50obs_10sd, rep(1:ncol(rep_50obs_10sd), each = nrow(rep_50obs_10sd)))

#20 SD
set.seed(1)
sims_20sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=20),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=20))
sim_20sd_df <- as.data.frame(sims_20sd)
ggplot(data= sim_20sd_df, aes(x = sims_20sd)) + geom_histogram(bins = 150)
quantile(sims_20sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) 
#min = 76.58, 10% - 139.29, 50% - 206.48, #90% - 240.55, 100% - 294.56
mean(sims_20sd)
#196.56

set.seed(1)
sims_20sd_wp <- make_weigted_probs(sims_20sd)
rep_10obs_20sd <- replicate(n = 100, expr = sample(sims_20sd_wp$x, size = 10,
                                                   prob = sims_20sd_wp$y, replace = FALSE))
list_10obs_20sd <- split(rep_10obs_20sd, rep(1:ncol(rep_10obs_20sd), each = nrow(rep_10obs_20sd)))

set.seed(2)
rep_20obs_20sd <- replicate(n = 100, expr = sample(sims_20sd_wp$x, size = 20,
                                                   prob = sims_20sd_wp$y, replace = FALSE))
list_20obs_20sd <- split(rep_20obs_20sd, rep(1:ncol(rep_20obs_20sd), each = nrow(rep_20obs_20sd)))

set.seed(5)
rep_50obs_20sd <- replicate(n = 100, expr = sample(sims_20sd_wp$x, size = 50,
                                                   prob = sims_20sd_wp$y, replace = FALSE))
list_50obs_20sd <- split(rep_50obs_20sd, rep(1:ncol(rep_50obs_20sd), each = nrow(rep_50obs_20sd)))
