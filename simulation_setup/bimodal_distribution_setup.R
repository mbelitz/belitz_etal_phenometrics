library(truncnorm)
library(ggplot2)

### Set up simulated bimodal distributions ###

# set total number of individuals to be in our simulated "population"
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

# now create a list of 100 different vectors of 10, 20, and 50 observations
set.seed(1)
rep_10obs_10sd <- replicate(n = 100, expr = sample(sims_10sd, size = 10, replace = FALSE))
list_10obs_10sd <- split(rep_10obs_10sd, rep(1:ncol(rep_10obs_10sd), each = nrow(rep_10obs_10sd)))

set.seed(2)
rep_20obs_10sd <- replicate(n = 100, expr = sample(sims_10sd, size = 20, replace = FALSE))
list_20obs_10sd <- split(rep_20obs_10sd, rep(1:ncol(rep_20obs_10sd), each = nrow(rep_20obs_10sd)))

set.seed(5)
rep_50obs_10sd <- replicate(n = 100, expr = sample(sims_10sd, size = 50, replace = FALSE))
list_50obs_10sd <- split(rep_50obs_10sd, rep(1:ncol(rep_50obs_10sd), each = nrow(rep_50obs_10sd)))

#20 SD
set.seed(1)
sims_20sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=20),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=20))
sim_20sd_df <- as.data.frame(sims_20sd)
ggplot(data= sim_20sd_df, aes(x = sims_20sd)) + geom_histogram(bins = 150)
quantile(sims_20sd, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) 
mean(sims_20sd)

# now create a list of 100 different vectors of 10, 20, and 50 observations
set.seed(1)
rep_10obs_20sd <- replicate(n = 100, expr = sample(sims_20sd, size = 10, replace = FALSE))
list_10obs_20sd <- split(rep_10obs_20sd, rep(1:ncol(rep_10obs_20sd), each = nrow(rep_10obs_20sd)))

set.seed(2)
rep_20obs_20sd <- replicate(n = 100, expr = sample(sims_20sd, size = 20, replace = FALSE))
list_20obs_20sd <- split(rep_20obs_20sd, rep(1:ncol(rep_20obs_20sd), each = nrow(rep_20obs_20sd)))

set.seed(5)
rep_50obs_20sd <- replicate(n = 100, expr = sample(sims_20sd, size = 50, replace = FALSE))
list_50obs_20sd <- split(rep_50obs_20sd, rep(1:ncol(rep_50obs_20sd), each = nrow(rep_50obs_20sd)))
