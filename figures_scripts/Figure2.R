# Load Libraries
library(dplyr)
library(Metrics)
library(ggplot2)
library(gridExtra)
library(truncnorm)

# Set up data for unimodal distributions

set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)

set.seed(365)
obs_20sd <- rnorm(20000, mean = 200, sd = 20)
to_20sd <- as.data.frame(obs_20sd)

set.seed(40)
obs_40sd <- rnorm(20000, mean = 200, sd = 40)
to_40sd <- as.data.frame(obs_40sd)

# plot unimodal distributions

tensd <- ggplot(data= to_10sd, aes(x = obs_10sd)) + 
  geom_histogram(bins = 250) + 
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("10 SD") +
  scale_y_continuous(expand = c(0,0))+ 
  scale_x_continuous(limits = c(0,365))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
twentysd <- ggplot(data= to_20sd, aes(x = obs_20sd)) + 
  geom_histogram(bins = 250) + 
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("20 SD") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0,365))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
fortysd <- ggplot(data= to_40sd, aes(x = obs_40sd)) + 
  geom_histogram(bins = 250) + 
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("40 SD") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0,365))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

######## Bimodal distributions now ############

# set up distributions

nn <- 20000

#10 SD
set.seed(1)
sims_10sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=10),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=10))
sim_10sd_df <- as.data.frame(sims_10sd)

#20 SD
set.seed(1)
sims_20sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=20),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=20))
sim_20sd_df <- as.data.frame(sims_20sd)

# plot bimodal distributions

bm_dist_10sd <- ggplot(data= sim_10sd_df, aes(x = sims_10sd)) + 
  geom_histogram(bins = 250) + 
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(limits = c(0,365))+
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("10 SD") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

bm_dist_20sd <- ggplot(data= sim_20sd_df, aes(x = sims_20sd)) + 
  geom_histogram(bins = 250) + 
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(limits = c(0,365))+
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("20 SD") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

layout <- rbind(c(1,2,3),
                c(4,5,NA))

total_dist <- grid.arrange(tensd, twentysd, fortysd,
                           bm_dist_10sd, bm_dist_20sd, layout_matrix = layout)

ggsave("figures_outputs/Fig2.png", plot = total_dist, dpi = 300, width = 6, height = 4)
