# Libraries

library(Metrics)
library(dplyr)
library(ggplot2)

# read in simulation results, bi_sims and bi_sims

bi_quantile_sims <- read.csv("results/bimodal_quantile.csv", 
                              stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile')
bi_skewed_quantile_sims <- read.csv("results/bimodal_skewed_quantile.csv",
                                     stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile - Skewed')

bi_phest_sims <- read.csv("results/bimodal_phest.csv", 
                           stringsAsFactors = FALSE) %>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest')
bi_skewed_phest_sims <- read.csv("results/bimodal_skewed_phest.csv",
                                  stringsAsFactors = FALSE)%>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest - Skewed')

bi_mean_sims <- read.csv("results/bimodal_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean')
bi_skewed_mean_sims <- read.csv("results/bimodal_skewed_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean - Skewed')

bi_phenesse_sims <- read.csv("results/bimodal_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")
bi_skewed_phenesse_sims <- read.csv("results/bimodal_skewed_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse - Skewed")


# group by and summarize to get bias and rmse

total_bi <- rbind(bi_mean_sims, bi_skewed_mean_sims, bi_phest_sims, bi_skewed_phest_sims,
                   bi_quantile_sims, bi_skewed_quantile_sims, bi_phenesse_sims, bi_skewed_phenesse_sims)

total_skewed_bi_metrics <- total_bi %>%  
  group_by(Estimator, perc, obs, Q, sd) %>% 
  summarize(RMSE = rmse(actual = true_value, predicted = estimate), 
            Bias = bias(actual = true_value, predicted = estimate)) 

total_skewed_bi_metrics$sd <- as.character(total_skewed_bi_metrics$sd)
total_skewed_bi_metrics$obs <- as.character(total_skewed_bi_metrics$obs)
total_skewed_bi_metrics$Q <- as.character(total_skewed_bi_metrics$Q)
total_skewed_bi_metrics <- total_skewed_bi_metrics %>% 
  mutate(fac_Q = ifelse(Q == 50.50, "Mean", Q))

total_skewed_bi_metrics_barplot <- total_skewed_bi_metrics %>% 
  ungroup(fac_Q) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(sd = paste(sd, "sd")) %>% 
  mutate(obs = paste(obs, "Observations")) 



##### bimodal RMSE PLOT #########

bi_skewed_rmse <- ggplot(total_skewed_bi_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("bimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'RMSE') + 
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5)) 

bi_skewed_rmse