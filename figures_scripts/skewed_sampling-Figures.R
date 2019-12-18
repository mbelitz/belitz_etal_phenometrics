# Libraries

library(Metrics)
library(dplyr)
library(ggplot2)

# read in simulation results, uni_sims and bi_sims

uni_phenesse_sims <- read.csv("results/unimodal_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")
uni_skewed_phenesse_sims <- read.csv("results/unimodal_skewed_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse - Skewed")


# group by and summarize to get bias and rmse

total_uni <- rbind( uni_phenesse_sims, uni_skewed_phenesse_sims)

total_skewed_uni_metrics <- total_uni %>%  
  group_by(Estimator, perc, obs, Q, sd) %>% 
  summarize(RMSE = rmse(actual = true_value, predicted = estimate), 
            Bias = bias(actual = true_value, predicted = estimate)) 

total_skewed_uni_metrics$sd <- as.character(total_skewed_uni_metrics$sd)
total_skewed_uni_metrics$obs <- as.character(total_skewed_uni_metrics$obs)
total_skewed_uni_metrics$Q <- as.character(total_skewed_uni_metrics$Q)
total_skewed_uni_metrics <- total_skewed_uni_metrics %>% 
  mutate(fac_Q = ifelse(Q == 50.50, "Mean", Q))

total_skewed_uni_metrics_barplot <- total_skewed_uni_metrics %>% 
  ungroup(fac_Q) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(sd = paste(sd, "sd")) %>% 
  mutate(obs = paste(obs, "Observations")) 



##### UNIMODAL RMSE PLOT #########

um_skewed_rmse <- ggplot(total_skewed_uni_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Unimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'RMSE') + 
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5)) 

um_skewed_rmse
