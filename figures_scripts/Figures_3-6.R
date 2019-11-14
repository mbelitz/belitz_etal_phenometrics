# Libraries

library(Metrics)
library(dplyr)
library(ggplot2)

# read in simulation results, uni_sims and bi_sims

uni_phenesse_sims <- read.csv("results/unimodal_phenesse.csv", 
                              stringsAsFactors = FALSE)
bi_phenesse_sims <- read.csv("results/bimodal_phenesse.csv", 
                             stringsAsFactors = FALSE)

uni_quantile_sims <- read.csv("results/unimodal_quantile.csv", 
                           stringsAsFactors = FALSE)
bi_quantile_sims <- read.csv("results/bimodal_quantile.csv",
                          stringsAsFactors = FALSE)

uni_phest_sims <- read.csv("results/unimodal_phest.csv", 
                           stringsAsFactors = FALSE) %>% 
  dplyr::select(-lowCI, -highCI)
bi_phest_sims <- read.csv("results/bimodal_phest.csv",
                          stringsAsFactors = FALSE)%>% 
  dplyr::select(-lowCI, -highCI)

uni_mean_sims <- read.csv("results/unimodal_mean.csv", stringsAsFactors = FALSE)
bi_mean_sims <- read.csv("results/bimodal_mean.csv", stringsAsFactors = FALSE)

# Combine phenesse simulation results with naive simulation results

total_uni <- rbind(uni_phenesse_sims, uni_quantile_sims, uni_phest_sims, uni_mean_sims)
total_bi <- rbind(bi_phenesse_sims, bi_quantile_sims, bi_phest_sims, bi_mean_sims)

# group by and summarize to get bias and rmse

total_uni_metrics <- total_uni %>%  
  group_by(estimator, perc, obs, Q, sd) %>% 
  summarize(RMSE = rmse(actual = true_value, predicted = estimate), 
            Bias = bias(actual = true_value, predicted = estimate)) 

total_uni_metrics$sd <- as.character(total_uni_metrics$sd)
total_uni_metrics$obs <- as.character(total_uni_metrics$obs)
total_uni_metrics$Q <- as.character(total_uni_metrics$Q)
total_uni_metrics <- total_uni_metrics %>% 
  mutate(fac_Q = ifelse(Q == 50.50, "Mean", Q))

total_bi_metrics <- total_bi %>%  
  group_by(estimator, perc, obs,Q, sd) %>% 
  summarize(RMSE = rmse(actual = true_value, predicted = estimate), 
            Bias = bias(actual = true_value, predicted = estimate)) 
total_bi_metrics$sd <- as.character(total_bi_metrics$sd)
total_bi_metrics$obs <- as.character(total_bi_metrics$obs)
total_bi_metrics$Q <- as.character(total_bi_metrics$Q)
total_bi_metrics <- total_bi_metrics %>% 
  mutate(fac_Q = ifelse(Q == 50.50, "Mean", Q))

# prepare dataframe for barplot
total_uni_metrics_barplot <- total_uni_metrics %>% 
  ungroup(fac_Q) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(sd = paste(sd, "sd")) %>% 
  mutate(obs = paste(obs, "Observations")) %>% 
  mutate(Estimator = ifelse(estimator == "belitz", "Phenesse",
                            ifelse(estimator == "naive" , "Quantile",
                                   ifelse(estimator == "mean", "Mean",
                                          "Phest")))) %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 'Quantile', 'Mean','Phest')))

total_bi_metrics_barplot <- total_bi_metrics %>% 
  ungroup(fac_Q) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(sd = paste(sd, "sd")) %>% 
  mutate(obs = paste(obs, "Observations")) %>% 
  mutate(Estimator = ifelse(estimator == "belitz", "Phenesse",
                            ifelse(estimator == "naive" , "Quantile",
                                   ifelse(estimator == "mean", "Mean",
                                          "Phest")))) %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 'Quantile', 'Mean','Phest')))

##### UNIMODAL RMSE PLOT #########

um_rmse <- ggplot(total_uni_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Unimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'RMSE') + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot2::ggsave("figures_outputs/Fig3.png", plot = um_rmse, width = 10, height = 8, dpi = 300)

####### BIMODAL RMSE PLOT ############

bm_rmse <- ggplot(total_bi_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Bimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'RMSE') + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot2::ggsave("figures_outputs/Fig4.png", plot = bm_rmse, width = 10, height = 8, dpi = 300)

##### UNIMODAL BIAS PLOT #########

um_bias <- ggplot(total_uni_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Unimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot2::ggsave("figures_outputs/Fig5.png", plot = um_bias, width = 10, height = 8, dpi = 300)

####### BIMODAL BIAS PLOT ############

bm_bias <- ggplot(total_bi_metrics_barplot) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Bimodal Distribution") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot2::ggsave("figures_outputs/Fig6.png", plot = bm_bias, width = 10, height = 8, dpi = 300)