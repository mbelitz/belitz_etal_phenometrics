# Libraries
library(Metrics)
library(dplyr)
library(ggplot2)
library(cowplot)

# read in unimodal simulation results

uni_quantile_sims <- read.csv("results/unimodal_quantile.csv", 
                              stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile') %>% 
  dplyr::mutate(Skewed = "No")
uni_skewed_quantile_sims <- read.csv("results/unimodal_skewed_quantile.csv",
                                     stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile') %>% 
  dplyr::mutate(Skewed = "Yes")

uni_phest_sims <- read.csv("results/unimodal_phest.csv", 
                           stringsAsFactors = FALSE) %>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest')%>% 
  dplyr::mutate(Skewed = "No")
uni_skewed_phest_sims <- read.csv("results/unimodal_skewed_phest.csv",
                                  stringsAsFactors = FALSE)%>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest')%>% 
  dplyr::mutate(Skewed = "Yes")

uni_mean_sims <- read.csv("results/unimodal_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean')%>% 
  dplyr::mutate(Skewed = "No")
uni_skewed_mean_sims <- read.csv("results/unimodal_skewed_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean')%>% 
  dplyr::mutate(Skewed = "Yes")

uni_phenesse_sims <- read.csv("results/unimodal_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")%>% 
  dplyr::mutate(Skewed = "No")
uni_skewed_phenesse_sims <- read.csv("results/unimodal_skewed_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")%>% 
  dplyr::mutate(Skewed = "Yes")


# group by and summarize to get bias and rmse

total_uni <- rbind(uni_mean_sims, uni_skewed_mean_sims, uni_phest_sims, uni_skewed_phest_sims,
                   uni_quantile_sims, uni_skewed_quantile_sims, uni_phenesse_sims, uni_skewed_phenesse_sims)

total_skewed_uni_metrics <- total_uni %>%  
  group_by(Estimator, perc, obs, Q, sd, Skewed) %>% 
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

### only random

um_random_rmse_data <- total_skewed_uni_metrics_barplot %>% 
  dplyr::filter(Skewed == "No") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))

##### UNIMODAL RANDOM RMSE PLOT #########

um_rmse <- ggplot(um_random_rmse_data) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Random Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'RMSE') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

##### UNIMODAL BIAS PLOT #########

um_bias <- ggplot(um_random_rmse_data) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Random Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

### Now only do skewed

um_skewed_rmse_data <- total_skewed_uni_metrics_barplot %>% 
  dplyr::filter(Skewed == "Yes") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))

um_rmse_skewed <- ggplot() + 
  geom_bar(data = um_skewed_rmse_data, aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Skewed Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  facet_grid(sd~obs) +
  labs(x = "Percentile", y = 'RMSE') + 
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


um_bias_skewed <- ggplot(um_skewed_rmse_data) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Skewed Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_grid(sd~obs, labeller = labeller()) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# join Unimodal RMSE plots

comb_um_rmse <- cowplot::plot_grid(um_rmse, um_rmse_skewed, nrow = 2)

ggsave(filename = "figures_outputs/S1.png", dpi = 300, width = 10,
       height = 12)

# Unimodal bias

comb_um_bias <- cowplot::plot_grid(um_bias, um_bias_skewed, nrow = 2)

ggsave(filename = "figures_outputs/S2.png", dpi = 300, width = 10,
       height = 12)
