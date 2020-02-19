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
### Now only do skewed

um_skewed_rmse_data <- total_skewed_uni_metrics_barplot %>% 
  dplyr::filter(Skewed == "Yes") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))

## Plot only 20 obs RMSE & Bias

um_skewed_rmse_data_20obs <- um_skewed_rmse_data %>% 
  filter(obs == "20 Observations")

um_random_rmse_data_20obs <- total_skewed_uni_metrics_barplot %>% 
  filter(obs == "20 Observations") %>% 
  filter(Skewed == "No") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))

####### Skewed Unimodal RMSE PLOT ###########

sk_um_rmse_20obs <- ggplot() + 
  geom_bar(data = um_skewed_rmse_data_20obs, aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Skewed Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  facet_wrap(~sd) +
  labs(x = "Percentile", y = 'RMSE') + 
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

##### Skewed UNIMODAL BIAS PLOT #########

sk_um_bias_20obs <- ggplot(um_skewed_rmse_data_20obs) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Skewed Sampling") + 
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_wrap(~sd) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

#### Random Sampling Unimodal RMSE Plot ##########

rando_um_rmse_20obs <- ggplot(um_random_rmse_data_20obs) + 
  geom_bar(aes(x = fac_Q, y = RMSE, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  ggtitle("Random Sampling") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  facet_wrap(~sd) +
  labs(x = "Percentile", y = 'RMSE') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

#### Random Sampling Unimodal bias plot ##########

rando_um_bias_20obs <- ggplot(um_random_rmse_data_20obs) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Random Sampling") + 
  theme_bw() +
  facet_wrap(~sd) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

########### Combine Unimodal RMSE and Bias Figures for 20 observation scenario ##########

um_rmse_combo <- cowplot::plot_grid(rando_um_rmse_20obs, sk_um_rmse_20obs, nrow = 2) +
  cowplot::draw_plot_label(label = "A") + 
  cowplot::draw_plot_label(label = "B", vjust = 19.5)

um_bias_combo <- cowplot::plot_grid(rando_um_bias_20obs, sk_um_bias_20obs, nrow = 2)+
  cowplot::draw_plot_label(label = "A") + 
  cowplot::draw_plot_label(label = "B", vjust = 19.5)

# save the plot

ggplot2::ggsave("figures_outputs/Fig3.png", plot = um_rmse_combo, width = 8, height = 6, dpi = 300)
ggplot2::ggsave("figures_outputs/Fig4.png", plot = um_bias_combo, width = 8, height = 6, dpi = 300)

