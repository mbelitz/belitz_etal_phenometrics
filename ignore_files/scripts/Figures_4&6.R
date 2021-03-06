library(dplyr)
library(cowplot)

## Plot only 20 obs RMSE & Bias

bm_skewed_rmse_data_20obs <- bi_skewed_rmse_data %>% 
  filter(obs == "20 Observations")

bm_random_rmse_data_20obs <- total_skewed_bi_metrics_barplot %>% 
  filter(obs == "20 Observations") %>% 
  filter(Skewed == "No") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))

####### Skewed Unimodal RMSE PLOT ###########

sk_bm_rmse_20obs <- ggplot() + 
  geom_bar(data = bm_skewed_rmse_data_20obs, aes(x = fac_Q, y = RMSE, fill = Estimator), 
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

sk_bm_bias_20obs <- ggplot(bm_skewed_rmse_data_20obs) + 
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

rando_bm_rmse_20obs <- ggplot(bm_random_rmse_data_20obs) + 
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

rando_bm_bias_20obs <- ggplot(bm_random_rmse_data_20obs) + 
  geom_bar(aes(x = fac_Q, y = Bias, fill = Estimator), 
           stat = "identity", size = 1, alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  scale_y_continuous(breaks = c(-40,-20,0,20,40,60), limits = c(-50,50)) +
  ggtitle("Random Sampling") + 
  theme_bw() +
  facet_wrap(~sd) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 


########### Combine Unimodal RMSE and Bias Figures for 20 observation scenario ##########

bm_rmse_combo <- cowplot::plot_grid(rando_bm_rmse_20obs, sk_bm_rmse_20obs, nrow = 2)

bm_bias_combo <- cowplot::plot_grid(rando_bm_bias_20obs, sk_bm_bias_20obs, nrow = 2)

# save the plot

ggplot2::ggsave("figures_outputs/combined_bm_rmse_20obs.png", plot = bm_rmse_combo, width = 8, height = 6, dpi = 300)
ggplot2::ggsave("figures_outputs/combined_bm_bias_20obs.png", plot = bm_bias_combo, width = 8, height = 6, dpi = 300)
