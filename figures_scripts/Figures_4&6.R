# Libraries
library(Metrics)
library(dplyr)
library(ggplot2)
library(cowplot)

# read in bimodal simulation results

bi_quantile_sims <- read.csv("results/bimodal_quantile.csv", 
                             stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile')%>% 
  dplyr::mutate(Skewed = "No")
bi_skewed_quantile_sims <- read.csv("results/bimodal_skewed_quantile.csv",
                                    stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Quantile')%>% 
  dplyr::mutate(Skewed = "Yes")

bi_phest_sims <- read.csv("results/bimodal_phest.csv", 
                          stringsAsFactors = FALSE) %>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest')%>% 
  dplyr::mutate(Skewed = "No")
bi_skewed_phest_sims <- read.csv("results/bimodal_skewed_phest.csv",
                                 stringsAsFactors = FALSE)%>% 
  dplyr::select(-lowCI, -highCI) %>% 
  dplyr::mutate(Estimator = 'Phest')%>% 
  dplyr::mutate(Skewed = "Yes")

bi_mean_sims <- read.csv("results/bimodal_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean')%>% 
  dplyr::mutate(Skewed = "No")
bi_skewed_mean_sims <- read.csv("results/bimodal_skewed_mean.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = 'Mean')%>% 
  dplyr::mutate(Skewed = "Yes")

bi_phenesse_sims <- read.csv("results/bimodal_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")%>% 
  dplyr::mutate(Skewed = "No")
bi_skewed_phenesse_sims <- read.csv("results/bimodal_skewed_phenesse.csv", stringsAsFactors = FALSE) %>% 
  dplyr::mutate(Estimator = "Phenesse")%>% 
  dplyr::mutate(Skewed = "Yes")


# group by and summarize to get bias and rmse

# join all the results together, both skewed and normal sampling results
total_bi <- rbind(bi_mean_sims, bi_skewed_mean_sims, bi_phest_sims, bi_skewed_phest_sims,
                  bi_quantile_sims, bi_skewed_quantile_sims, bi_phenesse_sims, bi_skewed_phenesse_sims)

# calculate RMSE & BIAS values
total_skewed_bi_metrics <- total_bi %>%  
  group_by(Estimator, perc, obs, Q, sd, Skewed) %>% 
  summarize(RMSE = rmse(actual = true_value, predicted = estimate), 
            Bias = bias(actual = true_value, predicted = estimate)) 

# make variables structured as character for graphing purposes
total_skewed_bi_metrics$sd <- as.character(total_skewed_bi_metrics$sd)
total_skewed_bi_metrics$obs <- as.character(total_skewed_bi_metrics$obs)
total_skewed_bi_metrics$Q <- as.character(total_skewed_bi_metrics$Q)
total_skewed_bi_metrics <- total_skewed_bi_metrics %>% 
  mutate(fac_Q = ifelse(Q == 50.50, "Mean", Q))

# add levels to fac_Q column and add different wording to sd and observations
total_skewed_bi_metrics_barplot <- total_skewed_bi_metrics %>% 
  ungroup(fac_Q) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(sd = paste(sd, "sd")) %>% 
  mutate(obs = paste(obs, "Observations")) 

### Get Random sampling data ready to plot

bm_random_rmse_data <- total_skewed_bi_metrics_barplot %>% 
  dplyr::filter(Skewed == "No") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))
### Now only do skewed

bi_skewed_rmse_data <- total_skewed_bi_metrics_barplot %>% 
  dplyr::filter(Skewed == "Yes") %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  'Mean',
                                                  'Phest')))


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

####### Skewed Bimodal RMSE PLOT ###########

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

##### Skewed Bimodal BIAS PLOT #########

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

#### Random Sampling Bimodal RMSE Plot ##########

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

#### Random Sampling Bimodal bias plot ##########

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


########### Combine Bimodal RMSE and Bias Figures for 20 observation scenario ##########

bm_rmse_combo <- cowplot::plot_grid(rando_bm_rmse_20obs, sk_bm_rmse_20obs, nrow = 2) +
  cowplot::draw_plot_label(label = "A") + 
  cowplot::draw_plot_label(label = "B", vjust = 19.5)

bm_bias_combo <- cowplot::plot_grid(rando_bm_bias_20obs, sk_bm_bias_20obs, nrow = 2) +
  cowplot::draw_plot_label(label = "A") + 
  cowplot::draw_plot_label(label = "B", vjust = 19.5)

# save the plot

ggplot2::ggsave("figures_outputs/Fig4.png", plot = bm_rmse_combo, width = 8, height = 6, dpi = 300)
ggplot2::ggsave("figures_outputs/Fig6.png", plot = bm_bias_combo, width = 8, height = 6, dpi = 300)