library(tidyverse)

# read in phenometric results
cc_12559 <- read.csv("empirical_data/empirical_results/cercis_canadensis_12559.csv", stringsAsFactors = FALSE) %>% 
  mutate(scientificName = "Cercis canadensis",
         hex_ids = 12559)
cc_10566 <- read.csv("empirical_data/empirical_results/cercis_canadensis_10566.csv", stringsAsFactors = FALSE) %>% 
  mutate(scientificName = "Cercis canadensis",
         hex_ids = 10566)

as_8896 <- read.csv("empirical_data/empirical_results/asclepias_syriaca_8896.csv", stringsAsFactors = FALSE) %>% 
  mutate(scientificName = "Asclepias syriaca",
         hex_ids = 8896)

# combine dataframes and add in RMSE and Bias calculations

total_results <- rbind(cc_10566, cc_12559, as_8896)

calc_rmse <- total_results %>% 
  group_by(percentile) %>% 
  summarise(phen_rmse = Metrics::rmse(actual = benchmark, predicted = phenes_est),
            q_rmse = Metrics::rmse(actual = benchmark, predicted = quan_est),
            phest_rmse = Metrics::rmse(actual = benchmark, predicted = phest_est),
            mean_rmse = Metrics::rmse(actual = benchmark, predicted = mean_est))

calc_bias <- total_results %>% 
  group_by(percentile) %>% 
  summarise(phen_bias = Metrics::bias(actual = benchmark, predicted = phenes_est),
            q_bias = Metrics::bias(actual = benchmark, predicted = quan_est),
            phest_bias = Metrics::bias(actual = benchmark, predicted = phest_est),
            mean_bias = Metrics::bias(actual = benchmark, predicted = mean_est))

rmse_long <- calc_rmse %>% 
  gather(Estimator, RMSE, -percentile) %>% 
  mutate(fac_Q = case_when(percentile == "0" ~ "0",
                           percentile == "0.01" ~ "1",
                           percentile == "0.05" ~ "5",
                           percentile == "0.1"~ "10",
                           percentile == "0.5"~ "50",
                           percentile == "mean" ~ "Mean",
                           percentile == "0.9" ~ "90",
                           percentile == "0.95" ~ "95",
                           percentile == "0.99" ~ "99",
                           percentile == "1" ~ "100")) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(Estimator = case_when(Estimator == "phen_rmse" ~ "Phenesse",
                               Estimator == "q_rmse" ~ "Quantile",
                               Estimator == "mean_rmse" ~ "Mean",
                               Estimator == "phest_rmse" ~ "Phest")) %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  "Mean",
                                                  'Phest'))) %>% 
  na.omit()


bias_long <- calc_bias %>% 
  gather(Estimator, bias, -percentile) %>% 
  mutate(fac_Q = case_when(percentile == "0" ~ "0",
                           percentile == "0.01" ~ "1",
                           percentile == "0.05" ~ "5",
                           percentile == "0.1"~ "10",
                           percentile == "0.5"~ "50",
                           percentile == "mean" ~ "Mean",
                           percentile == "0.9" ~ "90",
                           percentile == "0.95" ~ "95",
                           percentile == "0.99" ~ "99",
                           percentile == "1" ~ "100")) %>% 
  mutate(fac_Q = factor(fac_Q, levels = c('0','1','5','10','50',"Mean",'90','95','99','100'))) %>% 
  mutate(Estimator = case_when(Estimator == "phen_bias" ~ "Phenesse",
                               Estimator == "q_bias" ~ "Quantile",
                               Estimator == "mean_bias" ~ "Mean",
                               Estimator == "phest_bias" ~ "Phest")) %>% 
  mutate(Estimator = factor(Estimator, levels = c('Phenesse', 
                                                  'Quantile', 
                                                  "Mean",
                                                  'Phest'))) %>% 
  na.omit()


# plot the results

flower_rmse <- ggplot() +
  geom_bar(rmse_long, mapping = aes(x = fac_Q, y = RMSE, fill = Estimator),
           stat = "identity", size = 1, alpha = 0.8, position = position_dodge(preserve = "total")) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Open Flowers") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  labs(x = "Percentile", y = 'RMSE') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

flower_bias <- ggplot() +
  geom_bar(bias_long, mapping = aes(x = fac_Q, y = bias, fill = Estimator),
           stat = "identity", size = 1, alpha = 0.8, position = position_dodge(preserve = "total")) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ggtitle("Open Flowers") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF","#FDE725FF", "#73D055FF")) +
  labs(x = "Percentile", y = 'Bias') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 


library(tidyverse)

## Read in monarch data 

te <- read.csv("empirical_data/inat_monarchs/total_estimates.csv", stringsAsFactors = FALSE)

te4 <- te %>% 
  filter(q_error >= 0) %>% 
  filter(inat_obs >= 10) %>% 
  filter(jn_obs >=2)

mon_error <- ggplot (te4) +
  geom_point(aes(x = inat_obs, y = q_error * -1, color = "Quantile")) +
  geom_smooth(aes(x = inat_obs, y = q_error * -1, color = "Quantile"), se = FALSE, method = "lm") +
  geom_point(aes(x = inat_obs, y = phest_error * -1, color = "Phest")) +
  geom_smooth(aes(x = inat_obs, y = phest_error * -1, color = "Phest"), se = FALSE, method = "lm") +
  geom_point(aes(x = inat_obs, y = phenesse_error * -1, color = "Phenesse")) +
  geom_smooth(aes(x = inat_obs, y = phenesse_error * -1, color = "Phenesse"), se = FALSE, method = "lm") +
  scale_x_log10() +
  labs(x = "Number of Observations", y = "Error", color = "Legend") +
  scale_color_manual(values = c("#440154FF", "#73D055FF", "#287D8EFF")) +
  theme_bw() 

ggsave("figures_outputs/empirical_outputs/monarch_error_by_obs.png", device = "png",
       height = 6, width = 8)

## RMSE AND BIAS calculation

rmse_bias <- te4 %>% 
  mutate(q_rmse = Metrics::rmse(actual = benchmark, predicted = quan_est),
         phen_rmse = Metrics::rmse(actual = benchmark, predicted = phen_est),
         phest_rmse = Metrics::rmse(actual = benchmark, predicted = phest_est),
         q_bias = Metrics::bias(actual = benchmark, predicted = quan_est),
         phen_bias = Metrics::bias(actual = benchmark, predicted = phen_est),
         phest_bias = Metrics::bias(actual = benchmark, predicted = phest_est)) %>% 
  distinct(q_rmse, phen_rmse, phest_rmse,
           q_bias, phen_bias, phest_bias)

rmse_bias2 <- data.frame(estimator = c("Quantile", "Phenesse", "Phest"),
                         rmse = c(rmse_bias$q_rmse, rmse_bias$phen_rmse, rmse_bias$phest_rmse),
                         bias = c(rmse_bias$q_bias, rmse_bias$phen_bias, rmse_bias$phest_bias)) %>% 
  mutate(Estimator = estimator)


mon_rmse <- ggplot(rmse_bias2) +
  geom_bar(mapping = aes(x = Estimator, y = rmse, fill = Estimator),
           stat = "identity", size = 1, alpha = 0.8, position = position_dodge(preserve = "total")) +
  ggtitle("First Monarch Observered") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  labs(x = "Estimator", y = 'RMSE') + 
  scale_fill_manual(values = c("#440154FF", "#73D055FF", "#287D8EFF")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

mon_bias <- ggplot(rmse_bias2) +
  geom_bar(mapping = aes(x = Estimator, y = bias, fill = Estimator),
           stat = "identity", size = 1, alpha = 0.8, position = position_dodge(preserve = "total")) +
  ggtitle("First Monarch Observered") + 
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  labs(x = "Estimator", y = 'Bias') + 
  scale_fill_manual(values = c("#440154FF", "#73D055FF", "#287D8EFF")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.75, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 


cp <- cowplot::plot_grid(flower_rmse, flower_bias, mon_rmse, mon_bias, ncol = 2, 
                   labels = c("A", "B", "C", "D"))

ggsave(filename = "figures_outputs/empirical_outputs/empirical_results.png", plot = cp,
       width = 11, height = 8)
