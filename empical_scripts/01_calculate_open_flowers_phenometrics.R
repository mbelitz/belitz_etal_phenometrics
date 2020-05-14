library(tidyverse)
library(mgcv)
library(lubridate)
library(phest)
library(phenesse)


# read in the cleaned inat and npn data
npn_c <- read.csv("empirical_data/cleaned_data/cleaned_npn_cercis.csv", stringsAsFactors = FALSE)
npn_a <- read.csv("empirical_data/cleaned_data/cleaned_npn_asclepias.csv", stringsAsFactors = FALSE)

inat_c <- read.csv("empirical_data/cleaned_data/cleaned_inat_cercis.csv", stringsAsFactors = FALSE) %>% 
  dplyr::select(gbifID, identifier, references, institutionID, recordedBy,
                eventDate, scientificName, hex_ids) %>% 
  dplyr::mutate(doy = yday(eventDate))
inat_a <- read.csv("empirical_data/cleaned_data/cleaned_inat_asclepias.csv", stringsAsFactors = FALSE) %>% 
  dplyr::select(gbifID, identifier, references, institutionID, recordedBy,
                eventDate, scientificName, hex_ids) %>% 
  dplyr::mutate(doy = yday(eventDate))

# read in the scored iNat data 
scored_c <- read.csv("empirical_data/scored_inat_images/c_canadensis.csv", stringsAsFactors = FALSE) %>% 
  mutate(identifier = as.numeric(stringr:::str_remove_all(file, "[.jpg]")))
scored_a <- read.csv("empirical_data/scored_inat_images/a_syriaca.csv", stringsAsFactors = FALSE) %>% 
  mutate(identifier = as.numeric(stringr:::str_remove_all(file, "[.jpg]")))

# remove records from iNat dataset that were not of open flowers
assy_inat <- left_join(inat_a, scored_a) %>% 
  dplyr::filter(flowers == 1)
ceca_inat <- left_join(inat_c, scored_c) %>% 
  dplyr::filter(flowers == 1)

# see how man observations are left for each hex cell

assy_inat %>% 
  group_by(hex_ids) %>% 
  summarise(count = n()) ## So keeping just hex_ids 8895

ceca_inat %>% 
  group_by(hex_ids) %>% 
  summarise(count = n()) ## so keeping all three hex_ids (7955, 10566, 12559)

## Get asclepias onset and offset estimates from phest and phenesse
assy_inat_8896 <- filter(assy_inat, hex_ids == 8896)

assy_8896_phest_on <- phest::weib.limit(assy_inat_8896$doy, upper = FALSE)
assy_8896_phest_off <- phest::weib.limit(assy_inat_8896$doy, upper = TRUE)

assy_8896_phenesse_on <- phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0)
assy_8896_phenesse_off <- phenesse::weib_percentile(assy_inat_8896$doy, percentile = 1)

# build GAM using NPN data for asclepias
assy_npn_8896 <- filter(npn_a, hex_ids == 8896)

assy_8896_gam <- gam(Phenophase_Status ~ s(Day_of_Year, bs = "cc"),
                     family = binomial, gamma = 3, data = assy_npn_8896)

plot(assy_8896_gam)

assy_all_doys = data.frame(Day_of_Year = 1:365)
assy_all_doys$flowering_probability = predict(assy_8896_gam, newdata = assy_all_doys, type = 'response')

ggplot() + 
  geom_line(assy_all_doys, mapping = aes(x = Day_of_Year, y = flowering_probability), size = 2) +
  geom_point(assy_npn_8896, mapping = aes(x = Day_of_Year, y = Phenophase_Status, 
                                          color = as.character(Phenophase_Status))) +
  geom_vline(xintercept = assy_8896_phenesse_on, color = "purple", size = 1.5) +
  geom_vline(xintercept = assy_8896_phenesse_off, color = "purple", size = 1.5) +
  geom_vline(xintercept = assy_8896_phest_on[1], color = "green", size = 1.5) +
  geom_vline(xintercept = assy_8896_phest_off[1], color = "green", size = 1.5) 


## build GAM using NPN data for cercis
#ceca_npn_7955 <- filter(npn_c, hex_ids == 7955)
#
#ceca_7955_gam <- gam(Phenophase_Status ~ s(Day_of_Year, bs = "cc"),
#                     family = binomial, gamma = 1, data = ceca_npn_7955)
#
#plot(ceca_7955_gam)
#
#ceca_7955_all_doys = data.frame(Day_of_Year = 1:365)
#ceca_7955_all_doys$flowering_probability = predict(ceca_7955_gam, newdata = ceca_7955_all_doys, type = 'response')
#
#ggplot() + 
#  geom_line(ceca_7955_all_doys, mapping = aes(x = Day_of_Year, y = flowering_probability), size = 2) +
#  geom_point(ceca_npn_7955, mapping = aes(x = Day_of_Year, y = Phenophase_Status, 
#                                          color = as.character(Phenophase_Status))) 
#
#This hex cell ended up having two flowering periods, one late November/december, so we removed from the dataset

## Get asclepias onset and offset estimates from phest and phenesse
ceca_inat_12559 <- filter(ceca_inat, hex_ids == 12559)

ceca_12559_phest_on <- phest::weib.limit(ceca_inat_12559$doy, upper = FALSE)
ceca_12559_phest_off <- phest::weib.limit(ceca_inat_12559$doy, upper = TRUE)

ceca_12559_phenesse_on <- phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0)
ceca_12559_phenesse_off <- phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 1)


## build GAM using NPN data for cercis
ceca_npn_12559 <- filter(npn_c, hex_ids == 12559) %>% 
  filter(Phenophase_Status >=0 & Phenophase_Status <= 1)

# appears to be to erroneous "yes's", removing outliers
outliers <- ceca_npn_12559 %>% 
  filter(Day_of_Year <= 100 & Phenophase_Status == 1)

ceca_npn_12559 <- anti_join(ceca_npn_12559, outliers)

ceca_12559_gam <- gam(Phenophase_Status ~ s(Day_of_Year, bs = "cc"),
                      family = binomial, gamma = 4, data = ceca_npn_12559)

plot(ceca_12559_gam)

ceca_12559_all_doys = data.frame(Day_of_Year = 1:365)
ceca_12559_all_doys$flowering_probability = predict(ceca_12559_gam, newdata = ceca_12559_all_doys, type = 'response')

ggplot() + 
  geom_line(ceca_12559_all_doys, mapping = aes(x = Day_of_Year, y = flowering_probability), size = 2) +
  geom_point(ceca_npn_12559, mapping = aes(x = Day_of_Year, y = Phenophase_Status, 
                                           color = as.character(Phenophase_Status))) 


## Get cercis onset and offset estimates from phest and phenesse
ceca_inat_10566 <- filter(ceca_inat, hex_ids == 10566)

ceca_10566_phest_on <- phest::weib.limit(ceca_inat_10566$doy, upper = FALSE)
ceca_10566_phest_off <- phest::weib.limit(ceca_inat_10566$doy, upper = TRUE)

ceca_10566_phenesse_on <- phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0)
ceca_10566_phenesse_off <- phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 1)


## build GAM using NPN data for cercis
ceca_npn_10566 <- filter(npn_c, hex_ids == 10566) %>% 
  filter(Phenophase_Status >=0 & Phenophase_Status <= 1)

# appears to be to one erroneous "yes", removing outlier
outliers_10566 <- ceca_npn_10566 %>% 
  filter(Day_of_Year >= 200 & Phenophase_Status == 1)

ceca_npn_10566 <- anti_join(ceca_npn_10566, outliers_10566)

ceca_10566_gam <- gam(Phenophase_Status ~ s(Day_of_Year, bs = "cc"),
                      family = binomial, gamma = 7, data = ceca_npn_10566)

plot(ceca_10566_gam)

ceca_10566_all_doys = data.frame(Day_of_Year = 1:365)
ceca_10566_all_doys$flowering_probability = predict(ceca_10566_gam, newdata = ceca_10566_all_doys, type = 'response')

ggplot() + 
  geom_line(ceca_10566_all_doys, mapping = aes(x = Day_of_Year, y = flowering_probability), size = 2) +
  geom_point(ceca_npn_10566, mapping = aes(x = Day_of_Year, y = Phenophase_Status, 
                                           color = as.character(Phenophase_Status))) +
  geom_vline(xintercept = ceca_10566_phenesse_on, color = "purple", size = 1.5) +
  geom_vline(xintercept = ceca_10566_phenesse_off, color = "purple", size = 1.5) +
  geom_vline(xintercept = ceca_10566_phest_on[1], color = "green", size = 1.5) +
  geom_vline(xintercept = ceca_10566_phest_off[1], color = "green", size = 1.5) 

################################## Now for analyses and stuff #########################

# Asclepias first 
assy_gam_results <- assy_all_doys %>% 
  mutate(
    cum_prob = cumsum(flowering_probability),
    cum_perc = cum_prob / max(cum_prob),
    onset = Day_of_Year[which.max(cum_perc >= 0.001)],
    first = Day_of_Year[which.max(cum_perc >= 0.01)],
    fifth = Day_of_Year[which.max(cum_perc >= 0.05)],
    tenth = Day_of_Year[which.max(cum_perc >= 0.10)],
    fiftieth = Day_of_Year[which.max(cum_perc >= 0.50)],
    nintieth = Day_of_Year[which.max(cum_perc >= 0.90)],
    nintyfifth = Day_of_Year[which.max(cum_perc >= 0.95)],
    nintyninth = Day_of_Year[which.max(cum_perc >= 0.99)],
    offset = Day_of_Year[which.max(cum_perc >= 0.999)]
  )


ggplot(assy_gam_results, aes(x = Day_of_Year, y = flowering_probability)) + 
  geom_line(size = 2) +
  geom_vline(xintercept = c(assy_gam_results$onset, assy_gam_results$first, assy_gam_results$tenth, 
                            assy_gam_results$fiftieth, assy_gam_results$nintieth, 
                            assy_gam_results$nintyninth, assy_gam_results$offset))

q_assy_8896 <- quantile(assy_inat_8896$doy, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  as.data.frame() %>% 
  mutate(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  dplyr::rename(quan_est = ".")

phest_assy_8896 <- data.frame(percentile = c(0,1),
                              phest_est = c(phest::weib.limit(assy_inat_8896$doy, upper = FALSE)[1],
                                            phest::weib.limit(assy_inat_8896$doy, upper = TRUE)[1]))            

phenesse_assy_8896 <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                                 phenes_est = c(phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.01),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.05),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.1),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.5),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.9),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.95),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 0.99),
                                                phenesse::weib_percentile(assy_inat_8896$doy, percentile = 1)))

assy_npn_perc_df <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                               benchmark = c(assy_gam_results$onset[1], 
                                             assy_gam_results$first[1], 
                                             assy_gam_results$fifth[1], 
                                             assy_gam_results$tenth[1], 
                                             assy_gam_results$fiftieth[1], 
                                             assy_gam_results$nintieth[1],
                                             assy_gam_results$nintyfifth[1], 
                                             assy_gam_results$nintyninth[1],
                                             assy_gam_results$offset[1]))

assy_total <- left_join(assy_npn_perc_df, phenesse_assy_8896)
assy_total <- left_join(assy_total, phest_assy_8896)                               
assy_total <- left_join(assy_total, q_assy_8896) %>% 
  mutate(mean_est = NA)

assy_gam_mean <- sample_n(assy_all_doys, size = 1000, 
                          weight = flowering_probability, replace = TRUE)
mean_npn_assy <- mean(assy_gam_mean$Day_of_Year)
mean_inat_assy <- mean(assy_inat_8896$doy)

mean_row_assy <- c("mean", mean_npn_assy, NA, NA, NA, mean_inat_assy)
assy_total <- rbind(assy_total, mean_row_assy)


############### Cercis Next ####################

# Cercis hex cell 12559
ceca_gam_results_12559 <- ceca_12559_all_doys %>% 
  mutate(
    cum_prob = cumsum(flowering_probability),
    cum_perc = cum_prob / max(cum_prob),
    onset = Day_of_Year[which.max(cum_perc >= 0.001)],
    first = Day_of_Year[which.max(cum_perc >= 0.01)],
    fifth = Day_of_Year[which.max(cum_perc >= 0.05)],
    tenth = Day_of_Year[which.max(cum_perc >= 0.10)],
    fiftieth = Day_of_Year[which.max(cum_perc >= 0.50)],
    nintieth = Day_of_Year[which.max(cum_perc >= 0.90)],
    nintyfifth = Day_of_Year[which.max(cum_perc >= 0.95)],
    nintyninth = Day_of_Year[which.max(cum_perc >= 0.99)],
    offset = Day_of_Year[which.max(cum_perc >= 0.999)]
  )


ggplot(ceca_gam_results_12559, aes(x = Day_of_Year, y = flowering_probability)) + 
  geom_line(size = 2) +
  geom_vline(xintercept = c(ceca_gam_results_12559$onset, ceca_gam_results_12559$first, ceca_gam_results_12559$tenth, 
                            ceca_gam_results_12559$fiftieth, ceca_gam_results_12559$nintieth, 
                            ceca_gam_results_12559$nintyninth, ceca_gam_results_12559$offset))

q_ceca_12559 <- quantile(ceca_inat_12559$doy, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  as.data.frame() %>% 
  mutate(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  dplyr::rename(quan_est = ".")

phest_ceca_12559 <- data.frame(percentile = c(0,1),
                               phest_est = c(phest::weib.limit(ceca_inat_12559$doy, upper = FALSE)[1],
                                             phest::weib.limit(ceca_inat_12559$doy, upper = TRUE)[1]))            

phenesse_ceca_12559 <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                                  phenes_est = c(phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.01),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.05),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.1),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.5),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.9),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.95),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 0.99),
                                                 phenesse::weib_percentile(ceca_inat_12559$doy, percentile = 1)))

ceca_npn_perc_df_12559 <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                                     benchmark = c(ceca_gam_results_12559$onset[1], 
                                                   ceca_gam_results_12559$first[1], 
                                                   ceca_gam_results_12559$fifth[1], 
                                                   ceca_gam_results_12559$tenth[1], 
                                                   ceca_gam_results_12559$fiftieth[1],
                                                   ceca_gam_results_12559$nintieth[1],
                                                   ceca_gam_results_12559$nintyfifth[1], 
                                                   ceca_gam_results_12559$nintyninth[1],
                                                   ceca_gam_results_12559$offset[1]))

ceca_total_12559 <- left_join(ceca_npn_perc_df_12559, phenesse_ceca_12559)
ceca_total_12559 <- left_join(ceca_total_12559, phest_ceca_12559)                               
ceca_total_12559 <- left_join(ceca_total_12559, q_ceca_12559) %>% 
  mutate(mean_est = NA)

ceca_gam_mean_12559 <- sample_n(ceca_12559_all_doys, size = 1000, 
                                weight = flowering_probability, replace = TRUE)
mean_npn_12559 <- mean(ceca_gam_mean_12559$Day_of_Year)
mean_inat_12559 <- mean(ceca_inat_12559$doy)

mean_row_12559 <- c("mean", mean_npn_12559, NA, NA, NA, mean_inat_12559)
ceca_total_12559 <- rbind(ceca_total_12559, mean_row_12559)

# Cercis hex cell 10566
ceca_gam_results_10566 <- ceca_10566_all_doys %>% 
  mutate(
    cum_prob = cumsum(flowering_probability),
    cum_perc = cum_prob / max(cum_prob),
    onset = Day_of_Year[which.max(cum_perc >= 0.001)],
    first = Day_of_Year[which.max(cum_perc >= 0.01)],
    fifth = Day_of_Year[which.max(cum_perc >= 0.05)],
    tenth = Day_of_Year[which.max(cum_perc >= 0.10)],
    fiftieth = Day_of_Year[which.max(cum_perc >= 0.50)],
    nintieth = Day_of_Year[which.max(cum_perc >= 0.90)],
    nintyfifth = Day_of_Year[which.max(cum_perc >= 0.95)],
    nintyninth = Day_of_Year[which.max(cum_perc >= 0.99)],
    offset = Day_of_Year[which.max(cum_perc >= 0.999)]
  )


ggplot(ceca_gam_results_10566, aes(x = Day_of_Year, y = flowering_probability)) + 
  geom_line(size = 2) +
  geom_vline(xintercept = c(ceca_gam_results_10566$onset, ceca_gam_results_10566$first, ceca_gam_results_10566$tenth, 
                            ceca_gam_results_10566$fiftieth, ceca_gam_results_10566$nintieth, 
                            ceca_gam_results_10566$nintyninth, ceca_gam_results_10566$offset))

q_ceca_10566 <- quantile(ceca_inat_10566$doy, probs = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  as.data.frame() %>% 
  mutate(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1)) %>% 
  dplyr::rename(quan_est = ".")

phest_ceca_10566 <- data.frame(percentile = c(0,1),
                               phest_est = c(phest::weib.limit(ceca_inat_10566$doy, upper = FALSE)[1],
                                             phest::weib.limit(ceca_inat_10566$doy, upper = TRUE)[1]))            

phenesse_ceca_10566 <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                                  phenes_est = c(phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.01),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.05),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.1),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.5),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.9),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.95),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 0.99),
                                                 phenesse::weib_percentile(ceca_inat_10566$doy, percentile = 1)))

ceca_npn_perc_df_10566 <- data.frame(percentile = c(0,0.01,0.05,0.1,0.5,0.9,0.95,0.99,1),
                                     benchmark = c(ceca_gam_results_10566$onset[1], 
                                                   ceca_gam_results_10566$first[1], 
                                                   ceca_gam_results_10566$fifth[1], 
                                                   ceca_gam_results_10566$tenth[1], 
                                                   ceca_gam_results_10566$fiftieth[1], 
                                                   ceca_gam_results_10566$nintieth[1],
                                                   ceca_gam_results_10566$nintyfifth[1], 
                                                   ceca_gam_results_10566$nintyninth[1],
                                                   ceca_gam_results_10566$offset[1]))

ceca_total_10566 <- left_join(ceca_npn_perc_df_10566, phenesse_ceca_10566)
ceca_total_10566 <- left_join(ceca_total_10566, phest_ceca_10566)                               
ceca_total_10566 <- left_join(ceca_total_10566, q_ceca_10566) %>% 
  mutate(mean_est = NA)   

ceca_gam_mean_10566 <- sample_n(ceca_10566_all_doys, size = 1000, 
                                weight = flowering_probability, replace = TRUE)
mean_npn_10566 <- mean(ceca_gam_mean_10566$Day_of_Year)
mean_inat_10566 <- mean(ceca_inat_10566$doy)

mean_row_10566 <- c("mean", mean_npn_10566, NA, NA, NA, mean_inat_10566)
ceca_total_10566 <- rbind(ceca_total_10566, mean_row_10566)

# Save results

write.csv(ceca_total_10566, file = "empirical_data/empirical_results/cercis_canadensis_10566.csv", row.names = FALSE)
write.csv(ceca_total_12559, file = "empirical_data/empirical_results/cercis_canadensis_12559.csv", row.names = FALSE)
write.csv(assy_total, file = "empirical_data/empirical_results/asclepias_syriaca_8896.csv", row.names = FALSE)