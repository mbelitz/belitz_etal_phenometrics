library(dplyr)
library(ggplot2)

oh <- read.csv("iNatskew/pollard_flightcurves_OH.csv", stringsAsFactors = FALSE)
inat_test <- read.csv("iNatskew/iNat_testdata.csv", stringsAsFactors = FALSE)

scybele_oh <- oh %>% 
  select(S.cybele, surveyN, DAY)
anumitor_oh <- oh %>% 
  select(A.numitor, surveyN, DAY)
cpegala_oh <- oh %>% 
  select(C.pegala, surveyN, DAY)
mcymela_oh <- oh %>% 
  select(M.cymela, surveyN, DAY)

scybele_inat_2017 <- inat_test %>% 
  filter(scientific_name == "Speyeria cybele") %>% 
  filter(year == 2017) %>% 
  filter(latitude >= 41 & latitude <= 42) %>% 
  filter(longitude >= -82 & longitude <= -81)


anumitor_inat_2017 <- inat_test %>% 
  filter(scientific_name == "Ancyloxypha numitor") %>% 
  filter(year == 2017) %>% 
  filter(latitude >= 41 & latitude <= 42) %>% 
  filter(longitude >= -82 & longitude <= -81)


mcymela_inat_2017 <- inat_test %>% 
  filter(scientific_name == "Megisto cymela") %>% 
  filter(year == 2017) %>% 
  filter(latitude >= 41 & latitude <= 42) %>% 
  filter(longitude >= -82 & longitude <= -81)


cpegala_inat_2017 <- inat_test %>% 
  filter(scientific_name == "Cercyonis pegala") %>% 
  filter(year == 2017) %>% 
  filter(latitude >= 41 & latitude <= 42) %>% 
  filter(longitude >= -82 & longitude <= -81)

set.seed(70)
d <- sample_n(scybele_oh, size = 100, weight = S.cybele, replace = TRUE)
mean(d$DAY)
sd(d$DAY)

mean(scybele_inat_2017$day)
sd(scybele_inat_2017$day)

### Megisto cymela

set.seed(70)
mc <- sample_n(mcymela_oh, size = 100, weight = M.cymela, replace = TRUE)
mean(mc$DAY)
sd(mc$DAY)

mean(mcymela_inat_2017$day)
sd(mcymela_inat_2017$day)

# Ancyloxypha numitor

set.seed(70)
an <- sample_n(anumitor_oh, size = 100, weight = A.numitor, replace = TRUE)
mean(an$DAY)
sd(an$DAY)

mean(anumitor_inat_2017$day)
sd(anumitor_inat_2017$day)


# Cercyonis pegala
set.seed(70)
cp <- sample_n(cpegala_oh, size = 100, weight = C.pegala, replace = TRUE)
mean(cp$DAY)
sd(cp$DAY)

mean(cpegala_inat_2017$day)
sd(cpegala_inat_2017$day)

##### Plot ohio data
sc_oh_plot <- ggplot() + 
  geom_line(scybele_oh, mapping = aes(x = DAY, y = S.cybele/nrow(scybele_oh)), color = "purple") + 
  geom_density(scybele_inat_2017, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 196.39, color = "purple") + 
  geom_vline(xintercept = 203.6, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Speyeria cybele") + 
  theme_classic()

mc_oh_plot <- ggplot() + 
  geom_line(mcymela_oh, mapping = aes(x = DAY, y = M.cymela/nrow(mcymela_oh)), color = "purple") + 
  geom_density(mcymela_inat_2017, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 155.58, color = "purple") + 
  geom_vline(xintercept = 158.59, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Megisto cymela") + 
  theme_classic()

an_oh_plot <- ggplot() + 
  geom_line(anumitor_oh, mapping = aes(x = DAY, y = A.numitor/nrow(anumitor_oh)), color = "purple") + 
  geom_density(anumitor_inat_2017, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 210.99, color = "purple") + 
  geom_vline(xintercept = 199.33, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Ancyloxypha numitor") + 
  theme_classic()

cp_oh_plot <- ggplot() + 
  geom_line(cpegala_oh, mapping = aes(x = DAY, y = C.pegala/nrow(cpegala_oh)), color = "purple") + 
  geom_density(cpegala_inat_2017, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 197.67, color = "purple") + 
  geom_vline(xintercept = 200.7, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Cercyonis pegala") + 
  theme_classic()

cowplot::plot_grid(sc_oh_plot, mc_oh_plot, an_oh_plot, cp_oh_plot,
                    nrow = 2)

ggsave(filename = "iNatskew/Ohiotests.png")

## Now with the IL data 

il <- read.csv("iNatskew/pollard_flightCurves_IL.csv", stringsAsFactors = FALSE)

scybele_il <- il %>% 
  select(S.cybele, surveyN, DAY)
anumitor_il <- il %>% 
  select(A.numitor, surveyN, DAY)
cpegala_il <- il %>% 
  select(C.pegala, surveyN, DAY)
mcymela_il <- il %>% 
  select(M.cymela, surveyN, DAY)

scybele_inat_2018 <- inat_test %>% 
  filter(scientific_name == "Speyeria cybele") %>% 
  filter(year == 2018) %>% 
  filter(latitude >=  41.5 & latitude <= 42.5) %>% 
  filter(longitude >= -88.5 & longitude <= -87.5)


anumitor_inat_2018 <- inat_test %>% 
  filter(scientific_name == "Ancyloxypha numitor") %>% 
  filter(year == 2018) %>% 
  filter(latitude >= 41.5 & latitude <= 42.5) %>% 
  filter(longitude >= -88.5 & longitude <= -87.5)

mcymela_inat_2018 <- inat_test %>% 
  filter(scientific_name == "Megisto cymela") %>% 
  filter(year == 2018) %>% 
  filter(latitude >= 41.5  & latitude <= 42.5) %>% 
  filter(longitude >= -88.5 & longitude <= -87.5)


cpegala_inat_2018 <- inat_test %>% 
  filter(scientific_name == "Cercyonis pegala") %>% 
  filter(year == 2018)  %>% 
  filter(latitude >= 41.5  & latitude <= 42.5) %>% 
  filter(longitude >= -88.5 & longitude <= -87.5)


set.seed(70)
d <- sample_n(scybele_il, size = 100, weight = S.cybele, replace = TRUE)
mean(d$DAY)
sd(d$DAY)

mean(scybele_inat_2018$day)
sd(scybele_inat_2018$day)

### Megisto cymela

set.seed(70)
mc <- sample_n(mcymela_il, size = 100, weight = M.cymela, replace = TRUE)
mean(mc$DAY)
sd(mc$DAY)

mean(mcymela_inat_2018$day)
sd(mcymela_inat_2018$day)

# Ancyloxypha numitor

set.seed(70)
an <- sample_n(anumitor, size = 100, weight = A.numitor, replace = TRUE)
mean(an$DAY)
sd(an$DAY)

mean(anumitor_inat_2018$day)
sd(anumitor_inat_2018$day)


# Cercyonis pegala
set.seed(70)
cp <- sample_n(cpegala, size = 100, weight = C.pegala, replace = TRUE)
mean(cp$DAY)
sd(cp$DAY)

mean(cpegala_inat_2018$day)
sd(cpegala_inat_2018$day)

##### Plot Illinois data
sc_il_plot <- ggplot() + 
  geom_line(scybele_il, mapping = aes(x = DAY, y = S.cybele/nrow(scybele_il)), color = "purple") + 
  geom_density(scybele_inat_2018, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 191.79, color = "purple") + 
  geom_vline(xintercept = 190.33, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Speyeria cybele") + 
  theme_classic()

mc_il_plot <- ggplot() + 
  geom_line(mcymela_il, mapping = aes(x = DAY, y = M.cymela/nrow(mcymela_il)), color = "purple") + 
  geom_density(mcymela_inat_2018, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 167.24, color = "purple") + 
  geom_vline(xintercept = 171.85, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Megisto cymela") + 
  theme_classic()

an_il_plot <- ggplot() + 
  geom_line(anumitor_il, mapping = aes(x = DAY, y = A.numitor/nrow(anumitor_il)), color = "purple") + 
  geom_density(anumitor_inat_2018, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 208.88, color = "purple") + 
  geom_vline(xintercept = 206.67, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Ancyloxypha numitor") + 
  theme_classic()

cp_il_plot <- ggplot() + 
  geom_line(cpegala_il, mapping = aes(x = DAY, y = C.pegala/nrow(cpegala_il)), color = "purple") + 
  geom_density(cpegala_inat_2018, mapping = aes(day)) + 
  xlim(0,300) +
  geom_vline(xintercept = 202.04, color = "purple") + 
  geom_vline(xintercept = 208.33, color = "black") +
  labs(x = "DAY", y = "Relative Abundance", title = "Cercyonis pegala") + 
  theme_classic()

cowplot::plot_grid(sc_il_plot, mc_il_plot, an_il_plot, cp_il_plot,
                   nrow = 2)

ggsave(filename = "iNatskew/Illinoistest.png")
