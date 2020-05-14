library(tidyverse)
library(lubridate)
library(sf)

# Jouney North Data
jn <- read.csv("empirical_data/jouneyNorth/JourneyNorthAdultMonarchs2018.csv", stringsAsFactors = FALSE)

jn <- jn %>% 
  mutate(doy = yday(mdy(Date))) %>% 
  filter(!is.na(Long)) %>% 
  filter(!is.na(Lat)) %>% 
  filter(Event == "Monarch Adult (FIRST sighted)")

jn_sf <- st_transform(st_as_sf(jn,
                               coords = c("Long", "Lat"),
                               crs = 4326),
                               crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# read monarch data
inat <- read.csv("empirical_data/inat_monarchs/monarchs_annotated_inat.csv", stringsAsFactors = FALSE)

inat_adults <- inat %>% 
  filter(adults == 1)

inat_adults_sf <- st_transform(st_as_sf(inat_adults,
                                        coords = c("longitude", "latitude"),
                                        crs = 4326),
                               crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# make grids of survey area
e <- as(raster::extent(-94, -68, 36, 42), "SpatialPolygons") %>% 
  st_as_sf() %>% 
  st_set_crs(4326)


grid <- st_make_grid(e, cellsize = c(1, 1), square = F)
hex <- mutate(st_as_sf(grid), hex_ids = 1:n())
hex_proj <- st_transform(hex, crs = st_crs(inat_adults_sf))
hex_proj <- st_make_grid(hex_proj, cellsize = c(30000,30000), square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(hex_ids = 1:n())

# clip data to study area

jn_clip_sf <- st_intersection(x = jn_sf, hex_proj)
inat_clip_sf <- st_intersection(x = inat_adults_sf, hex_proj)

## See how many observations are in each cell

inat_cells <- inat_clip_sf %>% 
  group_by(hex_ids) %>% 
  summarise(inat_obs = n()) %>% 
  st_drop_geometry() 

inat_too_few_obs <- inat_cells %>% 
  filter(inat_obs <= 1)

jn_cells <- jn_clip_sf %>% 
  group_by(hex_ids) %>% 
  summarise(jn_obs = n()) %>% 
  st_drop_geometry()

## Inat estimates per cell

# Note parellelize for quicker calculations

inat_cells_est <- anti_join(inat_clip_sf, inat_too_few_obs) %>% 
  group_by(hex_ids) %>% 
  summarise(inat_obs = n(), 
            quan_est = quantile(day, probs = 0),
            phen_est = tryCatch(phenesse::weib_percentile(observations = day, percentile = 0),
                                error = function(e) NA),
            phest_est = tryCatch(phest::weib.limit(x = day, k = 30)[1],
                                 error = function(e) NA))

inat_est_df <- st_drop_geometry(inat_cells_est)


## Journey North First observed per cell
jn_cells_est <- jn_clip_sf %>% 
  group_by(hex_ids) %>% 
  summarise(jn_obs = n(),
            benchmark = min(doy)) 

jn_est_df <- st_drop_geometry(jn_cells_est)

## Join cells together
total_estimates <- left_join(inat_est_df, jn_est_df) %>% 
  filter(!is.na(benchmark)) %>% 
  mutate(q_error = quan_est - benchmark,
         phest_error = phest_est - benchmark,
         phenesse_error = phen_est - benchmark)

write.csv(total_estimates, file = "empirical_data/inat_monarchs/total_estimates.csv", row.names = FALSE)