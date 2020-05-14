library(tidyverse)
library(sf)
library(downloader)
library(rnaturalearth)
library(rinat)

# read in NPN data
npn <- read.csv("empirical_data/NPN/status_intensity_observation_data.csv", stringsAsFactors = FALSE)

# filter to asclepias and 
npn_asclepias <- filter(npn, Phenophase_Description == "Open flowers") %>% 
  filter(Genus == "Asclepias")
npn_cercis <- filter(npn, Phenophase_Description == "Open flowers") %>% 
  filter(Genus == "Cercis")

## read in iNat data
iNat_cercis <- fread('empirical_data/gbif1/occurrence.txt') %>% 
  filter(year == 2019)
iNat_asclepias <- fread("empirical_data/gbif2/occurrence.txt") %>% 
  filter(year == 2019)

## read in eastern temperate forest data
na <- ne_countries(country = "United States of America", returnclass = "sf")

na_proj <- st_transform(na, 
                        crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") %>% 
  st_simplify(dTolerance = 2500)

## convert occurrence datasets to sf objects

iNat_cercis_sf <- st_transform(st_as_sf(iNat_cercis, 
                                        coords = c("decimalLongitude", "decimalLatitude"),
                                        crs = 4326),
                               crs = st_crs(na_proj))

iNat_asclepias_sf <- st_transform(st_as_sf(iNat_asclepias, 
                                           coords = c("decimalLongitude", "decimalLatitude"),
                                           crs = 4326),
                                  crs = st_crs(na_proj))

npn_asclepias_sf <- st_transform(st_as_sf(npn_asclepias, 
                                          coords = c("Longitude", "Latitude"),
                                          crs = 4326),
                                 crs = st_crs(na_proj))

npn_cercis_sf <- st_transform(st_as_sf(npn_cercis, 
                                       coords = c("Longitude", "Latitude"),
                                       crs = 4326),
                              crs = st_crs(na_proj))

## make grid over US

grids <- st_make_grid(na_proj, cellsize = c(30000, 30000), square = FALSE)
hex <- mutate(st_as_sf(grids), hex_ids = 1:n())

ggplot() + 
  geom_sf(na_proj, mapping = aes(), color = "Blue") +
  geom_sf(hex, mapping = aes(), alpha = 0.3, fill = "transparent") +
  geom_sf(iNat_asclepias_sf, mapping = aes())

## Now lets intersect with our datasets

## first with NPN
npn_cercis_intersect <- st_intersection(npn_cercis_sf, hex)
npn_asclepias_intersect <- st_intersection(npn_asclepias_sf, hex)

npn_cercis_cells <- npn_cercis_intersect %>% 
  group_by(hex_ids) %>% 
  summarise(count = n())

npn_cercis_cells_100obs <- npn_cercis_intersect %>% 
  filter(hex_ids == 7955 | hex_ids == 12559 | hex_ids == 10566) %>% 
  st_drop_geometry()

write.csv(x = npn_cercis_cells_100obs, 
          file = "empirical_data/cleaned_data/cleaned_npn_cercis.csv",
          row.names = FALSE)

npn_asclepias_cells <- npn_asclepias_intersect %>% 
  group_by(hex_ids) %>% 
  summarise(count = n())

npn_asclepias_cells_100obs <- npn_asclepias_intersect %>% 
  filter(hex_ids == 12533 | hex_ids == 8896) %>% 
  st_drop_geometry()

write.csv(x = npn_asclepias_cells_100obs, 
          file = "empirical_data/cleaned_data/cleaned_npn_asclepias.csv",
          row.names = FALSE)

## next with iNat
iNat_cercis_intersect <- st_intersection(iNat_cercis_sf, hex)
iNat_asclepias_intersect <- st_intersection(iNat_asclepias_sf, hex)

iNat_cercis_cells <- iNat_cercis_intersect %>% 
  group_by(hex_ids) %>% 
  summarise(count = n())


inat_cercis_cells_10obs <- iNat_cercis_intersect %>% 
  filter(hex_ids == 7955 | hex_ids == 12559 | hex_ids == 10566) %>% 
  st_drop_geometry()

write.csv(x = inat_cercis_cells_10obs, 
          file = "empirical_data/cleaned_data/cleaned_inat_cercis.csv",
          row.names = FALSE)

iNat_asclepias_cells <- iNat_asclepias_intersect %>% 
  group_by(hex_ids) %>% 
  summarise(count = n())

iNat_asclepias_cells_10obs <- iNat_asclepias_intersect %>% 
  filter(hex_ids == 12533 | hex_ids == 8896) %>% 
  st_drop_geometry()

write.csv(x = iNat_asclepias_cells_10obs, 
          file = "empirical_data/cleaned_data/cleaned_inat_asclepias.csv",
          row.names = FALSE)

## download these images from iNat

myfmt <- NULL
myfmt[[1]] <- c("border","white","20x50")
myfmt[[2]] <- c("border","grey", "2x2")
myfmt[[3]] <- c("annotate",c('"\u00A9"', 'photoby','"   "'),"black",NA, 20,"southeast")
myfmt[[4]] <- c("annotate",c('" "','scname','"\\n "', 'cname'),"black",NA, 20,"northwest")
myfmt[[5]] <- c("annotate",c('" "','place','"\\n "', 'photodate'),"black",NA, 20,"southwest")
myfmt[[6]] <- c("annotate",c('"iNat Id \\n "','obs_id','" "'),"black",NA, 20,"northeast")

## Cercis downloads

#' note change output directory to your own directory to reproduce
cercis_ids <- c$identifier

for(i in cercis_ids){
  m_obs <- get_inat_obs_id(i)
  iurl <- m_obs$observation_photos$photo$medium_url
  downloader::download(iurl, destfile=paste0('c:/Users/Mike/Documents/UF2/plant_inat_images/Cercis_Images/', i,'.jpg'), mode = 'wb')
}

## Asclepias downloads

asclepias_ids <- iNat_asclepias_cells_10obs$identifier

for(i in asclepias_ids){
  m_obs <- get_inat_obs_id(i)
  iurl <- m_obs$observation_photos$photo$medium_url
  downloader::download(iurl, destfile=paste0('c:/Users/Mike/Documents/UF2/plant_inat_images/Asclepias_Images/', i,'.jpg'), mode = 'wb')
}