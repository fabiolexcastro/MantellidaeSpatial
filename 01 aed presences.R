
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
limt <- terra::vect('./shp/base/madagascar_Madagascar_Country_Boundary.shp')
occr <- read_csv('./tbl/occurrence.csv', show_col_types = F)

## Unique species
spcs <- unique(occr$scientificName)
occr %>% distinct(class, order, family )

## Families 
occr %>% distinct(class, order, family)

## Frequency analysis
freq <- as_tibble(as.data.frame(table(occr$scientificName))) %>% arrange(desc(Freq))
write.csv(freq, './tbl/freq_ocurrence.csv', row.names = FALSE)

# Table to shapefile  -----------------------------------------------------
pnts <- terra::vect(occr, c('decimalLongitude', 'decimalLatitude'), crs = 'EPSG:4326')

# DEM  --------------------------------------------------------------------
srtm <- geodata::elevation_30s(country = 'MDG', path = './tmpr')

