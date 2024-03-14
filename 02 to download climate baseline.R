
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, dismo, sf, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Vector data - Down ------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- wrld[wrld$name == 'Madagascar',]

# Raster data - down ------------------------------------------------------
prec <- geodata::worldclim_country(country = 'MDG', var = 'prec', path = './tmpr')
tmin <- geodata::worldclim_country(country = 'MDG', var = 'tmin', path = './tmpr')
tmax <- geodata::worldclim_country(country = 'MDG', var = 'tmax', path = './tmpr')

# Extract by mask ---------------------------------------------------------
prec <- terra::crop(prec, limt) %>% terra::mask(., limt)
tmin <- terra::crop(tmin, limt) %>% terra::mask(., limt)
tmax <- terra::crop(tmax, limt) %>% terra::mask(., limt)

# To change the names -----------------------------------------------------
names(prec) <- glue('prec_{1:12}')
names(tmin) <- glue('tmin_{1:12}')
names(tmax) <- glue('tmax_{1:12}')

# To write the rasters ----------------------------------------------------
terra::writeRaster(x = prec, filename = glue('./tif/wc/prec.tif'), overwrite = TRUE)
terra::writeRaster(x = tmin, filename = glue('./tif/wc/tmin.tif'), overwrite = TRUE)
terra::writeRaster(x = tmax, filename = glue('./tif/wc/tmax.tif'), overwrite = TRUE)

# To create biovars variables ---------------------------------------------

## Coordinates
crds <- terra::as.data.frame(prec, xy = T) %>% as_tibble() %>% dplyr::select(x, y)

## Raster to table
prec.mx <- terra::as.data.frame(prec, xy = F) %>% as.matrix()
tmin.mx <- terra::as.data.frame(tmin, xy = F) %>% as.matrix()
tmax.mx <- terra::as.data.frame(tmax, xy = F) %>% as.matrix()

## To create biovars variables 
bioc.mx <- dismo::biovars(prec = prec.mx, tmin = tmin.mx, tmax = tmax.mx)
bioc.mx <- cbind(crds, bioc.mx)
bioc.rs <- as.data.frame(bioc.mx) %>% terra::rast(., type = 'xyz')
crs(bioc.rs) <- 'EPSG:4326'

## To write raster
terra::writeRaster(x = bioc.rs, filename = './tif/wc/bioc.tif', overwrite = TRUE)

# To download SRTM --------------------------------------------------------
srtm <- geodata::elevation_30s(country = 'MDG', path = './tmpr')
terra::writeRaster(x = srtm, filename = './tif/dem/srtm_raw.tif', overwrite = TRUE)

# Go to SAGA - GIS and apply the function: Fill Sinks (Wang & Liu)
# This tool uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
# The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) 
# but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a 
# minimum slope gradient (and thus elevation difference) between cells.

srtm.raw  <- terra::rast('./tif/dem/srtm_raw.tif')
srtm.fill <- terra::rast('./tif/dem/srtm_fill.tif')

srtm <- c(srtm.raw, srtm.fill)
plot(srtm)
