

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, corrplot, GGally, psych, dismo, usdm, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Tabular data
pnts <- read_csv('./tbl/occurrence_outliers.csv', show_col_types = FALSE)

## Raster data 
bioc <- terra::rast('./tif/wc/bioc.tif')

## Vector data 
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- wrld[wrld$name == 'Madagascar',]

# Extract the bioclimatic values for the presences ------------------------
pnts <- filter(pnts, type == 'Keep')
pnts <- as_tibble(cbind(pnts, terra::extract(bioc, pnts[,c('lon', 'lat')])))
pnts <- dplyr::select(pnts, lon, lat, bio1:bio19)

# Make VIF analysis -------------------------------------------------------
vars <- vifstep(x = as.data.frame(pnts)[,3:ncol(pnts)], th = 10)@results$Variables %>% as.character()

# Select the variables from the points ------------------------------------
pnts <- dplyr::select(pnts, lon, lat, all_of(vars))
pnts
write.csv(pnts, './tbl/ocurrence_vif-vars.csv', row.names = FALSE)

# Cor analisis for the remain variables -----------------------------------
mtrx <- pnts[,3:ncol(pnts)]

gcor <- ggpairs(mtrx) + 
  theme_light() +
  theme(
    strip.text = element_text(face = 'bold'), 
    panel.grid.major = element_blank(), 
    axis.text.x = element_text(size = 6), 
    axis.text.y = element_text(size = 6, angle = 90, hjust = 0.5)
  )

ggsave(plot = gcor, filename = './png/graphs/cor_ggpairs.png', units = 'in', width = 7, height = 7, dpi = 300)


