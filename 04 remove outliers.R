

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, dismo, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Spatial data (raster / vect)
bioc <- terra::rast('./tif/wc/bioc.tif')
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- filter(wrld, name == 'Madagascar')
adm1 <- geodata::gadm(country = 'MDG', level = 1, path = './tmpr')
adm1 <- st_as_sf(adm1)

# Presence data 
occr <- read_csv('./tbl/occurrence.csv', show_col_types = FALSE)
occr <- dplyr::select(occr, name = scientificName, lon = decimalLongitude, lat = decimalLatitude)

# Tidy the points ---------------------------------------------------------

## To remove NAs 
nrow(occr)
occr <- drop_na(occr, lon, lat)

## To extract the values for the presences 
occr <- cbind(occr, terra::extract(bioc, occr[,c('lon', 'lat')]))
occr <- drop_na(occr)
occr <- as_tibble(occr)

## To calculate the scores (z normal)
pnts <- as.data.frame(occr)
norm <- scores(pnts[,5:ncol(pnts)], 'z')
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- NA
vars <- names(which(sapply(norm_na, function(x) sum(is.na(x))) > 0))
normpoints <- cbind(pnts[,c('lon', 'lat')], norm_na) %>% na.omit() %>% as.data.frame()
normpoints <- normpoints[,c('lon', 'lat')]

# Select the rows with outliers data
rows <- norm_na
rows <- rownames(rbind(rows[is.na(rows$bio7),], rows[is.na(rows$bio8),], rows[is.na(rows$bio10),], rows[is.na(rows$bio19),]))

# To select the dataframes (removed and no removed)
norm_na <- pnts[rows,]
norm_na <- dplyr::select(norm_na, name, lon, lat)
pnts <- pnts[setdiff(1:nrow(pnts), rows),] %>% dplyr::select(name, lon, lat)
pnts <- mutate(pnts, type = 'Keep')
norm_na <- mutate(norm_na, type = 'Removed')
pnts <- as_tibble(rbind(pnts, norm_na))
pnts <- mutate(pnts, type = factor(type, levels = c('Keep', 'Removed')))

# To make the map for the points removed and no removed in the outliner analysis --------

## To make the map  --------------------------------------------------------
gpnt <- ggplot() + 
  geom_sf(
    data = limt, 
    fill = NA, 
    col = 'grey50'
  ) + 
  geom_sf(
    data = wrld, 
    fill = NA, 
    col = 'grey80'
  ) +
  geom_sf(
    data = adm1, 
    fill = NA, 
    col = 'grey50'
  ) +
  geom_point(
    data = pnts, 
    aes(
      x = lon, 
      y = lat, 
      col = type
    ), 
    pch = 18
  ) +
  scale_color_manual(
    values = c('#86B404', '#8A0829')
  ) +
  ggtitle(
    label = 'Points removed after the outlier analysis'
  ) +
  coord_sf(
    xlim = c(42.5, 51.5), 
    ylim = c(-25.3, -12.3),
  ) + 
  labs(
    x = 'Lon', 
    y = 'Lat', 
    col = ''
  ) +
  theme_light() + 
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5, family = 'Segoe UI'),
    text = element_text(family = 'Segoe UI'),
    axis.title = element_text(size = 6, face = 'bold'),
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5),
    axis.text.x = element_text(size = 5), 
    legend.position = 'bottom', 
    legend.key.width = unit(3, 'line'), 
    legend.title = element_text(face = 'bold')
  ) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  annotation_scale(location =  "br", width_hint = 0.3, text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.12, "in"), 
                         style = north_arrow_fancy_orienteering(text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = gpnt, filename = './png/maps/fig 02 outliers dataset.png', units = 'in', width = 4, height = 6, dpi = 300)

#