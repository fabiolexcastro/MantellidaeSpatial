
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, dismo, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Vector data - Down ------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- wrld[wrld$name == 'Madagascar',]
adm1 <- gadm(country = 'MDG', level = 1, path = './tmpr')
adm1 <- st_as_sf(adm1)

# Raster data - SRTM  -----------------------------------------------------
srtm <- terra::rast('./tif/dem/srtm_fill.tif')

# Tabular data - presences ------------------------------------------------
occr <- read_csv('./tbl/occurrence.csv', show_col_types = F)
occr <- dplyr::select(occr, name = scientificName, lon = decimalLongitude, lat = decimalLatitude)

# Rasster to table --------------------------------------------------------
srtm <- terra::as.data.frame(srtm, xy = T) %>% as_tibble()

# Coordinates for the labels ----------------------------------------------
crds <- adm1 %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% mutate(name = adm1$NAME_1)

# To make the map  --------------------------------------------------------
gpnt <- ggplot() + 
  geom_tile(
    data = srtm, 
    aes(
      x = x, 
      y = y,
      fill = srtm_fill
    ), 
    alpha = 0.5
  ) + 
  scale_fill_gradientn(
    colors = terrain.colors(n = 10), 
    labels = seq(0, 2500, 500), 
    breaks = seq(0, 2500, 500)
  ) +
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
    data = occr, 
    aes(
      x = lon, 
      y = lat
    ), 
    pch = 18, 
    col = '#5F4C0B', 
    size = 0.7,
    bg.colour = "white", 
    bg.r = .2
  ) + 
  geom_text_repel(
    data = crds, 
    aes(
      x = X,
      y = Y, 
      label = name
    ), 
    family = 'Segoe UI', 
    size = 1.4,
    bg.colour = "white", 
    bg.r = 0.5
  ) +
  ggtitle(
    label = expression(bold(paste("Spatial ocurrence for ", italic("Mantellidae"), " at Madagascar")))
  ) +
  coord_sf(
    xlim = c(42.5, 51.5), 
    ylim = c(-25.3, -12.3),
  ) + 
  labs(
    x = 'Lon', 
    y = 'Lat', 
    fill = 'SRTM (m.a.s.l.)'
  ) +
  theme_light() + 
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5, family = 'Segoe UI'),
    text = element_text(family = 'Segoe UI'),
    axis.title = element_text(size = 6, face = 'bold'),
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 5),
    axis.text.x = element_text(size = 5), 
    legend.position = 'bottom', 
    legend.key.width = unit(3, 'line') 
  ) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) +
  annotation_scale(location =  "br", width_hint = 0.3, text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.12, "in"), 
                         style = north_arrow_fancy_orienteering(text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

gpnt
ggsave(plot = gpnt, filename = './png/maps/location_occ.png', units = 'in', width = 4, height = 6, dpi = 300)





