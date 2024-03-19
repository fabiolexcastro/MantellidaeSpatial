
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, corrplot, colourpicker, ggnewscale, cptcity, raptr, openxlsx, cowplot, gridExtra, ggpubr, ghibli, RColorBrewer, readxl, GGally, psych, dismo, usdm, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

## Fit the font 
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Load data ---------------------------------------------------------------

## Raster data -----------
rstr <- terra::rast('./models/maxent/mantellidae/run_1/predict_crnt_all-models.tif')
avrg <- mean(rstr)

## Vector data -----------
wrld <- ne_countries(returnclass = 'sf', scale = 50) 
limt <- filter(wrld, name == 'Madagascar')
adm1 <- geodata::gadm(country = 'MDG', level = 1, path = './tmpr')
adm1 <- st_as_sf(adm1)

## Altitude data ---------
srtm <- terra::rast('./tif/dem/srtm_fill.tif')

# To make the maps --------------------------------------------------------

## Raster to table ------------
tble <- avrg %>% 
  terra::as.data.frame(., xy = T) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value'))

srtm <- srtm %>% 
  terra::as.data.frame(., xy = T) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'srtm'))

## To make the map ------------

ggp <- ggplot() + 
  geom_tile(
    data = srtm, 
    aes(
      x = x,  
      y = y, 
      fill = srtm
    )
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(
      n = 9, name = 'Greys'
    ), 
    guide = FALSE
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = tble,
    aes(
      x = x,
      y = y, 
      fill = value
    ), 
    alpha = 0.75
  ) + 
  scale_fill_gradientn(
    colors = 
      cpt(
        pal = 'imagej_gyr_centre', 
        n = 10, 
        rev = TRUE
      ), 
    breaks = seq(0, 1, 0.1), 
    labels = seq(0, 1, 0.1)
  ) +
  geom_sf(
    data = wrld, 
    fill = NA, 
    col = 'grey50'
  ) +
  geom_sf(
    data = limt, 
    fill = NA, 
    col = 'grey40'
  ) + 
  geom_sf(
    data = adm1,
    fill = NA, 
    col = 'grey30'
  ) +
  ggtitle(
    label = glue('Suitability score for the baseline')
  ) +
  labs(
    x = 'Lon', 
    y = 'Lat', 
    fill = 'Suitability\nscore'
  ) + 
  coord_sf(
    xlim = ext(adm1)[1:2], 
    ylim = ext(adm1)[3:4]
  ) + 
  theme_light() + 
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5), 
    axis.text.y = element_text(size = 4.4, angle = 90, hjust = 0.5), 
    axis.text.x = element_text(size = 4.4),
    text = element_text(family = 'Segoe UI'),
    legend.position = 'right'
  ) + 
  guides(
    fill = guide_legend(
      direction = 'vertical',
      keyheight = unit(14, units = 'mm'), 
      title.position = 'top', 
      title.hjust = 0.5
    )
  ) +
  annotation_scale( 
    location =  "br", width_hint = 0.3, text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.1
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "true", 
    pad_x = unit(0.08, "in"), pad_y = unit(0.12, "in"), 
    style = north_arrow_fancy_orienteering(
      text_col = 'grey40', 
      line_col = 'grey60', 
      fill = c('grey60', 'grey99')
    )
  ) 

ggp
ggsave(plot = ggp, filename = './png/maps/suitability/suitability_current-average_run1.jpg', units = 'in', width = 5.5, height = 8, dpi = 300)


