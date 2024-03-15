
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, corrplot, openxlsx, cowplot, ggpubr, RColorBrewer, readxl, GGally, psych, dismo, usdm, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data 
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- filter(wrld, name == 'Madagascar')
adm1 <- geodata::gadm(country = 'MDG', level = 1, path = './tmpr')
adm1 <- st_as_sf(adm1)

## Tabular data
pnts <- read_csv('./tbl/occurrence_outliers.csv', show_col_types = FALSE)
vars <- c('bio3', 'bio7', 'bio8', 'bio15', 'bio18')

## Raster data 
bioc.bsln <- terra::rast('./tif/wc/bioc.tif')
bioc.bsln <- bioc.bsln[[grep(paste0(vars, collapse = '|'), names(bioc))]]

names(bioc.bsln)

dirs.ftre <- dir_ls('./tif/c6')
ssps <- c('ssp246', 'ssp585')
prds <- c('2050', '2090')

## Labels for the variables 
lbls <- read.xlsx('./tbl/tables draft article.xlsx', sheet = 'Sheet2')
lbls <- mutate(lbls, Abbreviature = tolower(Abbreviature))
lbls <- mutate(lbls, Abbreviature = gsub(' ', '', Abbreviature))
lbls <- filter(lbls, Abbreviature %in% vars)
lbls <- mutate(lbls, name = c('Isothermality', 'Temperature annual range', 'Mean temperature of wettest quarter', 'Precipitation seasonality', 'Precipitation of warmest quarter'))

# Future ------------------------------------------------------------------
s <- 1
p <- 1

## To calculate the ensemble for the future -----
map(.x = 1:length(prds), .f = function(p){
    
  ssp <- ssps[s]
  prd <- prds[p]
    
  cat('>>> To process: ', ssp, ' ', prd, '\n')
  fls <- grep(ssp, dirs.ftre, value = T) %>% 
    grep(prd, ., value = T) %>% 
    as.character()
    
  ppt <- grep('prec', fls, value = T)
  tmx <- grep('tmax', fls, value = T)
  tmn <- grep('tmin', fls, value = T)
    
})

# Baseline climate maps  --------------------------------------------------
rs2tb <- function(x){x %>% terra::as.data.frame(., xy = T) %>% as_tibble() %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -c(gid, x, y))}
bioc.bsln.tble <- rs2tb(x = bioc.bsln)
bioc.bsln.tble <- mutate(bioc.bsln.tble, var = factor(var, levels = vars))
bioc.bsln.tble <- inner_join(bioc.bsln.tble, lbls, by = c('var' = 'Abbreviature'))
bioc.bsln.tble <- dplyr::select(bioc.bsln.tble, gid, x, y, var, name, value)
bioc.bsln.tble <- mutate(bioc.bsln.tble, name = factor(name, levels = lbls$name))

# Function to make the map ------------------------------------------------
map.bio <- function(tbl, vrb, clr){
  
  # tbl <- bioc.bsln.tble
  # vrb <- 'bio7'
  # clr <- 'YlOrRd'

  cat('To process: ', vrb, '\n')
  tbl <- filter(tbl, var %in% vrb)
  nme <- unique(tbl$name)
  smr <- quantile(tbl$value, seq(0, 1, 0.20))
  smr <- round(smr, 1)
  
  ggp <- ggplot() + 
    geom_tile(
      data = tbl,
      aes(
        x = x,
        y = y, 
        fill = value
      )
    ) + 
    scale_fill_gradientn(
      colors = rev(brewer.pal(n = 9, name = clr)), 
      breaks = smr, 
      labels = smr
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
      label = glue('{nme}')
    ) +
    labs(
      x = 'Lon', 
      y = 'Lat', 
      fill = nme
    ) + 
    coord_sf(
      xlim = ext(adm1)[1:2], 
      ylim = ext(adm1)[3:4]
    ) + 
    theme_light() + 
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5), 
      axis.text.y = element_text(size = 5, angle = 90, hjust = 0.5), 
      axis.text.x = element_text(size = 5),
      text = element_text(family = 'Segoe UI'),
      legend.position = 'bottom', 
      legend.key.width = unit(3, 'line')
    ) +
    guides(fill = guide_legend( 
      direction = 'horizontal',
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(13, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    ))
  
  ggsave(plot = ggp, filename = glue('./png/maps/climate/bsl_vles_{vrb}.png'), units = 'in', width = 4, height = 7, dpi = 300)
  cat('Done!\n')
  return(ggp)
  
}

# To make the maps --------------------------------------------------------

## Baseline maps --------
gg.bio3.bsl <- map.bio(tbl = bioc.bsln.tble, vrb = 'bio3', clr = 'Spectral')
gg.bio7.bsl <- map.bio(tbl = bioc.bsln.tble, vrb = 'bio7', clr = 'YlOrRd')
gg.bio8.bsl <- map.bio(tbl = bioc.bsln.tble, vrb = 'bio8', clr = 'YlOrRd')
gg.bi15.bsl <- map.bio(tbl = bioc.bsln.tble, vrb = 'bio15', clr = 'Blues')
gg.bi18.bsl <- map.bio(tbl = bioc.bsln.tble, vrb = 'bio18', clr = 'BrBG')

## Join baseline maps into only one
gg.bsl <- ggarrange(gg.bio3.bsl, gg.bio7.bsl, gg.bio8.bsl, gg.bi15.bsl, gg.bi18.bsl, ncol = 3, nrow = 2)
ggsave(plot = gg.bsl, filename = glue('./png/maps/climate/bsl_vles.jpg'), units = 'in', width = 10.5, height = 8, dpi = 300)
