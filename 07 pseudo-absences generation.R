


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, corrplot, openxlsx, cowplot, gridExtra, ggpubr, ghibli, RColorBrewer, readxl, GGally, psych, dismo, usdm, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------


