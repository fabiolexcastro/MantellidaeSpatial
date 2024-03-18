


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, corrplot, colourpicker, raptr, openxlsx, cowplot, gridExtra, ggpubr, ghibli, RColorBrewer, readxl, GGally, psych, dismo, usdm, outliers, showtext, ggspatial, sf, ggrepel, extrafont, tidyverse, glue, rnaturalearthdata, rnaturalearth, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
occr <- read_csv('./tbl/ocurrence_pseudo-absencse.csv', show_col_types = FALSE)
bioc <- terra::rast('./tif/wc/bioc.tif')
vars <- read_csv('./tbl/ocurrence_vif-vars.csv', show_col_types = FALSE) %>% select(starts_with('bio')) %>% colnames()

# To extract the values for the presences and the pseudo-absences ---------
occr <- as_tibble(cbind(occr, terra::extract(bioc, occr[,c('lon', 'lat')])))
occr <- dplyr::select(occr, lon, lat, pb, all_of(vars))
colnames(occr)[1:2] <- c('x', 'y')

## To write ----------
write.csv(occr, './tbl/ocurrence_pseudo-absencse_swd.csv', row.names = FALSE)
vrs <- vars

# Select the variables for the bioc stack ---------------------------------
bioc <- bioc[[grep(paste0(vars, collapse = '|'), names(bioc), value = T)]]

# To make the cross-validation for the maxent model
occ <- occr %>% filter(pb == 1)
bck <- occr %>% filter(pb == 0)

fld_occ <- kfold(occ, k = 5)
fld_bck <- kfold(bck, k = 5)

mdls <- map(
  
  1:25, 
  function(k){
    
    cat('To make the model: ', k, '\n')
    
    # Filtering cross-validation ------
    
    ## Presences -----
    tst <- occ[fld_occ == k,]
    trn <- occ[fld_occ != k,]
    
    ## Pseudoabsences -----
    tst_bck <- bck[fld_bck == k,]
    trn_bck <- bck[fld_bck != k,]
    
    ## Join both into only one ------
    env <- rbind(trn, trn_bck)
    y <- c(trn$pb, trn_bck$pb)
    out <- glue('./models/maxent/mantellidae/run_1/model_{k}/')
    ifelse(!file.exists(out), dir_create(out), print('Directorio existe'))
    
    ## Filtering the variables for the analysis -----
    env <- dplyr::select(env, x, y, pb, all_of(vars))
    dta <- env
    
    ## Make the model and predict --------
    mxn <- dismo::maxent(env[,4:ncol(env)], y, argcs = c('addsamplestobackground=true', 'responsecurves'), path = out)
    rst <- terra::predict(mxn, bioc, progress = 'txt')
    
    ## To evaluate the model -------
    evl <- evaluate(mxn, p = as.data.frame(tst[,4:ncol(tst)]), a = data.frame(tst_bck[,4:ncol(tst_bck)]))
    prc <- as.data.frame(mxn@results)
    prc <- data.frame(variables = vrs, percentage = prc[grep('contribution', rownames(prc)),], rutin = k)
    auc <- evl@auc
    tss <- evl@TPR + evl@TNR - 1
    tss <- evl@t[which.max(tss)]
    dfm <- data.frame(routine = k, threshold = tss, auc = auc)
    
    ## To save the files -------
    saveRDS(object = mxn, file = glue('{out}/mxn.rds'))
    terra::writeRaster(x = rst, filename = glue('{out}/predict_mdl_{k}.tif'), overwrite = TRUE)
    cat('---Done---!\n')
    return(list(rst, prc, dfm))
    
  }
  
)

rstr <- reduce(map(mdls, 1), c)
porc <- map_dfr(mdls, 2)
dfrm <- map_dfr(mdls, 3)

# To write the final files
dire <- glue('./models/maxent/{nme}/run_1')

terra::writeRaster(
  x = rstr, 
  filename = glue('{dire}/predict_crnt_all-models.tif'), 
  overwrite = TRUE
)

write.csv(
  porc, 
  glue('./{dire}/percentage_contribution.csv'),
  row.names = FALSE
)

write.csv(
  dfrm,
  glue('./{dire}/thresholds_auc.csv'), 
  row.names = FALSE
)

write.csv(
  tbl, 
  glue('./{dire}/occ_bck.csv'),
  row.names = FALSE
)

rm(rstr, porc, mxn); gc(reset = T)
cat('Done!\n')

}
