library(sf)
library(data.table)
library(raster)
# library(terra)
library(exactextractr)

glg <- raster('../OBIC functies bodembewerkbaarheid/dev/LHM GLG_2011-2018_L1.tif')
ghg <- raster('../OBIC functies bodembewerkbaarheid/dev/LHM GHG_2011-2018_L1.tif')
# crs(glg)

# glg <- rast('dev/LHM GLG_2011-2018_L1.tif')
# ghg <- rast('dev/LHM GLG_2011-2018_L1.tif')

if(!file.exists('../OBIC functies bodembewerkbaarheid/dev/brp19.RData')){
  brp <- st_read('../OBIC functies bodembewerkbaarheid/dev/brpgewaspercelen_2019.gpkg')
  st_transform(brp, 28992)
  save(brp, file ='../OBIC functies bodembewerkbaarheid/dev/brp19.Rdata')
} else {load('../OBIC functies bodembewerkbaarheid/dev/brp19.Rdata')}

# Test how this works on small scale
brp5 <- head(brp)
b5 <- exact_extract(glg, brp5, 'mean', progress = TRUE)
brp5$glg <- b5
brp5$ghg <- exact_extract(ghg, brp5, 'mean', progress = TRUE)

# Add ghg and glg to brp
brp_water <- brp
brp_water$glg <- exact_extract(glg, brp_water, 'mean', progress = TRUE)
brp_water$ghg <- exact_extract(ghg, brp_water, 'mean', progress = TRUE)

# Save brp_water
saveRDS(brp_water, '../OBIC functies bodembewerkbaarheid/dev/brp_water.rds')

# Extract values that were missing with original raster resolution
dt.brp <- as.data.table(brp_water)
brp_lr <- dt.brp[is.na(glg)|is.na(ghg)]
brp_lr <- brp_lr[, glg := NULL]
brp_lr <- brp_lr[, ghg := NULL]
brp_lr <- st_as_sf(brp_lr)

# aggregate glg and ghg
glg.agg <- raster::aggregate(glg, fact = 2)
ghg.agg <- raster::aggregate(ghg, fact = 2)

brp_lr_water <- brp_lr
brp_lr_water$glg <- exact_extract(glg.agg, brp_lr_water, 'mean', progress = TRUE)
brp_lr_water$ghg <- exact_extract(ghg.agg, brp_lr_water, 'mean')

# Select last points with super low resolution
dt.brp <- as.data.table(brp_lr_water)
brp_slr <- dt.brp[is.na(glg)|is.na(ghg)]

# aggregate glg and ghg
glg.agg <- raster::aggregate(glg, fact = 10)
ghg.agg <- raster::aggregate(ghg, fact = 10)

brp_slr_water <- brp_slr
brp_slr_water$glg <- exact_extract(glg.agg, brp_slr_water, 'mean', progress = TRUE)
brp_slr_water$ghg <- exact_extract(ghg.agg, brp_slr_water, 'mean')
brp_slr_water <- st_as_sf(brp_slr_water)

# replace empty values in brp_water with lower res values
setDT(brp_water)
setDT(brp_lr_water)
brp_water <- brp_water[!is.na(glg)]
brp_water <- rbindlist(list(brp_water, brp_lr_water))

saveRDS(brp_water, '../OBIC functies bodembewerkbaarheid/dev/brp_water2.rds')
