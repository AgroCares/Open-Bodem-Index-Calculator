# Script to assign GHG and GLG to every parcel in brp2019
# Brent april 2021
library(data.table)
library(sf)
library(exactextractr)
library(raster)
library(lwgeom)

# Data
glg <- raster('../OBIC functies bodembewerkbaarheid/dev/LHM GLG_2011-2018_L1.tif')
ghg <- raster('../OBIC functies bodembewerkbaarheid/dev/LHM GHG_2011-2018_L1.tif')

if(!file.exists('../OBIC functies bodembewerkbaarheid/dev/brp19.RData')){
  brp <- st_read('../OBIC functies bodembewerkbaarheid/dev/brpgewaspercelen_2019.gpkg')
  brp <- st_transform(brp, 28992)
  save(brp, file ='../OBIC functies bodembewerkbaarheid/dev/brp19.Rdata')
} else {load('../OBIC functies bodembewerkbaarheid/dev/brp19.Rdata')}

bod50 <- st_read('../OBIC functies bodembewerkbaarheid/dev/Bodemkaart50.shp')

bod50 <- st_transform(bod50, 28992)
brp <- st_transform(brp, 28992)

st_crs(brp) == st_crs(bod50)

# Add gwt from bod50 to brp
brp <- st_make_valid(brp)
bod50 <- st_make_valid(bod50)
# make brp centroid
brp.centroid <- st_centroid(brp)
brp_gwt <- st_join(brp.centroid, bod50, join = st_intersects)

# Remove unwanted columns
cols <- names(brp)
cols <- append(cols, 'GWT')
cols <- append(cols, 'OBJECTID')
brp_water <- brp_gwt[,cols]

# Merge brp.centroid en brp
brp2 <- as.data.table(brp)
setDT(brp_water)
brp2 <- merge.data.table(brp2, brp_water[,.(ref_id, OBJECTID, GWT)], by = 'ref_id')
brp2 <- st_as_sf(brp2)

# Extract ghg and glg
brp2$glg <- exact_extract(glg, brp2, fun = 'mean', progress = TRUE)
brp2$ghg <- exact_extract(ghg, brp2, fun = 'mean', progress = TRUE)

# Intermediate save of object
saveRDS(brp2, '../OBIC functies bodembewerkbaarheid/dev/temp_brp_glg_ghg.rds')

# Identify negative or empty glg and ghg
brp2dt <- as.data.table(brp2)
misval <- brp2dt[is.na(glg)|is.na(ghg)]
misval_sf <- st_as_sf(misval)

hasval <- brp2dt[!is.na(glg)&!is.na(ghg)]
hasval_sf <- st_as_sf(hasval)

# Assign glg and ghg to missing or negative parcels based on value of nearest parcel with value
near <- st_nearest_feature(misval_sf, hasval_sf)

misval <- misval[,glg := brp2dt[near,glg]]
misval <- misval[,ghg := brp2dt[near,ghg]]
misval <- st_as_sf(misval)
setDT(misval)

# Add new values to brp2dt
brp2dt <- brp2dt[!ref_id %in% misval$ref_id]
brp2dt <- rbindlist(list(brp2dt, misval))

# Check for negative values
negval <- brp2dt[glg<0 | ghg<0]
negval_sf <- st_as_sf(negval)

# ~13100 parcels with negative values (which is weird and probably due to the raster not covering the whole country)
# calculate average glg and ghg per GWT
brp2dt <- brp2dt[,glg_mean := mean(glg), by = GWT]
brp2dt <- brp2dt[,ghg_mean := mean(ghg), by = GWT]

# assign mean values to negative values (if they have a gwt)
brp2dt <- brp2dt[glg<0, glg := glg_mean]
brp2dt <- brp2dt[ghg<0, ghg := ghg_mean]

# Save file
brp_glg_ghg <- st_as_sf(brp2dt)
saveRDS(brp_glg_ghg, '../OBIC functies bodembewerkbaarheid/dev/glg ghg/brp_glg_ghg.rds')
st_write(brp_glg_ghg, '../OBIC functies bodembewerkbaarheid/dev/glg ghg/brp_glg_ghg.gpkg')

#### Check wether glg and ghg fit the grondwater trap classification #######
sf <- st_read('../OBIC functies bodembewerkbaarheid/dev/glg ghg/brp_glg_ghg.gpkg')
dt <- as.data.table(sf)
dt <- dt[,.(ref_id, GWS_GEWASCODE,GWT, glg, ghg, glg_mean, ghg_mean)]

# convert glg and ghg from m to cm
dt[, glg := glg*100]
dt[, ghg := ghg*100]

# Check if glg and ghg fall in groundwater trap (according to wikipedia)
dt[GWT == 'I', ghgcheck := fifelse(ghg<20, TRUE, FALSE)]
dt[GWT == 'I', glgcheck := fifelse(glg<50, TRUE, FALSE)]

dt[GWT == 'II', ghgcheck := fifelse(ghg<40, TRUE, FALSE)]
dt[GWT == 'II', glgcheck := fifelse(glg>50 & glg <=80, TRUE, FALSE)]

dt[GWT == 'IIb', ghgcheck := fifelse(ghg<40 & ghg>25, TRUE, FALSE)]
dt[GWT == 'IIb', glgcheck := fifelse(glg>50 & glg <=80, TRUE, FALSE)]

dt[GWT == 'III', ghgcheck := fifelse(ghg<40, TRUE, FALSE)]
dt[GWT == 'III', glgcheck := fifelse(glg>80 & glg <=120, TRUE, FALSE)]

dt[GWT == 'IIIb', ghgcheck := fifelse(ghg<40 & ghg >25, TRUE, FALSE)]
dt[GWT == 'IIIb', glgcheck := fifelse(glg>80 & glg <=120, TRUE, FALSE)]

dt[GWT == 'IV', ghgcheck := fifelse(ghg>40, TRUE, FALSE)]
dt[GWT == 'IV', glgcheck := fifelse(glg>80 & glg <=120, TRUE, FALSE)]

dt[GWT == 'V', ghgcheck := fifelse(ghg<40, TRUE, FALSE)]
dt[GWT == 'V', glgcheck := fifelse(glg > 120, TRUE, FALSE)]

dt[GWT == 'VI', ghgcheck := fifelse(ghg > 40 & ghg < 80, TRUE, FALSE)]
dt[GWT == 'VI', glgcheck := fifelse(glg > 120, TRUE, FALSE)]

dt[GWT == 'VII', ghgcheck := fifelse(ghg > 80, TRUE, FALSE)]

dt[GWT == 'VIII', ghgcheck := fifelse(ghg > 140, TRUE, FALSE)]

dtgwt <- dt[!is.na(ghgcheck)]
dtgwt_false <- dtgwt[ghgcheck == FALSE| glgcheck == FALSE]

# conclusion: 424 thousand out of 614 thousand parcels do not have matching gwt with glg or ghg