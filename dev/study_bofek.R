# see what is in BOFEK files

# Brent Riechelman 2020

# Load packages
library(sf)
library(data.table)

# Load data
bofek <- st_read('../OBIC functies bodembewerkbaarheid/dev/bofek2012.gpkg')
names(bofek)
summary(bofek$bf.lutum_d1)
class(bofek$BOFEK2012)
unique(bofek$BOFEK2012)

# shape file 2020 has some soil units
bof <- st_read('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/GIS/shp_files/bod_clusters.shp')
ws.obic <- OBIC::waterstress.obic
bofdt <- as.data.table(bof)
bof2 <- st_as_sf(bofdt[BODEMCODE %in% ws.obic$soilunit])

bodemcode_not_in_ws <- !bof$BODEMCODE %in% bof2$BODEMCODE 
