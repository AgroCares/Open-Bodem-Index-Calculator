# see what is in BOFEK files

# Brent Riechelman 2020

# Load packages
library(sf)
library(data.table)
library(openxlsx)

# Load data
# bofek <- st_read('../OBIC functies bodembewerkbaarheid/dev/bofek2012.gpkg')
# names(bofek)
# summary(bofek$bf.lutum_d1)
# class(bofek$BOFEK2012)
# unique(bofek$BOFEK2012)

# shape file 2020 has some soil units
bof <- st_read('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/GIS/shp_files/bod_clusters.shp')
ws.obic <- OBIC::waterstress.obic
bofdt <- as.data.table(bof)
bof2 <- st_as_sf(bofdt[BODEMCODE %in% ws.obic$soilunit])

bodemcode_not_in_ws <- !bof$BODEMCODE %in% bof2$BODEMCODE 

# See if BOFEK2020 file is linkable to tabel_BOFEK2020
bof <- st_read('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/GIS/shp_files/bod_clusters.shp')
tab <- read.xlsx('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/kaart+tabel/tabel_BOFEK_2020_V_1_0.xlsx', sheet = 2, startRow = 10)
setDT(bof) # in bof, OBJECTID_1 is the unique Identifier
setDT(tab) # in tab, Profiel is the unique identifier
uniqueN(bof$BODEMCODE) # 1717
uniqueN(tab$Bodem)    #316
head(unique(bof$BODEMCODE),20)
head(unique(bof[!grepl('^\\|',BODEMCODE),BODEMCODE]),20)
uniqueN(bof[!grepl('^\\|',BODEMCODE),BODEMCODE])
head(unique(bof[!grepl('^\\|',BODEMCODE),BODEMCODE]),200)
uniqueN(bof$BODEM1) # 318
unique(bof[!BODEM1 %in% tab$Bodem, BODEM1])
unique(tab[!Bodem %in% bof$BODEM1,Bodem ])
unique(tab[Bodem %in% bof$BODEM1,Bodem ])

# laad bodemkaart50 
bod <- st_read('../OBIC functies bodembewerkbaarheid/dev/bodemkaart50.gpkg')
bodshp <- st_read('../OBIC functies bodembewerkbaarheid/dev/Bodemkaart50.shp')
uniqueN(bodshp$CODE) # 5872
uniqueN(bodshp$LETTER) #204
uniqueN(bodshp$OBJECTID) #67898

# bodschat
bodschat <- st_read('../OBIC functies bodembewerkbaarheid/dev/brp_2019_bodemschat_N2.gpkg')
st_transform(bodschat, 28992)

# Merge BOFEK2020 en BOFEK2020 Zcrit data
test <- merge.data.table(bof, tab[,.(Bodem, Profiel, Cluster, zcrit2)], by.x = 'BODEMCODE', by.y = 'Bodem')
test <- merge.data.table(bodshp.dt, tab[,.(Bodem, Profiel, Cluster, zcrit2)], by.x = 'EERSTE_BOD', by.y = 'Bodem')

# Read geodatabase
bofdb <- st_read('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/GIS/BOFEK2020_bestanden/BOFEK2020.gdb')
