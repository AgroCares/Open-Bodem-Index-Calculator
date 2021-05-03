# Script to make some test data to test workability functions

# 1
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_SOILTYPE_AGR <-  'zeeklei'
B_GWL_GLG <- 90
B_GWL_GHG <- 35
t1 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 2
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 240  # schokkers
B_SOILTYPE_AGR <-  'zeeklei'
B_GWL_GLG <- 90
B_GWL_GHG <- 35
t2  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)
t2.2 <- calc_workability(A_CLAY_MI = c(28), A_SILT_MI = c(20),  B_LU_BRP = c(240),  B_SOILTYPE_AGR = c('zeeklei'),  B_GWL_GLG = c(90),  B_GWL_GHG = c(35))

# 3
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 370  # Rand grenzend aan....
B_SOILTYPE_AGR <-  'zeeklei'
B_GWL_GLG <- 90
B_GWL_GHG <- 35
t3  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 4
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_SOILTYPE_AGR <-  'zeeklei'
B_GWL_GLG <- 80
B_GWL_GHG <- 30
t4 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 5
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 980  #Lelie droogbloemen
B_SOILTYPE_AGR <-  'zeeklei'
B_GWL_GLG <- 90
B_GWL_GHG <- 35
t5  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 6 
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 259  
B_SOILTYPE_AGR <-  'loess'
B_GWL_GLG <- 80
B_GWL_GHG <- 50
t6 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 7 
A_CLAY_MI <- 10
A_SILT_MI <- 20
B_LU_BRP <- 265  
B_SOILTYPE_AGR <-  'veen'
B_GWL_GLG <- 105
B_GWL_GHG <- 15
t7 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

# 8 zoals Huinink (2018)
A_CLAY_MI <- 0
A_SILT_MI <- 14
B_LU_BRP <- 1929  
B_SOILTYPE_AGR <-  'dekzand'
B_GWL_GLG <- 150
B_GWL_GHG <- 25
B_Z_TWO <-76 
t8 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)

#9
t9 <- calc_workability(
  A_CLAY_MI = c(28, 28, 28, 28, 28, 28, 10, 0),
  A_SILT_MI = c(20, 20, 20, 20, 20, 20, 20, 14),
  B_LU_BRP = c(238, 240, 370, 238, 980, 259, 265, 1929),
  B_SOILTYPE_AGR = c('zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'loess', 'veen','dekzand'),
  B_GWL_GLG = c(90, 90, 90, 80, 90, 80, 105, 150),
  B_GWL_GHG = c(35, 35, 35, 30, 35, 50, 15, 25)
)
t9
calc_workability(28,20,238,'zeeklei',90,35)

# Test negative valuses for ghg and glg
t10 <- calc_workability(
  A_CLAY_MI = c(28, 28),
  A_SILT_MI = c(20, 20),
  B_LU_BRP = c(265, 265),
  B_SOILTYPE_AGR = c('zeeklei', 'zeeklei'),
  B_GWL_GLG = c(76, -5),
  B_GWL_GHG = c(-233, -448)
)

# New test values for testthat 7-4-2021 ===================
tdt <- readRDS('../OBIC functies bodembewerkbaarheid/dev/testdata.rds')
tdt[,I_WO := OBIC::calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP,
                                    B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_GWL_GLG = B_GWL_GLG, B_GWL_GHG = B_GWL_GHG)]
tdt <- merge.data.table(tdt,OBIC::crops.obic[,.(crop_code, crop_name)], by.x = 'B_LU_BRP', by.y = 'crop_code')
zcrit <- st_read('../OBIC functies bodembewerkbaarheid/brp_glg_ghg_zcrit2.gpkg')
zcritdt <- as.data.table(zcrit)
tdt <- merge.data.table(tdt, zcritdt[,.(ref_id, profiel_zcrit2)], by.x = 'ID', by.y = 'ref_id')
setnames(tdt, c('B_GLG', 'B_GHG', 'profiel_zcrit2', 'B_BT_AK'), c('B_GWL_GLG', 'B_GWL_GHG', 'B_Z_TWO', 'B_SOILTYPE_AGR'))
tdt_mini <- tdt[,.(ID,crop_name,A_CLAY_MI, A_SILT_MI,B_LU_BRP, B_SOILTYPE_AGR, B_GWL_GLG, B_GWL_GHG, B_GT, B_Z_TWO)]



t12 <- OBIC::calc_workability(
  A_CLAY_MI = c(15.6,13.6, 4.3, 22.6, 1.9, 2.9, 3.1, 4.3, 15.6),
  A_SILT_MI = c(16.7,30.5, 11.8, 36.6, 9.2, 8.6, 10.6, 11.8, 16.7),
  B_LU_BRP = c(233, 234, 236, 256, 259, 265, 265, 317, 2014),
  B_SOILTYPE_AGR = c('zeeklei','zeeklei', 'dekzand','zeeklei', 'dekzand', 'dekzand', 'veen', 'dekzand', 'zeeklei'),
  B_GWL_GLG = c(173,139, 106, 144, 115, 113, 42, 106, 173),
  B_GWL_GHG = c(21, 18, 62, 70, 49, 81, 9, 62, 21),
  B_Z_TWO = c()
)

t13sel <- tdt_mini[c(1,25,37,41,45,123,139,168,233)]
t13sel$I_WO <- OBIC::calc_workability(
  A_CLAY_MI = t13sel$A_CLAY_MI,
  A_SILT_MI = t13sel$A_SILT_MI,
  B_LU_BRP = t13sel$B_LU_BRP,
  B_SOILTYPE_AGR = t13sel$B_SOILTYPE_AGR,
  B_GWL_GLG = t13sel$B_GWL_GLG,
  B_GWL_GHG = t13sel$B_GWL_GHG,
  B_Z_TWO = t13sel$B_Z_TWO
)
# Als huinink
huin <- OBIC::calc_workability(
  A_CLAY_MI = 0,
  A_SILT_MI = 14,
  B_LU_BRP = 1929,  
  B_SOILTYPE_AGR =  'dekzand',
  B_GWL_GLG = 150,
  B_GWL_GHG = 25,
  B_Z_TWO =76
)

# Testing build with mock input data =============
obiin <- readRDS('../OBIC functies bodembewerkbaarheid/dev/obiin_pdf.rds')
# need to add glg and ghg
set.seed(5)
obiin[,B_GWL_GLG := sample.int(100, 910, replace = TRUE)]
obiin[,B_GWL_GHG := sample.int(100, 910, replace = TRUE)]
for(i in 1:nrow(obiin)){
  if(obiin[i,B_GWL_GHG]>obiin[i, B_GWL_GLG]) {
    x <- obiin[i,B_GWL_GHG]
    obiin[i,B_GWL_GHG := B_GWL_GLG]
    obiin[i,B_GWL_GLG := x]
  }
}
# Values may still be equal
obiin[,B_GWL_GLG := fifelse(B_GWL_GLG-B_GWL_GHG < 10, B_GWL_GLG+10,B_GWL_GLG)]

# Calculate OBIC
result <- OBIC::obic(obiin, FALSE)
# Error on OBIC::obic_evalmeasure because I_P_WO is missing in OBIC::recom.obic
# Adding I_P_WO to recom.obic
m.obic <- as.data.table(OBIC::recom.obic)
m.test <- m.obic[indicator == 'I_B_DI']
m.test <- m.test[,indicator := 'I_P_WO']
m.test <- m.test[,m_effect := NA]
m.test <- m.test[,m_applicability := NA]
recom.obic <- rbindlist(list(m.obic, m.test))
save(recom.obic, file = 'data/recom_obic.RData')

# Test OBIC again
result <- OBIC::obic(obiin, FALSE)
# Still doesn't work, for some reason updated table with values for I_P_WO isn't used
# Try function without recomendations
result <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = FALSE)

# STUDY BOFEK 2020 files
library(sf)
bof <- st_read('../OBIC functies bodembewerkbaarheid/dev/BOFEK2020_GIS/GIS/shp_files/bod_clusters.shp')

# proberen dag van juiste grondwaterstand goed te berekenen
# Functie van Huinink zou 127 als antwoord moeten geven maar geeft 150:
138-sin((-76-(25-150)/2)/((-25+150)/2))/0.0172024
# Ook asin ipv sin (Huinink gebruikt BOOGSIN of ARCSIN in excel denk ik) geeft niet het gewenste resultat
138-asin((-76-(25-150)/2)/((-25+150)/2))/0.0172024
# Plotten van de sinosoide geeft ook iets raars
x <- B_GWL_GLG:B_GWL_GHG
B_GWL_GHG <- 25
B_GWL_GLG <- 150

png(filename='dev/grondwaterstand_doy.png')
plot(x, 138-(asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024), xlab = "gewenste grondwaterstand onder maaiveld cm", ylab = "doy waarop grondwaterstand wordt bereikt")
dev.off()
png(filename = 'dev/plot.png')
plot(138-(asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024), x, xlab = "doy waarop grondwaterstand wordt bereikt", ylab = "gewenste grondwaterstand onder maaiveld cm")
dev.off()

library(ggpubr)
B_GWL_GHG <- 25
B_GWL_GLG <- 150
x <- B_GWL_GLG:B_GWL_GHG
gg <- ggplot(data = data.frame(x = B_GWL_GLG:B_GWL_GHG, B_GWL_GHG = B_GWL_GHG, B_GWL_GLG = B_GWL_GHG, y = 138-(asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024)),
             mapping = aes(x = x, y =y ))+
  theme_pubr() + geom_point()+
  xlab("gewenste grondwaterstand onder maaiveld cm") +
  ylab("doy waarop grondwaterstand wordt bereikt")
show(gg)
ggsave('dev/grondwater_doy.png')

# Plot van lengte van het groeiseizoen============
gg <- ggplot(data = data.frame(x = B_GWL_GLG:B_GWL_GHG, B_GWL_GHG = B_GWL_GHG, B_GWL_GLG = B_GWL_GHG, y = 2*(228-(138-(asin((-x-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024)))),
             mapping = aes(x = x, y =y ))+
  theme_pubr() + geom_point()+
  xlab("gewenste grondwaterstand onder maaiveld cm") +
  ylab("lengte bewerkbare seizoen")
show(gg)

y =0:99
s = 100
d = 1:100
plot(1:100,(s-d)/s)

# meerjarige akker en tuinbouwgewassen
RLG <- seq(0,1, 0.05)
plot(RLG,538*RLG^2-1144*RLG+606, ylab = 'opbrengstderving bieten etc. (%)')
plot(RLG,232*RLG^2-475*RLG+244, ylab = 'opbrengstderving zomergranen etc. (%)')
plot(RLG,(232*RLG^2-475*RLG+244)*0.85/2, ylab = 'opbrengstderving wintergranen etc. (%)')
plot(RLG,392*RLG^2-785*RLG+393, ylab = 'opbrengstderving aardappel etc. (%)')
gg <- ggplot() +
  geom_line(aes(x = RLG, y = y, color = id), data.frame(RLG = seq(0,1, 0.05), y = 538*RLG^2-1144*RLG+606, id = 'suikerbiet en zaaigroenten'))+
  geom_line(aes(x = RLG, y = y, color = id), data.frame(RLG = seq(0,1, 0.05), y = 232*RLG^2-475*RLG+244, id = 'zomergranen'))+
  geom_line(aes(x = RLG, y = y, color = id), data.frame(RLG = seq(0,1, 0.05), y = (232*RLG^2-475*RLG+244)*0.85/2, id = 'wintergranen'))+
  geom_line(aes(x = RLG, y = y, color = id), data.frame(RLG = seq(0,1, 0.05), y = 392*RLG^2-785*RLG+393, id = 'aardappel en plant groenten'))+
  geom_line(aes(x = RLG, y = y, color = id), data.frame(RLG = seq(0,1, 0.05), y = (538*RLG^2-1144*RLG+606)/2, id = 'boomteelt dahlia asperge'))+
  theme_pubr() + coord_cartesian(ylim = c(0,100)) + ylab("opbrengstderving %")

show(gg)

# Onvermijdelijke beginschade vanwege textuur categorie==========
season.obic[landuse %in% c('suikerbieten', 'schorseneren', 'was bospeen', 'erwten, bonen',
                           'tulpen, narcis, hyacint','fabrieksaardappelen', 'pootaardappelen',
                           'aardappelen', 'bladgroenten', 'prei, spruiten, koolsoorten', 'witlof, selderij, uien',
                           'klein fruit'), ylcat_texture := 'zaai, plant, pootgoed, klein fruit']
season.obic[landuse %in% c('zomergerst', 'snijmais', 'graszaad', 'wintertarwe'), ylcat_texture := 'granen']
season.obic[landuse %in% c('overige boomteelt', 'beweid bemaaid gras', 'loofbos', 'laanbomen_onderstammen', 'groot fruit', 'naaldbos'), ylcat_texture := 'gras, bos']

# aproxfun voor yieldloss grasland ======
apf <- approxfun(x = c(1, 0.9, 0.8, 0.6, 0.55, 0.43, 0.22, 0.08, 0),
                 y = c(23, 25, 28, 36, 38, 41, 54, 59, 100), method = 'linear',
                 yleft = NA_integer_, yright = NA_integer_)


# Make some testable data based on realistic parcels==============
library(data.table)
library(OBIC)
library(sf)
library(dplyr)
library(httr)

# Load data
parcels <- st_read('../OBIC functies bodembewerkbaarheid/dev/glg ghg/brp_glg_ghg.gpkg')
parcels.dt <-as.data.table(parcels)
# Select some parcels
sf <- st_as_sf(parcels[201:250,])

m2 <- st_transform(sf, 4326)
setDT(m2)
m2 <- m2[,.(ref_id, geom)] 
m2 <- st_as_sf(m2)
m2 <- st_centroid(m2)

m3 <- data.table(m2$ref_id, st_coordinates(m2))
setnames(m3,c('filename','lon','lat'))
fieldprop = list()

  for(i in 1:nrow(m2)){
    
    # extract field properties for each parcel
    request <- GET(url    = 'https://api.nmi-agro.nl/nl/field_estimates',
                   query  = list(lat=m3$lat[i], lon=m3$lon[i]),
                   add_headers(Authorization = paste("Bearer", Sys.getenv('NMI_API_KEY'), sep = " ")))
    
    # get output
    out1 <- content(request)
    
    # cultivation list
    cl =  which(grepl('cultiv',names(out1$data)))
    
    # convert to data.table
    out2 <- as.data.table(out1$data[-cl])
    
    # add crop codes 2016-2019
    out3 <- as.data.table(out1$data[cl]$cultivation_history)[2,]
    
    # combine crop codes
    out3 <- cbind(out2,b_lu_brp = unlist(out3),year = 2015:2019,field = m3$filename[i])
    
    # save in list
    fieldprop[[i]] <- copy(out3)
    
    print(i)
    saveRDS(fieldprop, '../OBIC functies bodembewerkbaarheid/dev/fieldprop.rds')
  }

m4 <- rbindlist(fieldprop)
sf.dt <- as.data.table(sf)
m4 <- merge.data.table(m4, sf.dt[,.(ref_id, glg, ghg)], by.x = 'field', by.y = 'ref_id')
setnames(m4, names(m4), toupper(names(m4)))
setnames(m4, c('REF_ID', 'GLG', 'GHG'), c('ID', 'B_GWL_GLG', 'B_GWL_GHG'))
# GLG en GHG omzetten van m naar cm
m4[,B_GWL_GLG := B_GWL_GLG*100]
m4[,B_GWL_GHG := B_GWL_GHG*100]

saveRDS(m4, '../OBIC functies bodembewerkbaarheid/dev/testdata.rds')
testdt <- m4[YEAR == 2019]
test <- calc_workability(A_CLAY_MI = testdt$A_CLAY_MI, A_SILT_MI = testdt$A_SILT_MI, B_LU_BRP = testdt$B_LU_BRP,
                         B_SOILTYPE_AGR = testdt$B_SOILTYPE_AGR, B_GWL_GLG = testdt$B_GWL_GLG, B_GWL_GHG = testdt$B_GWL_GHG)
# NaN's worden geproduceerd wanneer (-req_depth_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024) kleiner is dan -1 of groter dan 1
# in regels 144 en 145: dt[,req_spring_depth_day := round(138-(asin((-req_depth_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024))]
# Zoek uit waarom dit zo kan zijn
testdt$I_WO <- test

#plot waardes
round(138-(asin((-req_depth_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024)) # Formule waar het mis gaat
mokghg <- seq(1,201, 4)
req_depth_spring <- 60
plot(mokghg, (-req_depth_spring-0.5*(-mokghg-testdt$B_GWL_GLG))/(0.5*(-mokghg+testdt$B_GWL_GLG))/0.0172024)
plot(mokghg, (-req_depth_spring-0.5*(-mokghg-150))/(0.5*(-mokghg+150))/0.0172024)              

plot(145, (-req_depth_spring-0.5*(-145-150))/(0.5*(-145+150))/0.0172024) 
x <- seq(130,170,2)
x <- seq(1, 100,1)
plot(x, (-req_depth_spring-0.5*(-x-150))/(0.5*(-x+150))/0.0172024) 

# constante glg, toenemende ghg
req_depth_day <- c()
for(B_GWL_GHG in 1:149) {
  B_GWL_GLG <- 150
  req_depth_spring <- 60
  req_depth_day[B_GWL_GHG] <- round(138-(asin((-req_depth_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024))
}
plot(1:149,req_depth_day)
# toenemende glg en ghg
req_depth_day <- c()
for(B_GWL_GHG in 1:149) {
  B_GWL_GLG <- 150 +B_GWL_GHG
  req_depth_spring <- 60
  req_depth_day[B_GWL_GHG] <- round(138-(asin((-req_depth_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024))
}
plot(1:149,req_depth_day)

# De grondwaterstand op de y-as op dagnr x kan worden berekend uit 0.5*amplitude * SIN(x*0.0172024) of
# GWSdagx = -(-B_GWL_GHG-B_GWL_GLG)/2+(-B_GWL_GHG+B_GWL_GLG)/2*sin(0.0172142*(dagx +46))
GWSdagx <- -(-B_GWL_GHG-B_GWL_GLG)/2+(-B_GWL_GHG+B_GWL_GLG)/2*sin(0.0172142*(dagx +46))
gws_jaar <- c()
for(dagx in 1:365){
  gws_jaar[dagx] <- -(-B_GWL_GHG-B_GWL_GLG)/2+(-B_GWL_GHG+B_GWL_GLG)/2*sin(0.0172142*(dagx +46))
}
plot(1:365,gws_jaar)
# De dag x te waarop tussen 15 februari en 15 augustus een gegeven grondwaterstand y gemiddeld voor komt volgt uit:
# Dag x = - arcsin [ -(gwsy – gemiddelde gws) / amplitude van de golf] /0,0172142 + 138
B_GWL_GHG <- 20
B_GWL_GLG <- 120
req_depth_spring <- 20:121
# dagx <- - asin(-(req_depth_spring-(B_GWL_GHG+B_GWL_GLG)/2/(-B_GWL_GHG+B_GWL_GLG)/2))/0.0172142+138
dagx <- 138-(asin((-req_depth_spring-(-B_GWL_GHG-B_GWL_GLG)/2)/((-B_GWL_GHG+B_GWL_GLG)/2))/0.0172142) # the proper formula
plot(min(req_depth_spring):max(req_depth_spring),dagx)
# perhaps req_depth_spring is sometimes outside bounds of B_GWL_GLG and B_GWL_GHG
debug(calc_workability)
test <- calc_workability(A_CLAY_MI = testdt$A_CLAY_MI, A_SILT_MI = testdt$A_SILT_MI, B_LU_BRP = testdt$B_LU_BRP,
                         B_SOILTYPE_AGR = testdt$B_SOILTYPE_AGR, B_GWL_GLG = testdt$B_GWL_GLG, B_GWL_GHG = testdt$B_GWL_GHG)

# yield loss voor overig heeft een formule nodig:
rsltest <- seq(0,1,0.01)
100*rsltest^2-200*rsltest+100
plot(rsltest, 100*rsltest^2-200*rsltest+100)

# testobject
require(data.table)
field <- jsonlite::fromJSON("dev/tests/obi2.json")

dt <- data.table(ID = 362662,
                 B_SOILTYPE_AGR = field$b_soiltype_agr,
                 B_GWL_CLASS = field$b_gwl_class,
                 B_SC_WENR = field$b_sc_wenr,
                 B_HELP_WENR = field$b_help_wenr,
                 B_AER_CBS = 'Veenkoloniën en Oldambt',
                 B_LU_BRP = field$b_lu_brp,
                 A_SOM_LOI = field$a_som_loi,
                 A_SAND_MI = field$a_sand_mi,
                 A_SILT_MI = field$a_silt_mi,
                 A_CLAY_MI = field$a_clay_mi,
                 A_PH_CC = field$a_ph_cc,
                 A_CACO3_IF = field$a_caco3_if,
                 A_N_RT = field$a_n_rt,
                 A_CN_FR = field$a_cn_fr,
                 A_COM_FR = field$a_com_fr,
                 A_S_RT = field$a_s_rt,
                 A_N_PMN = field$a_n_pmn,
                 A_P_AL = field$a_p_al,
                 A_P_CC = field$a_p_cc,
                 A_P_WA = field$a_p_wa,
                 A_CEC_CO = field$a_cec_co,
                 A_CA_CO_PO = field$a_ca_co_po,
                 A_MG_CO_PO = field$a_mg_co_po,
                 A_K_CO_PO = field$a_k_co_po,
                 A_K_CC = field$a_k_cc,
                 A_MG_CC = field$a_mg_cc,
                 A_MN_CC = field$a_mn_cc,
                 A_ZN_CC = field$a_zn_cc,
                 A_CU_CC = field$a_cu_cc,
                 A_C_BCS = field$a_c_bcs,
                 A_CC_BCS = field$a_cc_bcs,
                 A_GS_BCS = field$a_gs_bcs,
                 A_P_BCS = field$a_p_bcs,
                 A_RD_BCS = field$a_rd_bcs,
                 A_EW_BCS = field$a_ew_bcs,
                 A_SS_BCS = field$a_ss_bcs,
                 A_RT_BCS = field$a_rt_bcs,
                 A_SC_BCS = field$a_sc_bcs,
                 M_COMPOST = field$m_compost,
                 M_GREEN = field$m_green,
                 M_NONBARE = field$m_nonbare,
                 M_EARLYCROP = field$m_earlycrop,
                 M_SLEEPHOSE = field$m_sleephose,
                 M_DRAIN = field$m_drain,
                 M_DITCH = field$m_ditch,
                 M_UNDERSEED = field$m_underseed,
                 M_LIME = TRUE,
                 M_NONINVTILL = TRUE,
                 M_SSPM = TRUE,
                 M_SOLIDMANURE = TRUE,
                 M_STRAWRESIDUE = TRUE,
                 M_MECHWEEDS = TRUE,
                 M_PESTICIDES_DST = TRUE
                 )

require(OBIC)

nsim = 50
out.list = list()
pb <- txtProgressBar(0,nsim)

  for(i in 1:nsim){
    
    # adapt input random
    dt[,B_SOILTYPE_AGR := sample(OBIC::soils.obic$soiltype,.N,replace = T)]
    dt[,B_GWL_CLASS := sample(unique(OBIC::waterstress.obic$gt),.N,replace = T)]
    dt[,B_LU_BRP := sample(OBIC::crops.obic$crop_code,.N,replace = T)]
    dt[, B_HELP_WENR := sample(unique(waterstress.obic$soilunit),.N,replace = T)]
    dt[,B_AER_CBS := sample(c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                              'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                              'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                              'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                              'Veenkoloni\xebn en Oldambt','Bouwhoek en Hogeland'),.N,replace = T)]
    check = sample(c(TRUE,FALSE),1)
    if(check){
      dt[,B_SC_WENR := sample(c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                                "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                                "Beperkt", "Zeer beperkt"),.N,replace = T)]
    } else {
      
      dt[,B_SC_WENR := sample(c('1', '2', '3', '4', "5", '10', '11', '401', '901', '902'),.N,replace = T)]
    }
    dt[,A_SOM_LOI := runif(.N,0.1,60)]
    dt[,A_CLAY_MI := runif(.N,0.1,100)]
    dt[,A_SAND_MI := pmax(0,runif(.N,0.1,100)-A_CLAY_MI)]
    dt[,A_SILT_MI := 100 - A_SAND_MI - A_CLAY_MI]
    dt[,A_PH_CC := runif(.N,3,10)]
    dt[,A_CACO3_IF := runif(.N,0.1,50)]
    dt[,A_N_RT := runif(.N,1,30000)]
    dt[,A_CN_FR := runif(.N,5,40)]
    dt[,A_COM_FR := runif(.N,.3,.8)]
    dt[,A_S_RT := runif(.N,1,10000)]
    dt[,A_N_PMN := runif(.N,1,500)]
    dt[,A_P_AL := runif(.N,1,250)]
    dt[,A_P_CC := runif(.N,0.1,100)]
    dt[,A_P_WA := runif(.N,1,250)]
    dt[,A_CEC_CO := runif(.N,1,1000)]
    dt[,A_CA_CO_PO := 0.9*runif(.N,1,100)]
    dt[,A_MG_CO_PO := 0.7 * pmax(0.1,runif(.N,0.1,50) - A_CA_CO_PO)]
    dt[,A_K_CO_PO := pmax(0.1,runif(.N,0.1,50) - A_CA_CO_PO - A_K_CO_PO)]
    
    dt[,A_K_CC := runif(.N,1,600)]
    dt[,A_MG_CC := runif(.N,1,1100)]
    dt[,A_MN_CC := runif(.N,0.1,60000)]
    dt[,A_ZN_CC := runif(.N,5,50000)]
    dt[,A_CU_CC := runif(.N,0.1,1000)]
    
    dt[,A_C_BCS := sample(c(0,1,2,NA),.N,replace = T)]
    dt[,A_CC_BCS := sample(c(0,1,2),.N,replace = T)]
    dt[,A_GS_BCS := sample(c(0,1,2),.N,replace = T)]
    dt[,A_P_BCS := sample(c(0,1,2,NA),.N,replace = T)]
    dt[,A_RD_BCS := sample(c(0,1,2),.N,replace = T)]
    dt[,A_EW_BCS := sample(c(0,1,2),.N,replace = T)]
    dt[,A_SS_BCS := sample(c(0,1,2,NA),.N,replace = T)]
    dt[,A_RT_BCS := sample(c(0,1,2),.N,replace = T)]
    dt[,A_SC_BCS := sample(c(NA,NA,NA),.N,replace = T)]
     
    dt[,M_COMPOST := runif(.N,0,50)]
    dt[,M_GREEN := sample(c(NA,NA),.N,replace = T)]
    dt[,M_NONBARE := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_EARLYCROP := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_SLEEPHOSE := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_DRAIN := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_DITCH := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_UNDERSEED := sample(c(TRUE,FALSE),.N,replace = T)]
    
    dt[,M_LIME := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_NONINVTILL := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_SSPM := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_SOLIDMANURE := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_STRAWRESIDUE := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_MECHWEEDS := sample(c(TRUE,FALSE),.N,replace = T)]
    dt[,M_PESTICIDES_DST := sample(c(TRUE,FALSE),.N,replace = T)]
    
    out.list[[i]] <- obic_field(dt$B_SOILTYPE_AGR,dt$B_GWL_CLASS,dt$B_SC_WENR,dt$B_HELP_WENR,dt$B_AER_CBS,
                                dt$B_LU_BRP, 
                                dt$A_SOM_LOI, dt$A_SAND_MI, dt$A_SILT_MI, dt$A_CLAY_MI,dt$A_PH_CC,dt$A_CACO3_IF,
                                dt$A_N_RT,dt$A_CN_FR,dt$A_COM_FR, dt$A_S_RT,dt$A_N_PMN,
                                dt$A_P_AL, dt$A_P_CC, dt$A_P_WA,
                                dt$A_CEC_CO,dt$A_CA_CO_PO, dt$A_MG_CO_PO, dt$A_K_CO_PO,
                                dt$A_K_CC, dt$A_MG_CC, dt$A_MN_CC, dt$A_ZN_CC, dt$A_CU_CC,
                                dt$A_C_BCS , dt$A_CC_BCS ,dt$A_GS_BCS ,dt$A_P_BCS ,dt$A_RD_BCS ,
                                dt$A_EW_BCS ,dt$A_SS_BCS ,dt$A_RT_BCS ,dt$A_SC_BCS ,
                                dt$M_COMPOST  ,dt$M_GREEN , dt$M_NONBARE , dt$M_EARLYCROP , 
                                dt$M_SLEEPHOSE ,dt$M_DRAIN ,dt$M_DITCH ,dt$M_UNDERSEED,
                                dt$M_LIME, dt$M_NONINVTILL, dt$M_SSPM, dt$M_SOLIDMANURE,
                                dt$M_STRAWRESIDUE,dt$M_MECHWEEDS ,dt$M_PESTICIDES_DST ,
                                
                                ID = 1)
    setTxtProgressBar(pb, i)
  }
close(pb)

  out = rbindlist(out.list) 
out


# test when BSC contains NA


