# Script to make some test data to test workability functions

# 1
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t1 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 2
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 240  # schokkers
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t2  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)
t2.2 <- calc_workability(A_CLAY_MI = c(28), A_SILT_MI = c(20),  B_LU_BRP = c(240),  B_BT_AK = c('zeeklei'),  B_GLG = c(90),  B_GHG = c(35))

# 3
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 370  # Rand grenzend aan....
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t3  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 4
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_BT_AK <-  'zeeklei'
B_GLG <- 80
B_GHG <- 30
t4 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 5
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 980  #Lelie droogbloemen
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t5  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 6 
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 259  
B_BT_AK <-  'loess'
B_GLG <- 80
B_GHG <- 50
t6 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 7 
A_CLAY_MI <- 10
A_SILT_MI <- 20
B_LU_BRP <- 265  
B_BT_AK <-  'veen'
B_GLG <- 105
B_GHG <- 15
t7 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 8 zoals Huinink (2018)
A_CLAY_MI <- 0
A_SILT_MI <- 14
B_LU_BRP <- 1929  
B_BT_AK <-  'dekzand'
B_GLG <- 150
B_GHG <- 25
t8 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

#9
t9 <- calc_workability(
  A_CLAY_MI = c(28, 28, 28, 28, 28, 28, 10, 0),
  A_SILT_MI = c(20, 20, 20, 20, 20, 20, 20, 14),
  B_LU_BRP = c(238, 240, 370, 238, 980, 259, 265, 1929),
  B_BT_AK = c('zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'loess', 'veen','dekzand'),
  B_GLG = c(90, 90, 90, 80, 90, 80, 105, 150),
  B_GHG = c(35, 35, 35, 30, 35, 50, 15, 25)
)
t9
calc_workability(28,20,238,'zeeklei',90,35)

# Test negative valuses for ghg and glg
t10 <- calc_workability(
  A_CLAY_MI = c(28, 28),
  A_SILT_MI = c(20, 20),
  B_LU_BRP = c(265, 265),
  B_BT_AK = c('zeeklei', 'zeeklei'),
  B_GLG = c(76, -5),
  B_GHG = c(-233, -448)
)

# Testing build with mock input data
obiin <- readRDS('../OBIC functies bodembewerkbaarheid/dev/obiin_pdf.rds')
# need to add glg and ghg
set.seed(5)
obiin[,B_GLG := sample.int(100, 910, replace = TRUE)]
obiin[,B_GHG := sample.int(100, 910, replace = TRUE)]
for(i in 1:nrow(obiin)){
  if(obiin[i,B_GHG]>obiin[i, B_GLG]) {
    x <- obiin[i,B_GHG]
    obiin[i,B_GHG := B_GLG]
    obiin[i,B_GLG := x]
  }
}
# Values may still be equal
obiin[,B_GLG := fifelse(B_GLG-B_GHG < 10, B_GLG+10,B_GLG)]

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
x <- B_GLG:B_GHG
B_GHG <- 25
B_GLG <- 150

png(filename='dev/grondwaterstand_doy.png')
plot(x, 138-(asin((-x-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024), xlab = "gewenste grondwaterstand onder maaiveld cm", ylab = "doy waarop grondwaterstand wordt bereikt")
dev.off()
png(filename = 'dev/plot.png')
plot(138-(asin((-x-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024), x, xlab = "doy waarop grondwaterstand wordt bereikt", ylab = "gewenste grondwaterstand onder maaiveld cm")
dev.off()

library(ggpubr)
B_GHG <- 25
B_GLG <- 150
x <- B_GLG:B_GHG
gg <- ggplot(data = data.frame(x = B_GLG:B_GHG, B_GHG = B_GHG, B_GLG = B_GHG, y = 138-(asin((-x-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024)),
             mapping = aes(x = x, y =y ))+
  theme_pubr() + geom_point()+
  xlab("gewenste grondwaterstand onder maaiveld cm") +
  ylab("doy waarop grondwaterstand wordt bereikt")
show(gg)
ggsave('dev/grondwater_doy.png')

# Plot van lengte van het groeiseizoen
gg <- ggplot(data = data.frame(x = B_GLG:B_GHG, B_GHG = B_GHG, B_GLG = B_GHG, y = 2*(228-(138-(asin((-x-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024)))),
             mapping = aes(x = x, y =y ))+
  theme_pubr() + geom_point()+
  xlab("gewenste grondwaterstand onder maaiveld cm") +
  ylab("lengte bewerkbare seizoen")
show(gg)

y =0:99
s = 100
d = 1:100
plot(1:100,(s-d)/s)


# Modify season.obic to add more info
season.obic <- season.obic
# Eenjarige tuin en akkerbouw gewassen
season.obic[landuse %in% c('suikerbieten', 'schorseneren', 'was bospeen', 'erwten, bonen', 'tulpen, narcis, hyacint'), ylcat_season := "gezaaide groenten"]
season.obic[landuse %in% c('zomergerst', 'snijmais', 'graszaad'), ylcat_season := "zomergranen, mais, graszaad"]
season.obic[landuse %in% c('wintertarwe'), ylcat_season := "wintergranen"]
season.obic[landuse %in% c('fabrieksaardappelen', 'pootaardappelen','aardappelen',
                           'bladgroenten', 'prei, spruiten, koolsoorten', 'witlof, selderij, uien'),
            ylcat_season := "geplante groenten"]


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

# Onvermijdelijke beginschade vanwege textuur categorie
season.obic[landuse %in% c('suikerbieten', 'schorseneren', 'was bospeen', 'erwten, bonen',
                           'tulpen, narcis, hyacint','fabrieksaardappelen', 'pootaardappelen',
                           'aardappelen', 'bladgroenten', 'prei, spruiten, koolsoorten', 'witlof, selderij, uien',
                           'klein fruit'), ylcat_texture := 'zaai, plant, pootgoed, klein fruit']
season.obic[landuse %in% c('zomergerst', 'snijmais', 'graszaad', 'wintertarwe'), ylcat_texture := 'granen']
season.obic[landuse %in% c('overige boomteelt', 'beweid bemaaid gras', 'loofbos', 'laanbomen_onderstammen', 'groot fruit', 'naaldbos'), ylcat_texture := 'gras, bos']