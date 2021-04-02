#' Script to make season.obic table
#' 
#' This table is based on Tabel 2 in Huinink (2018) and links to crops.obic$crop_season
#' Column derving is based on equations a, b, c, d, a1, a2 in Huinink (2018)
#' 
#' @references  Huinink (2018) Bodem/perceel geschiktheidsbeoordeling voor Landbouw, Bosbouw en Recreatie. BodemConsult-Arnhem
#' 
#' @import data.table
# 
# Table is based on Tabel 2 in Huinink (2018) and links to crops.obic$crop_season
# Not all landuses in the original table appear in brp and some of the brp crops have been assigned to "overig" and could be reclassified later.
season.obic <- data.table(landuse = c('naaldbos', 'loofbos', 'groot fruit', 'klein fruit',
                                      'aardappelen', 'pootaardappelen', 'fabrieksaardappelen',
                                      'suikerbieten', 'wintertarwe', 'zomergerst', 'snijmais',
                                      'laanbomen_onderstammen', 'overige boomteelt', 'graszaad', 'asperge',
                                      'schorseneren', 'prei, spruiten, koolsoorten', 'erwten, bonen',
                                      'witlof, selderij, uien', 'was bospeen', 'tulpen, narcis, hyacint',
                                      'dahlia', 'bladgroenten', 'beweid bemaaid gras',
                                      'plantsoenen', 'gazons', 'kampeerterreinen, ligweiden', 'sportvelden',
                                      'overig'),
                          req_days_pre_glg = c(170, 170, 170, 170,
                                               155, 150, 155,
                                               155, 165, 165, 130,
                                               170, 170, 170, 170,
                                               170, 100, 135,
                                               75,  100, 180,
                                               100, 100, 180,
                                               180, 100, 100, 100,
                                               1),
                          req_days_post_glg =c(75, 75, 75, 75,
                                               35,  0, 65, 
                                               75, 10,  0, 35,
                                               75, 55, 15, 75,
                                               75,125,-15,
                                               65, 25,140,
                                              140, 75, 75,
                                               75, 75, 45, 75,
                                               1),
                          total_days = c(245, 245, 245, 245,
                                         190, 150, 220,
                                         230, 175, 170, 165,
                                         245, 225, 185, 245,
                                         245, 225, 130,
                                         140, 125, 320,
                                         320, 175, 225,
                                         225, 210, 145, 255,
                                         1))

# Modify season.obic to add more info on yield loss

# Eenjarige tuin en akkerbouw gewassen
season.obic[landuse %in% c('suikerbieten', 'schorseneren', 'was bospeen', 'erwten, bonen', 'tulpen, narcis, hyacint'), derving := "zaai groenten"]
season.obic[landuse %in% c('zomergerst', 'snijmais', 'graszaad'), derving := "zomergranen"]
season.obic[landuse %in% c('fabrieksaardappelen', 'pootaardappelen','aardappelen',
                           'bladgroenten', 'prei, spruiten, koolsoorten', 'witlof, selderij, uien'),
            derving := "plant groenten"]
# meerjarige akkerbouw en boomteelt gewassen
season.obic[landuse %in% c('naaldbos', 'loofbos', 'groot fruit', 'klein fruit', 'dahlia',
                           'asperge', 'overige boomteelt', 'laanbomen_onderstammen'), derving := 'boomteelt']
season.obic[landuse %in% c('wintertarwe'), derving := "wintergranen"]

# Melkveegrasland
season.obic[landuse == 'beweid bemaaid gras', derving := 'grasland']

# Overig
season.obic[landuse %in% c('plantsoenen', 'gazons', 'kampeerterreinen, ligweiden', 'sportvelden', 'overig'), derving := 'overig']

# Save table
write.csv(season.obic, 'data/season_obic.csv')
save(season.obic,file = 'data/season.obic.RData')
