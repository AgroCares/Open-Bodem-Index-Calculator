# Make nema.obic table

  # packages
  library(data.table); library(OBIC)

  # load initial data from csv file
  obic.nema <- fread('dev/nematodes.csv', sep = ';')

  # remove column standaard
  obic.nema[, standaard := NULL]

  # set parameters logistic evaluation function (selected by manual testing)
  # In principle yellow value corresponds with a score of 0.5. 
  # If yellow is much smaller than red/2, yellow should correspond with ~ 0.8 and red with ~ 0.2
  obic.nema[geel==150 & rood==300, c('b','v') := list(0.0153,0.43)]
  obic.nema[geel==200 & rood==500, c('b','v') := list(0.00765,0.43)]
  obic.nema[geel==500 & rood==1000, c('b','v') := list(0.004591,0.43)]
  obic.nema[geel==50 & rood==200, c('b','v') := list(0.0153,0.43)]
  obic.nema[geel==100 & rood==300, c('b','v') := list(0.011477,0.43)]
  obic.nema[geel==50 & rood==500, c('b','v') := list(0.0051,0.43)]
  obic.nema[geel==5 & rood==5, c('b','v') := list(1.75,2.75)]
  obic.nema[geel==30 & rood==100, c('b','v') := list(0.03276,0.43)]
  obic.nema[geel==1 & rood==10, c('b','v') := list(0.25505,0.43)]
  obic.nema[geel==100 & rood==500, c('b','v') := list(0.0057385,0.43)]
  obic.nema[geel==1 & rood==5, c('b','v') := list(0.57388,0.43)]
  obic.nema[geel==50 & rood==100, c('b','v') := list(0.0459,0.43)]
  obic.nema[geel==5 & rood==10, c('b','v') := list(0.574,0.43)]
  obic.nema[geel==10 & rood==50, c('b','v') := list(0.0573,0.43)]
 

  # Add standaard column. A column that can be used to determine which species should always be used to calculate an average indicator score.
  # obic.nema[species %in% c('Ditylenchus spp.', 'Ditylenchus dipsaci', 'Xiphinema spp.',
  #                        'Longidorus spp.', 'Trichodorus similis', 'Trichodorus primitivus',
  #                        'Paratrichodorus teres', 'Rotylenchus spp.', 'Paratylenchus spp.',
  #                        'Meloidogyne chitwoodi/fallax', 'Meloidogyne chitwoodi', 
  #                        'Meloidogyne fallax', 'Meloidogyne minor', 'Meloidogyne naasi',
  #                        'Meloidogyne hapla', 'Cysteaaltjes', 'Pratylenchus penetrans',
  #                        'Pratylenchus crenatus', 'Helicotylenchus spp.', 'Pratylenchus neglectus'), standaard := TRUE]
  # obic.nema[is.na(standaard),standaard := FALSE]
  
  # add ea nematodes
  ean <- fread('DEV/ea_aaltjes.csv', header = TRUE) |> setnames('x', 'element')
  
  # only keep element column
  ean <- data.table(element = ean$element)
  
  # select new species
  ean <- ean[!element %in% obic.nema$element]
  
  # add to obic.nema
  obic.nema <- rbindlist(list(obic.nema, ean), use.names = TRUE, fill = TRUE)
  
  # read and add parameters
  par <- fread('DEV/parameters.csv', header = TRUE)
  par <- par[,.(Code, Parameter)]
  
  # merge par and nema
  obic.nema <- merge(obic.nema, par, by.x = 'element', by.y = 'Code', all.x = TRUE)
  
  # add species names to new rows
  obic.nema <- obic.nema[is.na(species), species := gsub('^.*tode, ', '', Parameter)]
  obic.nema <- obic.nema[,species := gsub(',.*$', '', species)]
  obic.nema <- obic.nema[,species := gsub("(^)([[:alpha:]])", "\\1\\U\\2", species, perl = TRUE)]
  
  # remove parameter
  obic.nema <- obic.nema[,Parameter := NULL]
  
  # save files
  write.csv(obic.nema, 'dev/obic.nema.csv')
  nema.obic <- obic.nema
  save(nema.obic, file= 'data/nema_obic.RData')
