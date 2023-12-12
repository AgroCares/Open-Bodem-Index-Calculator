# ppr maatregel groupings in relation to ecosystem services

  # require package
  require(data.table)
  
  # read in csv file
  d1 <- fread('dev/data/maatregel_groep.csv')
  
  # measure.group
  management.obic <- copy(d1)
  
  # save file as RData-object
  usethis::use_data(management.obic, version = 3, overwrite = TRUE, compress = 'xz')
  