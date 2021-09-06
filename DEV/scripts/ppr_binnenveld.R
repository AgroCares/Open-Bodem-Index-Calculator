
# make example dataset Binnenveld for use in the package
# the csv dataset is prepared in OBIC-helpers

  # require package
  require(data.table)

  # read in the csv file with soil properties
  binnenveld <- fread('dev/data/binnenveld.csv')
  
  # remove ref_id
  binnenveld[,REF_ID := NULL]
  
  # save file
  save(binnenveld, file = 'data/binnenveld.RData')
  