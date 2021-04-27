# make example dataset Binnenveld for use in the package

  # require package
  require(data.table)

  # read in the csv file with soil properties
  binnenveld <- fread('dev/data/binnenveld.csv')
  
  # save file
  save(binnenveld, file = 'data/binnenveld.RData')