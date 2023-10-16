# Script to modify crops.obic

  # load package
  library(data.table)

  # Load crops.obic (copied csv from pandex)
  cr <- fread('DEV/data/crops_obic.csv')

  # update the Rdata file
  crops.obic <- copy(cr)
  save(crops.obic, file = 'data/crops_obic.RData')
  
  