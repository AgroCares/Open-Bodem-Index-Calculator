# make makkink table

  # load csv file with makkink per crop per month
  d1 <- fread('dev/data/makkink.csv')
  
  # colnames for the 12 months
  cols <- colnames(d1)[-1]
  
  # replace all missing values with the value 0.36 for braak soil
  d1[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0.23,x)),.SDcols = cols]
  
  # write file to data
  crops.makkink <- copy(d1)
  save(crops.makkink, file = 'data/crops_makkink.RData')
    