# make weights.obic table
# this table evaluates for each indicator whether its applicable yes or no
# applicability is currently differentiated for land use categories and soil type (peat and non-peat)
# in future this one can be used to differentiate between countries too
# weights are either 1 (applicable) or -1 (not applicable)

  # load library
  library(data.table)

  # load csv file
  weight.obic <- fread("dev/data/weight_obic.csv")

  # load the file with all indicators
  obic.ind <- as.data.table(readRDS('dev/data/column_description_obic.rds'))
  
  # which indicators should be present
  colsd <- obic.ind[type=="indicator",column]
  
  # which are present in weights
  cols <- unique(weight.obic$indicator)
  
  # which are missing
  cols.mis <- colsd[!colsd %in% cols]
  
  # checks
  if(length(cols.mis)==0){
    print('all indicators are also in column description')
  } else {
      print(paste0('indicators are missing: ',paste0(cols.mis,collapse=', ')))
  }

save(weight.obic, file = "data/weight_obic.RData")
