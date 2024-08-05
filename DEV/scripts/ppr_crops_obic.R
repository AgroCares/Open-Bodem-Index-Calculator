# Script to modify crops.obic

  # load package
  library(data.table)

  # Load crops.obic (copied csv from pandex)
  cr <- fread('DEV/data/crops_obic.csv')

  # update the Rdata file
  crops.obic <- copy(cr[, 2:ncol(cr)])
  usethis::use_data(crops.obic,version = 3, overwrite = TRUE, compress = 'xz')
  devtools::document()
  