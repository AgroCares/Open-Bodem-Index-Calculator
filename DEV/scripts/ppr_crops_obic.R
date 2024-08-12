# Script to modify crops.obic

  # load package
  library(data.table)

  # Load crops.obic (copied csv from pandex)
  crops.obic <- fread('data-raw/crops_obic/crops_obic.csv')

  # update the Rdata file
  usethis::use_data(crops.obic,version = 3, overwrite = TRUE, compress = 'xz')
  devtools::document()
  