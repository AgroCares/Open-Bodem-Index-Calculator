# Script to modify crops.obic

  # load package
  library(data.table)

  # Load crops.obic (copied csv from pandex)
  crops.obic <- pandex::crops.obic

  # update the Rdata file
  usethis::use_data(crops.obic,version = 3, overwrite = TRUE, compress = 'xz')
  devtools::document()
  fwrite(crops.obic, 'data-raw/crops_obic/crops_obic.csv')
  