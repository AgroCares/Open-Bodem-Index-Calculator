
# make example dataset Binnenveld for use in the package
# the csv dataset is prepared in OBIC-helpers

  # require package
  require(data.table)

  # read in the csv file with soil properties
  binnenveld <- fread('dev/data/binnenveld.csv')
  binnenveld[,B_FERT_NORM_FR := 1]

  # save file
  usethis::use_data(binnenveld, version = 3, overwrite = TRUE, compress = 'xz')
  