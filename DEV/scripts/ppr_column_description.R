# this R script makes a data.table with all inputs and outputs of the OBIC

  # require library
  library(data.table)

  # load description csv file
  column_description.obic <- fread("dev/data/column_description.csv")

  # a few adaptations to ensure correct format
  column_description.obic[column_description.obic == ''] <- NA
  column_description.obic <- na.omit(column_description.obic, cols = "column")

  # save as Rdata file for the package
  save(column_description.obic, file = "data/column_description_obic.RData", compress = "xz")

  # save as temporary file in dev/data
  saveRDS(column_description.obic,file='dev/data/column_description_obic.rds')
