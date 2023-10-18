library(data.table)

tbl.ph.delta <- fread("DEV/data/tbl_ph_delta.csv")
tbl.ph.delta$table <- as.character(tbl.ph.delta$table)
usethis::use_data(tbl.ph.delta, version = 3, overwrite = TRUE, compress = 'xz')
