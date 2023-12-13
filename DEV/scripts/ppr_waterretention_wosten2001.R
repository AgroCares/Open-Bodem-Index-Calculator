library(data.table)

# load  csv
bouwsteen_tb <- fread("DEV/data/Wosten2001_Tabel.csv")

usethis::use_data(bouwsteen_tb, version = 3, compress = 'xz', overwrite = TRUE)
