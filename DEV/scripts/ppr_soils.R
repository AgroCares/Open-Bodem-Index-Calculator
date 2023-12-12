library(data.table)

soils.obic <- fread("DEV/data/bodem.csv")

# add soil type for measure selection
soils.obic[,soiltype.m := soiltype.n]
soils.obic[soiltype == 'loess', soiltype.m := 'loess'] 

usethis::use_data(soils.obic, version = 3, overwrite = TRUE, compress = 'xz')