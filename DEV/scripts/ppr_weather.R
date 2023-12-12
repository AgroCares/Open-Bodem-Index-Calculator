# Script for loading weather data to weather.RData

# Load csv with weather data
dt <- fread('DEV/data/weather.csv')

# write file to data
weather.obic <- copy(dt)
usethis::use_data(weather.obic, version = 3, overwrite = TRUE, compress = 'xz')
