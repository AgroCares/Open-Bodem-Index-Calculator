# Script for loading weather data to weather.RData

# Load csv with weather data
dt <- fread('DEV/data/weather.csv')

# write file to data
weather.obic <- copy(dt)
save(weather.obic, file = 'data/weather_obic.RData')
