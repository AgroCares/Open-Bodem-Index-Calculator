# make makkink table

# load csv file with makkink per crop per month
dt <- fread('dev/data/makkink.csv',header = TRUE)

# colnames for the 12 months
cols <- colnames(dt)[-1]

# replace all missing values with the value 0.36 for braak soil
dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0.36,x)),.SDcols = cols]

# write file to data
crops.makkink <- copy(dt)
save(crops.makkink, file = 'data/crops_makkink.RData')
