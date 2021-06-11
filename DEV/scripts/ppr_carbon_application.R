# make a carbon application table

# load csv file with carbon application moments per crop type
dt <- fread('dev/data/carbon_application.csv',header = TRUE)

# write file to data
carbon_application <- copy(dt)
save(carbon_application, file = 'data/carbon_application.RData')