library(data.table)

column_description.obic <- fread("DEV/column_description.csv")
column_description.obic[column_description.obic == ''] <- NA
column_description.obic <- na.omit(column_description.obic, cols = "column")

save(column_description.obic, file = "dev/column_description_obic.RData", compress = "xz")
