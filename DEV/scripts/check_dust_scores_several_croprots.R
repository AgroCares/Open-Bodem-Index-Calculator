# Illustrate I_P_DU when there is one year with maize and the rest is permanent grassland

library(data.table)
library(OBIC)

# load data
odt <- binnenveld[ID == 4] # Field four has the lowest clay content of all binnenveld fields
cr <- crops.obic[,.(crop_code, crop_category, crop_name)]

# remove bodemconditiscore columns
cols <- names(odt)[!grepl('BCS', names(odt))]
odt <- odt[,..cols]

# merge
odt <- merge(odt, cr[,.(crop_code, crop_name)], by.x = 'B_LU_BRP', by.y = 'crop_code')

# parcel with permanent grass
grass.dt <- copy(odt)
grass.dt <- grass.dt[,B_LU_BRP := 265]
grass.dt <- grass.dt[,treatment := 'gras']

grass.s <- obic_field_dt(grass.dt)
grass.s <- grass.s[,treatment := 'gras']

# parcel with maize
mais.dt <- copy(odt)
mais.dt <- mais.dt[,B_LU_BRP := 259]
mais.dt <- mais.dt[,treatment := 'mais']

mais.s <- obic_field_dt(mais.dt)
mais.s <- mais.s[,treatment := 'mais']

# parcel with one maize year
mix.dt <- copy(odt)
mix.dt <- mix.dt[,B_LU_BRP := 265]
mix.dt <- mix.dt[1,B_LU_BRP := 259]
mix.dt <- mix.dt[,treatment := 'onemais']

mix.s <- obic_field_dt(mix.dt)
mix.s <- mix.s[,treatment := 'one_mais_year']

# combine
dt <- rbindlist(list(grass.dt, mais.dt, mix.dt))
s.dt <- rbindlist(list(grass.s, mais.s, mix.s))

s.dt[,.(treatment, I_P_DU, S_T_OBI_A, S_P_OBI_A,S_C_OBI_A, I_C_K, I_C_MG, I_C_N)]

pcols <- grep('I_P', names(s.dt))
s.dt[,..pcols]
