# make crops.obic
# load packages
library(data.table)

# load data
dt <- fread('data-raw/crops_obic/b_lu_brp.csv') # most cultivation data
dtn <- fread('data-raw/crops_obic/d_n_norm.csv') # N-use norms
load(file = 'data-raw/crops_obic/crops.obic.legacy.rda')

# preprocessing =====
# ppr dt
# remove unrequired columns
dt[,c('B_REGION', 'B_LU', 'B_LU_HC','B_LU_BBWP', 'B_LU_ARABLE_ER', 'B_LU_PRODUCTIVE_ER', 'B_LU_CULTIVATED_ER', 'B_LU_ROOTBALL_DIAMETER', 'B_LU_PLANT_DENSITY') := NULL]

# setnames
setnames(dt, names(dt), c('crop_code', 'crop_name', "crop_waterstress", "crop_intensity",
                          "crop_eos", "crop_eos_residue", "crop_category", "crop_rotation",
                          "crop_crumbleability", "crop_phosphate", "crop_sealing",
                          "crop_n", "crop_k", "crop_measure", 'crop_name_scientific',
                          'crop_season', 'crop_makkink'))


# ppr dtn
# add simple soiltypes
dtn <- merge(dtn, soils.obic[,.(soiltype, soiltype.m)], by.x = 'B_SOILTYPE_AGR', by.y = 'soiltype')

# select northern and southern province with relevant columns
dtn <- unique(dtn[B_REGION_PV %in% c('PV20', 'PV30'),.(B_LU, B_REGION_PV, D_N_NORM, soiltype.m)]) # PV20 = Groningen, PV30 = Noord-Brabant

# format soiltypes
dtn[soiltype.m == 'klei', soiltype.m := 'nf_clay']
dtn[soiltype.m == 'loess', soiltype.m := 'nf_loess']
dtn[soiltype.m == 'veen', soiltype.m := 'nf_peat']
dtn[soiltype.m == 'zand' & B_REGION_PV == 'PV20', soiltype.m := 'nf_sand.other']
dtn[soiltype.m == 'zand' & B_REGION_PV == 'PV30', soiltype.m := 'nf_sand.south']

# remove region and duplicated rows
dtn <- unique(dtn[,.(B_LU, D_N_NORM, soiltype.m)])

# reformat crop code
dtn <- dtn[,crop_code := as.numeric(gsub('^nl_', '',B_LU))]
dtn[,B_LU := NULL]

# reshape dtn
dtn <- dcast(dtn, crop_code~soiltype.m, value.var = 'D_N_NORM')

# set col order
setcolorder(dtn, c('crop_code', 'nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat'))

# merge data
crops.obic <- merge(dt, dtn, by = 'crop_code', all.x = TRUE)

# set column order
setcolorder(crops.obic, names(crops.obic.legacy))

# save data
save(crops.obic, file = 'data/crops_obic.RData')
fwrite(crops.obic, 'data-raw/crops_obic/crops_obic.csv')
