# Script to process aaltjeschema tables copied into excel from pdf into proper data.table
library(data.table)
library(openxlsx)

# if(file.exists('dev/bouwplan/raw/nema_propagation.rds')) {
#   propa <- readRDS('dev/bouwplan/raw/nema_propagation.rds')
# } else {
#   # Load data
#   dalg.v <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema.xlsx', sheet = 'dalgrond_verm'))
#   zand.v <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema.xlsx', sheet = 'zand_verm'))
#   klei.v <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema.xlsx', sheet = 'klei_verm'))
#   loes.v <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema.xlsx', sheet = 'loess_verm'))
#   zavl.v <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema.xlsx', sheet = 'zavel_verm'))
#   
#   # All vermeerdering (propagation) is the same amongst soil types
#   all(identical(dalg.v, zand.v),identical(dalg.v, klei.v),identical(dalg.v, loes.v),identical(dalg.v, zavl.v))
#   
#   # make one table for propagation
#   propa <- dalg.v
#   rm(dalg.v, zand.v, klei.v, loes.v, zavl.v)
#   
#   # Write propagation table
#   saveRDS(propa, 'dev/bouwplan/raw/nema_propagation.rds')
# }

propa <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema_update.xlsx', sheet = 'vermeerdering'))
propa.wortellesie <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema_update.xlsx', sheet = 'vermeerdering_wortellesie'))
propa[,crop2 := NULL]
propa.wortellesie[,crop2 := NULL]

# Merge wortellesie with the rest
propa <- merge.data.table(propa, propa.wortellesie, by = 'crop')

# Process table
propa <- propa[,crop := tolower(crop)]
propa <- propa[,crop := gsub('-/|/| ','_', crop)]

# Melt data table to operate on nematode names
propa.m <- melt.data.table(propa, id.vars = 'crop', variable.name = 'nema_name', value.name = 'propagation')

# Add columns for cultivar and serotype
propa.m <- propa.m[,cultivar_dependent:= fifelse(grepl('R',propagation), TRUE, FALSE)]
propa.m <- propa.m[,serotype_dependent:= fifelse(grepl('S',propagation), TRUE, FALSE)]

# Add column with quality of information
propa.m <- propa.m[,info := 'yes']
propa.m <- propa.m[grepl('i', propagation),info := 'some']
propa.m <- propa.m[grepl('^\\?$', propagation),info := 'none']
# Where 2 = hard evidence from expierments, 1 is less substantial information, 0 is no information

# change propagation to readable data
propa.m[,propagation := gsub('R$|S$|i$|Ri$|Si$', '', propagation)]
propa.m[propagation == 'lll', propagation := 'sterk']
propa.m[propagation == 'll', propagation := 'matig']
propa.m[propagation == 'l', propagation := 'weinig']
propa.m[propagation == '-', propagation := 'natuurlijke_afname']
propa.m[propagation == '--', propagation := 'actieve_afname']
propa.m[, propagation := gsub('^\\?$','onbekend',propagation)]

# Include for which soiltypes the information is valid
st <-  as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema_update.xlsx', sheet = 'info', cols = 1:2))
propa.m <- merge.data.table(propa.m, st, by.x = 'nema_name', by.y = 'nema_soort')
propa.m[,dalgrond := FALSE]
propa.m[,klei := FALSE]
propa.m[,loess := FALSE]
propa.m[,zand := FALSE]
propa.m[,zavel := FALSE]

propa.m[grepl('D',grondsoort),dalgrond := TRUE]
propa.m[grepl('K',grondsoort),klei := TRUE]
propa.m[grepl('L',grondsoort),loess := TRUE]
propa.m[grepl('^Z |^Z$',grondsoort),zand := TRUE]
propa.m[grepl('ZV',grondsoort),zavel := TRUE]

# seperate common from scientifc nematode name
# format scientific name
propa.m <- propa.m[,name_scientific := nema_name]
propa.m <- propa.m[grepl(',',nema_name),name_scientific := gsub(',\\.?\\w*$|,\\.?\\w*\\.?\\waaltje$|,\\w*-\\w*virus','',nema_name)]
propa.m <- propa.m[, name_scientific := gsub('_\\._', '_', name_scientific)]

# format common name
propa.m <- propa.m[,name_common := nema_name]
# propa.m <- propa.m[grepl('Geel\\.bieten,\\.cysteaaltje',name_common),name_common := 'Geel_bietencysteaaltje']
propa.m <- propa.m[,name_common := gsub('^.*,','',name_common)]

# Save temporary propagation object
saveRDS(propa.m,'dev/bouwplan/raw/nema_propagation_long_update.rds')

# Load process and add damage table
scd <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema_update.xlsx', sheet = 'schade'))
# Merge wortellessie schade
scd.w <- as.data.table(read.xlsx('../OBIC functies bouwplanindex/aaltjes schema/aaltjes_schema_update.xlsx', sheet = 'schade_wortellesie'))
scd <- merge.data.table(scd, scd.w, by = 'crop')

scd[,crop2.y := NULL]
scd[,crop2.x := NULL]
scd <- scd[,crop := tolower(crop)]
scd <- scd[,crop := gsub('-/|/| ','_', crop)]

# Melt scd
scd.m <- melt.data.table(scd, id.vars = 'crop', variable.name = 'nema_name', value.name = 'damage')

# Convert damage to human readable classes
scd.m <- scd.m[, damage := gsub('^-1$', 'onbekend',damage)]
scd.m <- scd.m[, damage := gsub('^0$', 'geen',damage)]
scd.m <- scd.m[, damage := gsub('^1$', 'weinig',damage)]
scd.m <- scd.m[, damage := gsub('^2$', 'matig',damage)]
scd.m <- scd.m[, damage := gsub('^3$', 'zwaar',damage)]
# Here weining == 0-15% yield loss, matig = 16-35%, zwaar is 36-100%
saveRDS(scd.m, 'dev/bouwplan/raw/nema_damage_long_update.rds')

# Merge two tables
nema.crop.rot.obic <- merge.data.table(propa.m, scd.m, by = c('nema_name','crop'))
setorder(nema.crop.rot.obic, crop, name_scientific)
setcolorder(nema.crop.rot.obic, c('crop', 'name_scientific', 'propagation', 'damage', 'cultivar_dependent', 'serotype_dependent',
                                  'dalgrond', 'klei', 'loess', 'zand', 'zavel', 'info', 'name_common', 'nema_name', 'grondsoort'))

# Write to OBIC
write.csv(nema.crop.rot.obic, 'dev/aaltjes_gewas_schema.csv')
save(nema.crop.rot.obic, file = 'data/nema_crop_rot_obic.RData')
