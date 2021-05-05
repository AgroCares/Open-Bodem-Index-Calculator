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

# Add groenbemester columns
# Braakland
nema.crop.rot.obic <- nema.crop.rot.obic[,groen_br := fifelse(grepl('_br$', crop),TRUE,FALSE)]
# vroege stoppel
nema.crop.rot.obic <- nema.crop.rot.obic[,groen_vs := fifelse(grepl('_vs$', crop),TRUE,FALSE)]
# Onder dekvrucht
nema.crop.rot.obic <- nema.crop.rot.obic[,groen_od := fifelse(grepl('_od$', crop),TRUE,FALSE)]
# late stoppel
nema.crop.rot.obic <- nema.crop.rot.obic[,groen_ls := fifelse(grepl('_ls$', crop),TRUE,FALSE)]
# groenbemester als stuifdek
nema.crop.rot.obic <- nema.crop.rot.obic[,groen_st := fifelse(grepl('_st$', crop),TRUE,FALSE)]


# Add scientific crop names for merging with crops.obic
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('aardappel', crop), crop_name_scientific := 'solanum tuberosum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('aardbei', crop), crop_name_scientific := 'fragaria x ananassa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('asperge', crop), crop_name_scientific := 'asparagus officinalis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('aubergine', crop), crop_name_scientific := 'solanum melongena']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('basilicum', crop), crop_name_scientific := 'ocimum basilicum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('bladkool_koolzaad', crop), crop_name_scientific := 'brassica oleracea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('bladrammenas', crop), crop_name_scientific := 'raphanus sativus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('boekweit', crop), crop_name_scientific := 'fagopyrum esculentum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('chichorei', crop), crop_name_scientific := 'chichorium intybus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('dille', crop), crop_name_scientific := 'anethum graveolens']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('engels_raaigras', crop), crop_name_scientific := 'lolium perenne']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('erwt', crop), crop_name_scientific := 'pisum sativum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('facelia', crop), crop_name_scientific := 'phacelia tanacetifolia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('gele_mosterd', crop), crop_name_scientific := 'sinapsis alba']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('gerst', crop), crop_name_scientific := 'horderum vulgare']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('gingellikruid', crop), crop_name_scientific := 'guizotia abyssinica']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('haver', crop), crop_name_scientific := 'avena sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('hennep', crop), crop_name_scientific := 'cannabis sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('huttentut_of_deder_of_vlasdodder', crop), crop_name_scientific := 'camelina sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('inkarnaatklaver', crop), crop_name_scientific := 'trifolium incarnatum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('italiaans_raaigras', crop), crop_name_scientific := 'lolium multiflorum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('japanse_haver', crop), crop_name_scientific := 'avena strigosa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('rode_klaver', crop), crop_name_scientific := 'trifolium pratense']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('perzische_klaver', crop), crop_name_scientific := 'trifolium resupinatum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('witte_klaver', crop), crop_name_scientific := 'trifolium repens']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('alexandrijnse_klaver', crop), crop_name_scientific := 'trifolium alexandrinum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('knoflook', crop), crop_name_scientific := 'allium sativum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('knolraap|bladkool|chinese_kool', crop), crop_name_scientific := 'brassica rapa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('komkommer', crop), crop_name_scientific := 'cucumis sativus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('koolzaad|bloemkool|broccoli|boerenkool|koolrabi|sluitkool|spruitkool', crop), crop_name_scientific := 'brassica oleracea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('koriander', crop), crop_name_scientific := 'coriandrum sativum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('lupine', crop), crop_name_scientific := 'lupinus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('luzerne', crop), crop_name_scientific := 'medicago sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('mais', crop), crop_name_scientific := 'zea mays']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('meloen|pompoen', crop), crop_name_scientific := 'cucurbita']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('paprika', crop), crop_name_scientific := 'capsicum annuum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('pastinaak', crop), crop_name_scientific := 'pastinaca sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('peen', crop), crop_name_scientific := 'daucus carota']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('peterselie', crop), crop_name_scientific := 'petroselinum crispum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('prei', crop), crop_name_scientific := 'allium ampeloprasum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('abies', crop), crop_name_scientific := 'abies']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('acer|esdoorn', crop), crop_name_scientific := 'acer']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('aconitum', crop), crop_name_scientific := 'aconitum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('allium', crop), crop_name_scientific := 'allium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('amelanchier', crop), crop_name_scientific := 'amelanchier']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('andijvie', crop), crop_name_scientific := 'chicorium endivia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('anemone', crop), crop_name_scientific := 'anemone']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('aster', crop), crop_name_scientific := 'aster']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('astilbe', crop), crop_name_scientific := 'astible']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('betula', crop), crop_name_scientific := 'betula']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('blauwmaanzaad', crop), crop_name_scientific := 'papaver somniferum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('buxus', crop), crop_name_scientific := 'buxus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('camassia', crop), crop_name_scientific := 'camassia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('campanula', crop), crop_name_scientific := 'campanula']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('carpinus', crop), crop_name_scientific := 'carpinus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('chamaecyparis', crop), crop_name_scientific := 'chamaecyparis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('chionodoxa', crop), crop_name_scientific := 'chionodoxa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('cichorei', crop), crop_name_scientific := 'chichorium intybus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('colchicum', crop), crop_name_scientific := 'colchicum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('convallaria', crop), crop_name_scientific := 'convallaria']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('cornus', crop), crop_name_scientific := 'cornus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('courgette', crop), crop_name_scientific := 'cucurbita pepo']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('crataegus', crop), crop_name_scientific := 'crataegus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('crocosmia', crop), crop_name_scientific := 'crocosmia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('dahlia', crop), crop_name_scientific := 'dahlia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('delphinium', crop), crop_name_scientific := 'delphinium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('eremurus', crop), crop_name_scientific := 'eremurus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('erythronium', crop), crop_name_scientific := 'erythronium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('ethiopische_mosterd', crop), crop_name_scientific := 'brassica carinata']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('euonymus', crop), crop_name_scientific := ' euonymus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('fagus', crop), crop_name_scientific := 'fagus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('fraxinus', crop), crop_name_scientific := 'fraxinus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('freesia', crop), crop_name_scientific := 'freesia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('fritillaria', crop), crop_name_scientific := 'fritillaria']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('galanthus', crop), crop_name_scientific := 'galanthus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('galtonia', crop), crop_name_scientific := 'galtonia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('geranium', crop), crop_name_scientific := 'geranium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('gierst_parel', crop), crop_name_scientific := 'pennisetum glaucum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('quinoa', crop), crop_name_scientific := 'chenopodium quinoa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('gladiool', crop), crop_name_scientific := 'gladiool']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('hemerocallis', crop), crop_name_scientific := 'hemerocallis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('hosta', crop), crop_name_scientific := 'hosta']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('hyacint', crop), crop_name_scientific := 'hyacinthus orientalis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('hymenocallis', crop), crop_name_scientific := 'hymenocallis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('iris', crop), crop_name_scientific := 'iris']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('karwij', crop), crop_name_scientific := 'carum carvi']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('krokus', crop), crop_name_scientific := 'crocus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('lelie', crop), crop_name_scientific := 'lilium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('leucojum', crop), crop_name_scientific := 'leucojum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('liatris', crop), crop_name_scientific := 'liatris']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('ligustrum', crop), crop_name_scientific := 'ligustrum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('maggi', crop), crop_name_scientific := 'levisticum officinale']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('mahonia', crop), crop_name_scientific := 'mahonia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('malus', crop), crop_name_scientific := 'malus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('meekrap', crop), crop_name_scientific := 'rubia tinctorum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('muscari', crop), crop_name_scientific := 'muscari']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('narcis', crop), crop_name_scientific := 'narcis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('ornithogalum', crop), crop_name_scientific := 'ornithogalum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('oxalis', crop), crop_name_scientific := 'oxalis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('paeonia', crop), crop_name_scientific := 'paeonia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('papaver', crop), crop_name_scientific := 'papaver']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('phlox', crop), crop_name_scientific := 'phlox']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('picea', crop), crop_name_scientific := 'picea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('pinus', crop), crop_name_scientific := 'pinus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('potentilla', crop), crop_name_scientific := 'potentilla']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('prunus', crop), crop_name_scientific := 'prunus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('pulmonaria', crop), crop_name_scientific := 'pulmonaria']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('puschkinia', crop), crop_name_scientific := 'puschkinia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('quercus', crop), crop_name_scientific := 'quercus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('rabarber', crop), crop_name_scientific := 'rheum rhabarbarum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('raketblad', crop), crop_name_scientific := 'solanum sisymbriifolium']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('ranunculus', crop), crop_name_scientific := 'ranunculus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('robinia', crop), crop_name_scientific := 'robinia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('rode_biet|suikerbiet', crop), crop_name_scientific := 'beta vulgaris']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('rogge', crop), crop_name_scientific := 'secale cereale']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('rosa', crop), crop_name_scientific := 'rosa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('salvia', crop), crop_name_scientific := 'salvia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('sarepta_mosterd', crop), crop_name_scientific := 'brassica juncea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('schorseneer', crop), crop_name_scientific := 'scorzonera hispanica']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('scilla', crop), crop_name_scientific := 'scilla']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('selderij', crop), crop_name_scientific := 'apium graveolens']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('^sla', crop), crop_name_scientific := 'lactuca sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('soedangras', crop), crop_name_scientific := 'sorghum bicolor']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('spelt', crop), crop_name_scientific := 'triticum spelta']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('spinazie', crop), crop_name_scientific := 'espinaca oleracea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('spiraea', crop), crop_name_scientific := 'spiraea']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('sprekelia', crop), crop_name_scientific := 'sprekelia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('spurrie', crop), crop_name_scientific := 'spergula']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('stamslaboon', crop), crop_name_scientific := 'phaseolus vulgaris']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('stenbergia', crop), crop_name_scientific := 'stenbergia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('syringa', crop), crop_name_scientific := 'syringa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('tagetes', crop), crop_name_scientific := 'tagetes']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('taxus', crop), crop_name_scientific := 'taxus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('teff', crop), crop_name_scientific := 'eragrostis tef']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('thuja', crop), crop_name_scientific := 'thuja']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('tigridia', crop), crop_name_scientific := 'tigridia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('tilia', crop), crop_name_scientific := 'tilia']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('triticale', crop), crop_name_scientific := 'triticale']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('tulp', crop), crop_name_scientific := 'tulipa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('^ui$', crop), crop_name_scientific := 'allium cepa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('valeriaan', crop), crop_name_scientific := 'valeriana officinalis']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('tuinboon', crop), crop_name_scientific := 'vicia faba']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('venkel', crop), crop_name_scientific := 'foeniculum vulgare']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('vlas', crop), crop_name_scientific := 'linum usitatissimum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('wikke', crop), crop_name_scientific := 'vicia sativa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('westerwolds', crop), crop_name_scientific := 'lolium multiflorum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('wintertarwe', crop), crop_name_scientific := 'triticum aestivum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('witlof', crop), crop_name_scientific := 'chichorium intybus']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('zantedeschia', crop), crop_name_scientific := 'zantedeschia aethiopica']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('zilverspar', crop), crop_name_scientific := 'abies']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('zomertarwe', crop), crop_name_scientific := 'triticum aestivum']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('zonnebloem', crop), crop_name_scientific := 'tulipa']
nema.crop.rot.obic <- nema.crop.rot.obic[grepl('zwaardherik', crop), crop_name_scientific := 'eruca vesicaria']

# unique(nema.crop.rot.obic[grepl('chic', crop),.(crop, crop_name_scientific)])
# 
# nema.crop.rot.obic <- nema.crop.rot.obic[grepl('', crop), crop_name_scientific := '']
# 'solanum tuberosum' %in% crops.obic$crop_name_scientific
# 
# crops.obic[grepl('zant',crop_name),.(crop_name, crop_name_scientific)]
# 
# unique(nema.crop.rot.obic[is.na(crop_name_scientific),.(crop_name_scientific,crop)])

# Format scientific species names to be aligned with nema.obic
nema.crop.rot.obic <- nema.crop.rot.obic[,name_scientific := gsub('spp$', 'spp\\.',name_scientific)]
# nema.crop.rot.obic <- nema.crop.rot.obic[name_scientific == 'Globodera rostochiensis of pallida',name_scientific := 'Cysteaaltjes']
# Heterodera are cystnematodes and may require extra input information for nema.obic
# Longidorus elongatus (as well as Xiphinema) are very large and do not appear in a typical analysis
# aaltjesschema has Rotylenchus uniformis obic only has Rotylenchus spp. similair with Xiphinema diversicaudatum and Xiphinema spp.
# 

# Write to OBIC
write.csv(nema.crop.rot.obic, 'dev/aaltjes_gewas_schema.csv')
save(nema.crop.rot.obic, file = 'data/nema_crop_rot_obic.RData')


