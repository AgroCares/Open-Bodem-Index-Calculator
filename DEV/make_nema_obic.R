# Make nema.obic table
# packages
library(data.table)
library(OBIC)

# data
obic.nema <- read.csv('dev/nematodes.csv', sep = ';')
setDT(obic.nema)
obic.nema[, standaard := NULL]

# Determine values for evaluate logistic functions per Unique Combination of nematode number treshold ====
uc <- unique(obic.nema[,.(geel, rood)]) # 14 unique combination, so 14 different evaluation curves need to be determined
# In principle yellow value corresponds with a score of 0.5. If yellow is much smaller than red/2, yellow should correspond with ~ 0.8 and red with ~0.2
# Change b to alter score for rood, change v to alter score for geel
#150 300
plot(
  seq(0,uc[1,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[1,rood]*1.5, by = 0.5), b = 0.0153, x0 = uc[1,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0,          0.0153, uc[1,geel]*1, 0.43, FALSE) # should be ~ 1
evaluate_logistic(uc[1,geel], 0.0153, uc[1,geel]*1, 0.43, FALSE) # should be ~ 0.8
evaluate_logistic(uc[1,rood], 0.0153, uc[1,geel]*1, 0.43, FALSE) # should be ~ 0.2
evaluate_logistic(uc[1,rood]*1.25, 0.0153, uc[1,geel]*1, 0.43, FALSE) # should be ~ 0
uc[1, b:= 0.0153]
uc[1, v:= 0.43]
# 200 500
plot(
  seq(0,uc[2,rood]*1.25, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[2,rood]*1.25, by = 0.5), b = 0.00765, x0 = uc[2,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0,          0.00765, uc[2,geel]*1, 0.43, FALSE) # should be ~ 1
evaluate_logistic(uc[2,geel], 0.00765, uc[2,geel]*1, 0.43, FALSE) # should be ~ 0.8
evaluate_logistic(uc[2,rood], 0.00765, uc[2,geel]*1, 0.43, FALSE) # should be ~ 0.2
evaluate_logistic(uc[2,rood]*1.25, 0.00765, uc[2,geel]*1, 0.43, FALSE) # should be ~ 0
uc[2, b:= 0.00765]
uc[2, v:= 0.43]
# 500 1000
plot(
  seq(0,uc[3,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[3,rood]*1.5, by = 0.5), b = 0.004591, x0 = uc[3,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0,          0.004591, uc[3,geel]*1, 0.43, FALSE) # should be ~ 1
evaluate_logistic(uc[3,geel], 0.004591, uc[3,geel]*1, 0.43, FALSE) # should be ~ 0.8
evaluate_logistic(uc[3,rood], 0.004591, uc[3,geel]*1, 0.43, FALSE) # should be ~ 0.2
uc[3, b:= 0.004591]
uc[3, v:= 0.43]
# 50 200
plot(
  seq(0,uc[4,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[4,rood]*1.5, by = 0.5), b = 0.0153, x0 = uc[4,geel], v = 0.425, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0,          0.0153, uc[4,geel]*1, 0.43, FALSE) # should be ~ 1
evaluate_logistic(uc[4,geel], 0.0153, uc[4,geel]*1, 0.43, FALSE) # should be ~ 0.8
evaluate_logistic(uc[4,rood], 0.0153, uc[4,geel]*1, 0.43, FALSE) # should be ~ 0.2
evaluate_logistic(250,        0.0153, uc[4,geel]*1, 0.43, FALSE) # should be ~ 0
uc[4, b:= 0.0153]
uc[4, v:= 0.43]
# 100 300
plot(
  seq(0,uc[5,rood]*1.4, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[5,rood]*1.4, by = 0.5), b = 0.011477, x0 = uc[5,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0,          0.011477, uc[5,geel], 0.43, FALSE)
evaluate_logistic(uc[5,geel], 0.011477, uc[5,geel], 0.43, FALSE) # should be ~ 0.8
evaluate_logistic(uc[5,rood], 0.011477, uc[5,geel], 0.43, FALSE) # should be ~ 0.2
evaluate_logistic(400       , 0.011477, uc[5,geel], 0.43, FALSE) # should be ~ 0.2
uc[5, b:= 0.011477]
uc[5, v:= 0.43]
# 50 500
plot(
  seq(0,uc[6,rood]*1.25, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[6,rood]*1.25, by = 0.5), b = 0.0051, x0 = uc[6,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0              , 0.0051, uc[6,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[6,geel]     , 0.0051, uc[6,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[6,rood]     , 0.0051, uc[6,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[6,rood]*1.25, 0.0051, uc[6,geel], 0.43, FALSE) # should be ~0
uc[6, b:= 0.0051]
uc[6, v:= 0.43]
# 5 5
plot(
  seq(0,uc[7,rood]*2, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[7,rood]*2, by = 0.5), b = 1.75, x0 = uc[7,geel], v = 2.75, increasing = FALSE),
  ylim = c(0,1)
)
OBIC::evaluate_logistic(5, b = 1.75, x0 = uc[7,geel], v = 2.75, increasing = FALSE)
uc[7, b:= 1.75]
uc[7, v:= 2.75]
# 30 100
plot(
  seq(0,uc[8,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[8,rood]*1.5, by = 0.5), b = 0.03276, x0 = uc[8,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0              , 0.03276, uc[8,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[8,geel]     , 0.03276, uc[8,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[8,rood]     , 0.03276, uc[8,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[8,rood]*1.5 , 0.03276, uc[8,geel], 0.43, FALSE) # should be ~0
uc[8, b:= 0.03276]
uc[8, v:= 0.43]
# 1 10
plot(
  seq(0,uc[9,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[9,rood]*1.5, by = 0.5), b = 0.25505, x0 = uc[9,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0              , 0.25505, uc[9,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[9,geel]     , 0.25505, uc[9,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[9,rood]     , 0.25505, uc[9,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[9,rood]*1.5 , 0.25505, uc[9,geel], 0.43, FALSE) # should be ~0
uc[9, b:= 0.25505]
uc[9, v:= 0.43]
#100 500
plot(
  seq(0,uc[10,rood]*1.25, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[10,rood]*1.25, by = 0.5), b = 0.0057385, x0 = uc[10,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0               , 0.0057385, uc[10,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[10,geel]     , 0.0057385, uc[10,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[10,rood]     , 0.0057385, uc[10,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[10,rood]*1.5 , 0.0057385, uc[10,geel], 0.43, FALSE) # should be ~0
uc[10, b:= 0.0057385]
uc[10, v:= 0.43]
# 1 5
plot(
  seq(0,uc[11,rood]*2, by = 0.2),
  OBIC::evaluate_logistic(seq(0,uc[11,rood]*2, by = 0.2), b = 0.57388, x0 = uc[11,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0               , 0.57388, uc[11,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[11,geel]     , 0.57388, uc[11,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[11,rood]     , 0.57388, uc[11,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[11,rood]*1.5 , 0.57388, uc[11,geel], 0.43, FALSE) # should be ~0
uc[11, b:= 0.57388]
uc[11, v:= 0.43]
# 50 100
plot(
  seq(0,uc[12,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[12,rood]*1.5, by = 0.5), b = 0.0459, x0 = uc[12,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0               , 0.0459, uc[12,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[12,geel]     , 0.0459, uc[12,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[12,rood]     , 0.0459, uc[12,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[12,rood]*1.5 , 0.0459, uc[12,geel], 0.43, FALSE) # should be ~0
uc[12, b:= 0.0459]
uc[12, v:= 0.43]
# 5 10
plot(
  seq(0,uc[13,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[13,rood], by = 0.5), b = 0.574, x0 = uc[13,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0               , 0.574, uc[13,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[13,geel]     , 0.574, uc[13,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[13,rood]     , 0.45, uc[13,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[13,rood]*1.5 , 0.574, uc[13,geel], 0.43, FALSE) # should be ~0
uc[13, b:= 0.574]
uc[13, v:= 0.43]
# 10 50
plot(
  seq(0,uc[14,rood]*1.5, by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[14,rood]*1.5, by = 0.5), b = 0.0573, x0 = uc[14,geel], v = 0.43, increasing = FALSE),
  ylim = c(0,1)
)
evaluate_logistic(0               , 0.0573, uc[14,geel], 0.43, FALSE) # should be ~1
evaluate_logistic(uc[14,geel]     , 0.0573, uc[14,geel], 0.43, FALSE) # should be ~0.8
evaluate_logistic(uc[14,rood]     , 0.0573, uc[14,geel], 0.43, FALSE) # should be ~0.2
evaluate_logistic(uc[14,rood]*1.5 , 0.0573, uc[14,geel], 0.43, FALSE) # should be ~0
uc[14, b:= 0.0573]
uc[14, v:= 0.43]


# add evaluate logistics parameters to obic.nema
obic.nema <- merge.data.table(obic.nema, uc, by = c('geel', 'rood'))
# Add standaard column. A column that can be used to determine which species should always be used to calculate an average indicator score.
obic.nema[species %in% c('Ditylenchus spp.', 'Ditylenchus dipsaci', 'Xiphinema spp.',
                         'Longidorus spp.', 'Trichodorus similis', 'Trichodorus primitivus',
                         'Paratrichodorus teres', 'Rotylenchus spp.', 'Paratylenchus spp.',
                         'Meloidogyne chitwoodi/fallax', 'Meloidogyne chitwoodi', 
                         'Meloidogyne fallax', 'Meloidogyne minor', 'Meloidogyne naasi',
                         'Meloidogyne hapla', 'Cysteaaltjes', 'Pratylenchus penetrans',
                         'Pratylenchus crenatus', 'Helicotylenchus spp.', 'Pratylenchus neglectus'), standaard := TRUE]
write.csv(obic.nema, 'dev/obic.nema.csv')
nema.obic <- obic.nema
save(nema.obic, file= 'data/nema_obic.RData')
