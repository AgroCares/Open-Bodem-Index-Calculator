# Script to make some test data to test workability functions

# 1
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t1 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 2
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 240  # schokkers
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t2  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)
t2.2 <- calc_workability(A_CLAY_MI = c(28), A_SILT_MI = c(20),  B_LU_BRP = c(240),  B_BT_AK = c('zeeklei'),  B_GLG = c(90),  B_GHG = c(35))

# 3
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 370  # Rand grenzend aan....
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t3  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 4
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 238
B_BT_AK <-  'zeeklei'
B_GLG <- 80
B_GHG <- 30
t4 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 5
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 980  #Lelie droogbloemen
B_BT_AK <-  'zeeklei'
B_GLG <- 90
B_GHG <- 35
t5  <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 6 
A_CLAY_MI <- 28
A_SILT_MI <- 20
B_LU_BRP <- 259  
B_BT_AK <-  'loess'
B_GLG <- 80
B_GHG <- 50
t6 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 7 
A_CLAY_MI <- 10
A_SILT_MI <- 20
B_LU_BRP <- 265  
B_BT_AK <-  'veen'
B_GLG <- 105
B_GHG <- 15
t7 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

# 8 zoals Huinink (2018)
A_CLAY_MI <- 0
A_SILT_MI <- 14
B_LU_BRP <- 1929  
B_BT_AK <-  'dekzand'
B_GLG <- 150
B_GHG <- 25
t8 <- calc_workability(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_BRP = B_LU_BRP, B_BT_AK = B_BT_AK, B_GLG = B_GLG, B_GHG = B_GHG)

#9
t9 <- calc_workability(
  A_CLAY_MI = c(28, 28, 28, 28, 28, 28, 10, 0),
  A_SILT_MI = c(20, 20, 20, 20, 20, 20, 20, 14),
  B_LU_BRP = c(238, 240, 370, 238, 980, 259, 265, 1929),
  B_BT_AK = c('zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'loess', 'veen','dekzand'),
  B_GLG = c(90, 90, 90, 80, 90, 80, 105, 150),
  B_GHG = c(35, 35, 35, 30, 35, 50, 15, 25)
)
t9
calc_workability(28,20,238,'zeeklei',90,35)

# Test negative valuses for ghg and glg
t10 <- calc_workability(
  A_CLAY_MI = c(28, 28),
  A_SILT_MI = c(20, 20),
  B_LU_BRP = c(265, 265),
  B_BT_AK = c('zeeklei', 'zeeklei'),
  B_GLG = c(76, -5),
  B_GHG = c(-233, -448)
)

# Testing build with mock input data
obiin <- readRDS('../OBIC functies bodembewerkbaarheid/dev/obiin_pdf.rds')
# need to add glg and ghg
set.seed(5)
obiin[,B_GLG := sample.int(100, 910, replace = TRUE)]
obiin[,B_GHG := sample.int(100, 910, replace = TRUE)]
for(i in 1:nrow(obiin)){
  if(obiin[i,B_GHG]>obiin[i, B_GLG]) {
    x <- obiin[i,B_GHG]
    obiin[i,B_GHG := B_GLG]
    obiin[i,B_GLG := x]
  }
}
# Values may still be equal
obiin[,B_GLG := fifelse(B_GLG-B_GHG < 10, B_GLG+10,B_GLG)]

# Calculate OBIC
result <- OBIC::obic(obiin, FALSE)
# Error on OBIC::obic_evalmeasure because I_P_WO is missing in OBIC::recom.obic
# Adding I_P_WO to recom.obic
m.obic <- as.data.table(OBIC::recom.obic)
m.test <- m.obic[indicator == 'I_B_DI']
m.test <- m.test[,indicator := 'I_P_WO']
m.test <- m.test[,m_effect := NA]
m.test <- m.test[,m_applicability := NA]
recom.obic <- rbindlist(list(m.obic, m.test))
save(recom.obic, file = 'data/recom_obic.RData')

# Test OBIC again
result <- OBIC::obic(obiin, FALSE)
# Still doesn't work, for some reason updated table with values for I_P_WO isn't used
# Try function without recomendations
result <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = FALSE)
