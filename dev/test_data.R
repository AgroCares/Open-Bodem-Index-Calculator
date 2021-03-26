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
