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
