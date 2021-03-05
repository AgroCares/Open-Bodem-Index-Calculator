# make test data for nematode data table
library(data.table)
# read test data
dt <- read.csv('DEV/dummy_nema.csv', sep = ';')
setDT(dt)
saveRDS(dt, 'dummy_nema.RDS')

# inbuilt nematode table
obic.nema <- read.csv('DEV/nematodes.csv', sep = ';')
setDT(obic.nema)



# Determine values for evaluate logistic functions per Unique Combinations
uc <- unique(obic.nema[,.(geel, rood)]) # 14 unique combination, so 14 different evaluation curves need to be determined
#150 300
plot(
  seq(0,uc[1,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[1,rood], by = 0.5), b = 0.05, x0 = uc[1,geel], v = 1, increasing = FALSE),
  ylim = c(0,1)
)
uc[1, b:= 0.05]
uc[1, v:= 1]
# 200 500
plot(
  seq(0,uc[2,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[2,rood], by = 0.5), b = 0.025, x0 = uc[2,geel], v = 1.2, increasing = FALSE),
  ylim = c(0,1)
)
uc[2, b:= 0.025]
uc[2, v:= 1.2]
# 500 1000
plot(
  seq(0,uc[3,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[3,rood], by = 0.5), b = 0.01, x0 = uc[3,geel], v = 1.2, increasing = FALSE),
  ylim = c(0,1)
)
uc[3, b:= 0.01]
uc[3, v:= 1.2]
# 50 200
plot(
  seq(0,uc[4,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[4,rood], by = 0.5), b = 0.15, x0 = uc[4,geel], v = 1, increasing = FALSE),
  ylim = c(0,1)
)
uc[4, b:= 0.15]
uc[4, v:= 1]
# 100 300
plot(
  seq(0,uc[5,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[5,rood], by = 0.5), b = 0.075, x0 = uc[5,geel], v = 1, increasing = FALSE),
  ylim = c(0,1)
)
uc[5, b:= 0.075]
uc[5, v:= 1]
# 50 500
plot(
  seq(0,uc[6,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[6,rood], by = 0.5), b = 1, x0 = uc[6,geel], v = 10, increasing = FALSE),
  ylim = c(0,1)
)
uc[6, b:= 1]
uc[6, v:= 10]
# 5 5
plot(
  seq(0,uc[7,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[7,rood], by = 0.5), b = 1, x0 = uc[7,geel], v = 2, increasing = FALSE),
  ylim = c(0,1)
)
uc[7, b:= 1]
uc[7, v:= 2]
# 30 100
plot(
  seq(0,uc[8,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[8,rood], by = 0.5), b = 0.25, x0 = uc[8,geel], v = 1.5, increasing = FALSE),
  ylim = c(0,1)
)
uc[8, b:= 0.25]
uc[8, v:= 1.5]
# 1 10
plot(
  seq(0,uc[9,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[9,rood], by = 0.5), b = 3, x0 = uc[9,geel], v = 1, increasing = FALSE),
  ylim = c(0,1)
)
uc[9, b:= 3]
uc[9, v:= 1]
#100 500
plot(
  seq(0,uc[10,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[10,rood], by = 0.5), b = 0.05, x0 = uc[10,geel], v = 1.2, increasing = FALSE),
  ylim = c(0,1)
)
uc[10, b:= 0.05]
uc[10, v:= 1.2]
# 1 5
plot(
  seq(0,uc[11,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[11,rood], by = 0.5), b = 5, x0 = uc[11,geel], v = 0.5, increasing = FALSE),
  ylim = c(0,1)
)
uc[11, b:= 5]
uc[11, v:= 0.5]
# 50 100
plot(
  seq(0,uc[12,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[12,rood], by = 0.5), b = 0.5, x0 = uc[12,geel], v = 5, increasing = FALSE),
  ylim = c(0,1)
)
uc[12, b:= 0.5]
uc[12, v:= 5]
# 5 10
plot(
  seq(0,uc[13,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[13,rood], by = 0.5), b = 1, x0 = uc[13,geel], v = 1, increasing = FALSE),
  ylim = c(0,1)
)
uc[13, b:= 1]
uc[13, v:= 1]
# 10 50
plot(
  seq(0,uc[14,rood], by = 0.5),
  OBIC::evaluate_logistic(seq(0,uc[14,rood], by = 0.5), b = 5, x0 = uc[14,geel], v = 10, increasing = FALSE),
  ylim = c(0,1)
)
uc[14, b:= 5]
uc[14, v:= 10]


# add evaluate logistics parameters to obic.nema
obic.nema <- merge.data.table(obic.nema, uc, by = c('geel', 'rood'))
write.csv(obic.nema, 'DEV/obic.nema.csv')

# Try out function
A_NEMA <- copy(dt)

ind_nematodes <- function(A_NEMA){
  checkmate::assert_data_table(A_NEMA)
  dd <- merge.data.table(obic.nema, A_NEMA, by = 'species')
  dd <- dd[standaard == TRUE|!is.na(count)]
  
  # Check if all standard nematodes are present
  if(checkmate::anyMissing(dd[,count])){
    errorCondition('at least one of the "standard" nematodes seems to be a missing value, its assumed this nematode is counted and is equal to 0.')
  } 
  # Calculate score for each individual nematode species
  dd[,nem_score := OBIC::evaluate_logistic(dd[,count], b = dd[,b], x0 = dd[,geel], v = dd[,v], increasing = FALSE)]
  # Set scores where count = 0 to 1
  dd[count == 0, nem_score:=1]
  
  value <- mean(dd[,nem_score])
  return(value)
}  



  plot(
    seq(0,dd[13,rood], by = 0.5),
    OBIC::evaluate_logistic(seq(0,dd[13,rood], by = 0.5), b = 1, x0 = dd[13,geel], v = 0.1, increasing = FALSE),
    ylim = c(0,1)
  )
  OBIC::evaluate_logistic(dd[13,count], b = dd[13,b], x0 = dd[13,geel], v = dd[13,v], increasing = FALSE)


