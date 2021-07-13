# make test data for nematode data table
library(data.table)
# read test data
dt <- read.csv('dev/dummy_nema.csv', sep = ';')
setDT(dt)
save(dt, file = 'DEV/dummy_nema.Rdata')

# inbuilt nematode table
obic.nema <- read.csv('dev/nematodes.csv', sep = ';')
setDT(obic.nema)




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

ind <- ind_nematodes(A_NEMA = A_NEMA)
