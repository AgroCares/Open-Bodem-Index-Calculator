library(data.table)

weight.obic <- fread("DEV/weight_obic.csv")

# check whether all I_C, I_B and I_P indicators are in weight
#load("data/internal_data/products/column_description_obic.RData")
#
# check <- copy(weight.obic)
#  check[,indicator := gsub('^W_','I_',var)]
#  
#  cols <- check[,indicator]
#  
#  # what indicators should be present
#  colsd <- column_description.obic$column
#  colsd <- colsd[grepl('^I_',colsd)]
#  
  # checks
#  if(length(cols[!cols %in% colsd])==0){print('all indicators are also in column description')} else {print('indicators are missing')}
#  if(length(colsd[!colsd %in% cols])==0){print('all required indicators are present')} else {print('indicators are missing')}
  
    
save(weight.obic, file = "data/weight_obic.RData")
