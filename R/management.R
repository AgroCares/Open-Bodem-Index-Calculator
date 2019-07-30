#' Calculate the 'performance' of sustainable soil management
#' 
#' This function evaluates the contribution of sustainable soil management following the Label Sustainable Soil Management (CLM, 2016)
#' 
#' Note Sven: is EOS balance calculated in preprocessing, dan can be input D_ here?
#' 
#' @param A_OS_GV (numeric) The organic matter content of the soil in %
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param D_OS_BAL (numeric) The organic matter balance of the soil (in kg EOS / ha)
#' 
#' @import data.table
#' 
#' @export
calc_management <- function(A_OS_GV,B_LU_BRP, B_BT_AK,D_OS_BAL) {
  
  id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_OS_GV), length(B_LU_BRP), length(B_BT_AK), length(D_OS_BAL))
  
  # add checks Sven
  
  # Settings if needed
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_OS_GV = A_OS_GV,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    D_OS_BAL = D_OS_BAL,
    value = 0
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # measure 80% green
  
  
  # grasklaver used
  dt[grepl('klaver',crop_name), value := value + 1]
  
  # permanent grass for GtI, GtII and GtIII
  dt[D_GA > 8 & !grepl('rand',crop_name) & B_GT %in% c('I','II','III'), value := value + 5]
  dt[D_GA > 3 & crop_n == "gras" & !B_GT %in% c('I','II','III'), value := value + 1]
  dt[D_CP_GRASS == 1 & !B_GT %in% c('I','II','III'), value := value + 2]
  
  # add organic matter balance (and when positive, add one point)
  dt[D_OS_BAL > 0, value := value + 1]
  
  # parcels that are not (or minimum) ploughed are positively evaluated (add one point)
  dt[M_M4==FALSE, value := value + 1]
  
  # crop rotation of potato is at minimum 1:4 (add two points)
  dt[D_CP_POTATO > 0.15 & D_CP_POTATO <=25, value := value + 2]
  
  # is minimal 40% of the crops cereals and grasses (add one point)
  dt[D_CP_RUST>0.4 & crops_n == 'akkerbouw',value := value + 1]
  dt[D_CP_RUST>0.4 & D_CP_RUSTDEEP >0.4 & crops_n == 'akkerbouw',value := value + 1]
  
  # use of early varieties to avoid harvesting after september (stimulating catch crop too)
  dt[grepl('mais|aardappel',crop_name) & M_M11, value := value + 1]
  
  # maize crop in combination with grassland
  dt[grepl('mais',crop_name) & M_M15, value := value + 1]
  
  # is heavy machinery avoided by slurry application
  dt[soiltype.n !='veen' & M_M12, value := value + 1]
  
  # on peaty soils: make use of under water drains and ditches are frequently cleaned
  dt[soiltype.n =='veen' & M_M13, value := value + 1]
  dt[soiltype.n =='veen' & M_M14, value := value + 1]
  
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}
