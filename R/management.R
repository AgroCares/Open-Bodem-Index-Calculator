#' Calculate the 'performance' of sustainable soil management
#' 
#' This function evaluates the contribution of sustainable soil management following the Label Sustainable Soil Management.
#'  
#' @param A_OS_GV (numeric) The organic matter content of the soil (in procent)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param B_GT (character) The groundwater table class
#' @param D_OS_BAL (numeric) The organic matter balance of the soil (in kg EOS per ha)
#' @param D_CP_POTATO (numeric) The fraction potato crops in crop rotation
#' @param D_CP_RUST (numeric) The fraction rustgewassen in crop rotation
#' @param D_CP_RUSTDEEP (numeric) The fraction diepe rustgewassen in crop rotation (-)
#' @param D_CP_GRASS (numeric) The fraction grassland in crop rotation
#' @param D_GA (numeric) The age of the grassland (years)
#' @param M_M6 (boolean) measure 6. are catchcrops sown after main crop (option: yes or no)
#' @param M_M10 (boolean) measure 10. is parcel for 80 percent of the year cultivated and 'green' (option: yes or no)
#' @param M_M11 (boolean) measure 11. use of early crop varieties to avoid late harvesting (option: yes or no)
#' @param M_M12 (boolean) measure 12. is sleepslangbemester used for slurry application (option: yes or no)
#' @param M_M13 (boolean) measure 13. are under water drains installed in peaty soils (option: yes or no)
#' @param M_M14 (boolean) measure 14. are ditched maintained carefully and slib applied on the land (option: yes or no)
#' @param M_M15 (boolean) measure 15. is grass used as second crop in between maize rows (option: yes or no)
#' 
#' @import data.table
#' 
#' @export
calc_management <- function(A_OS_GV,B_LU_BRP, B_BT_AK,B_GT,
                            D_OS_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                            M_M6, M_M10, M_M11, M_M12, M_M13, M_M14, M_M15) {
  
  id = crop_code = crop_name = soiltype = soiltype.n = crop_n = crop_category = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_OS_GV), length(B_LU_BRP), length(B_BT_AK), length(D_OS_BAL),length(B_GT),
                    length(D_CP_POTATO), length(D_CP_GRASS), length(D_CP_RUST), length(D_CP_RUSTDEEP),length(D_GA),
                    length(M_M6), length(M_M10), length(M_M11),length(M_M12),length(M_M13),
                    length(M_M14),length(M_M15))
 
  # add checks Sven
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_character(B_GT,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M6,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M10,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M11,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M12,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M13,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M14,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_M15,any.missing = FALSE, len = arg.length)
  
  # Settings if needed
  gt_wet <- c('GtI','GtII','GtIIb','GtIII','GtIIIb') # wet soils with undeep groundwater table
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_OS_GV = A_OS_GV,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    D_OS_BAL = D_OS_BAL,
    D_CP_GRASS = D_CP_GRASS,
    D_CP_POTATO = D_CP_POTATO,
    D_CP_RUST = D_CP_RUST,
    D_CP_RUSTDEEP = D_CP_RUSTDEEP,
    D_GA = D_GA,
    B_GT = B_GT,
    M_M6 = M_M6,
    M_M10 = M_M10,
    M_M11 = M_M11,
    M_M12 = M_M12,
    M_M13 = M_M13,
    M_M14 = M_M14,
    M_M15 = M_M15,
    value = 0
  )
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # ensure that crop name is in lower case (temporarily solution)
  dt[,crop_name := tolower(crop_name)]
  

  # evaluation of measures in an arable system -----
  
  # subset data.table
  dt.arable = dt[crop_n == 'akkerbouw' & crop_category != 'mais']
  
  # measure 1. is the parcel for 80% of the year grown by a crop (add 3 points)
  # calculate from fraction rustgewassen (suggestion of Wim)
  dt.arable[D_CP_RUST > 0.60, value := value + 3] 
  
  # measure 2. is minimal 40% of the deep rooting crops (add one point)
  dt.arable[D_CP_RUST > 0.4, value := value + 1]
  dt.arable[D_CP_RUST > 0.4 & D_CP_RUSTDEEP > 0.4, value := value + 1]
  
  # measure 3. crop rotation of potato is at minimum 1:4 (add two points)
  dt.arable[D_CP_POTATO > 0.15 & D_CP_POTATO <= 0.25, value := value + 2]
  
  # measure 4. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
  dt.arable[grepl('aardappel|bieten, suiker',crop_name) & M_M11==TRUE, value := value + 1]
  
  # measure 7. add minus points when arable crops are grown on wet peat soils
  dt.arable[B_GT %in% gt_wet & soiltype.n == 'veen', value := value - 5]
  
  # negative scores may not occur
  dt.arable[value < 0, value := 0]
  
  
  
  # evaluation of meaures in maize system -----
  dt.maize = dt[crop_category == 'mais']
  
  # measure 8. maize crop in combination with grassland
  dt.maize[M_M15 == TRUE, value := value + 1]
  
  # measure 6. are catchcrops sown after main crop (alleen klei, zand en löss al verplicht)
  dt.maize[soiltype.n =='klei' & M_M6 == TRUE, value := value + 1]
  
  # measure 4. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
  dt.maize[M_M11==TRUE, value := value + 1]
  
  # measure 9. is heavy machinery avoided by slurry application
  dt.maize[soiltype.n !='veen' & M_M12==TRUE, value := value + 1]
  
  # measure 6. add organic matter balance (and when positive, add one point)
  dt.maize[D_OS_BAL > 0, value := value + 2]
  
  # measure 2. is clover used in grassland
  dt.maize[soiltype.n !='veen' & grepl('klaver',crop_name), value := value + 1]
  
  # measure 7. add minus points when arable crops are grown on wet peat soils
  dt.maize[B_GT %in% gt_wet & soiltype.n == 'veen', value := value - 5]
  
  # negative scores may not occur
  dt.maize[value < 0, value := 0]
  
  # evaluation of measures in a grassland system -----
  
  # subset data.table
  dt.grass = dt[crop_n == 'gras']
  
  # measure 1. age of the grass
  dt.grass[D_GA > 3 & !B_GT %in% gt_wet, value := value + 1]
  dt.grass[D_CP_GRASS == 1 & !B_GT %in% gt_wet, value := value + 4]
  
  # measure 2. is clover used in grassland
  dt.grass[soiltype.n !='veen' & grepl('klaver',crop_name), value := value +1]
 
  # measure 3. permanent grass for GtI, GtII and GtIII
  dt.grass[D_GA > 8 & B_GT %in% gt_wet, value := value + 5]
  dt.grass[D_GA > 8 & B_GT %in% gt_wet & soiltype.n =='veen', value := value + 3]
  
  # measure 4. make use of under water drains
  dt.grass[soiltype.n =='veen' & M_M13==TRUE, value := value + 3]
  
  # measure 5. clean ditches frequently 
  dt.grass[soiltype.n =='veen' & M_M14, value := value + 2]
  
  # measure 6. is heavy machinery avoided by slurry application
  dt.grass[M_M12==TRUE, value := value + 1]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}


#' Calculate the indicator for sustainable management
#' 
#' This function calculates the the sustainability of strategic management options as calculated by \code{\link{calc_management}}
#' The main source of this indicator is developed for Label Duurzaam Bodembeheer (Van der Wal, 2016)
#' 
#' The current function allows a maximum score of 9 points for arable systems, 7 for maize 
#' and 7 for grass (non-peat) and 11 for grass on peat. 
#' 
#' @param D_MAN (numeric) The value of Sustainable Management  calculated by \code{\link{calc_management}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' 
#' @export
ind_management <- function(D_MAN,B_LU_BRP,B_BT_AK) {
  
  id = crop_code = soiltype = soiltype.n = crop_n = crop_name = crop_category = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(D_MAN), length(B_LU_BRP), length(B_BT_AK))
  checkmate::assert_numeric(D_MAN, lower = 0, upper = 11, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    D_MAN = D_MAN,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    value = NA_real_
  )
  
  # merge crop table with data.table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Evaluate the Sustainability of Soil Management
  dt[crop_n == 'gras', value := D_MAN / 7]
  dt[crop_n == 'gras' & soiltype.n == 'veen', value := D_MAN / 11]
  dt[crop_n == 'akkerbouw', value := D_MAN / 9]
  dt[crop_category == 'mais', value := D_MAN / 7]
  
  # Ensure no vales above 1
  dt[value > 1, value := 1]
  
  # round value
  dt[,value := round(value,2)]
  
  # prepare output
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}
