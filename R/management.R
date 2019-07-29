#' Calculate the 'performance' of sustainable soil management
#' 
#' This function evaluates the contribution of sustainable soil management following the Label Sustainable Soil Management (CLM, 2016)
#' 
#' Note Sven: is EOS balance calculated in preprocessing, dan can be input D_ here?
#' 
#' @param A_OS_GV (numeric) The organic matter content of the soil in %
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param A_P_PAL (numeric) The P-AL content of the soil (mg P2O5/ 100g)
#' @param A_P_PW (numeric) The P-water content of the soil (mg P2O5 / Liter)
#' @param M_M3 (numeric) The frequency that compost is applied (every x years)
#' @param M_M6 (boolean) are catch crops (groenbemesters) frequently used: yes or no
#' 
#' @import data.table
#' 
#' @export
calc_management <- function(A_OS_GV,B_LU_BRP, B_BT_AK,A_P_PAL, A_P_WA, M_M3, M_M6) {
  
  c.diss = id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_OS_GV), length(B_LU_BRP), length(B_BT_AK))
  
  # add checks Sven
  
  # Settings if needed
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_OS_GV = A_OS_GV,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
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
  dt[,sombalance := calc_sombalance(A_OS_GV, A_P_PAL, A_P_WA, M_M3, M_M6)]
  dt[sombalance > 0, value := value + 1]
  
  # parcels that are not (or minimum) ploughed are positively evaluated (add one point)
  dt[M_M4==FALSE, value := value + 1]
  
  # crop rotation of potato is at minimum 1:4 (add two points)
  dt[D_CP_POTATO > 0.15 & D_CP_POTATO <=25, value := value + 2]
  
  # is minimal 40% of the crops cereals and grasses (add one point)
  dt[D_CP_RUST>0.4 & crops_n == 'akkerbouw',value := value + 1]
  dt[D_CP_RUST>0.4 & D_CP_RUSTDEEP >0.4 & crops_n == 'akkerbouw',value := value + 1]
  
  # 
  
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}

#' Calculate the indicator for SLV
#' 
#' This function calculates the indicator for the the S-index by using the SLV calculated by \code{\link{calc_slv}}
#' 
#' Note1: Sven: parameters need still to be estimated on recomendation tables
#' Note2: checks need to be included
#' Note3: check crop_n in crop_code tabel, moet gras-mais-bouwland bevatten
#' 
#' @param D_SLV (numeric) The value of SLV  calculated by \code{\link{calc_slv}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param B_LG_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @export
ind_sulpher <- function(D_SLV,B_LU_BRP, B_BT_AK, B_LG_CBS) {
  
  id = crop_code = soiltype = soiltype.n = crop_n = sbal = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(D_SLV), length(B_LU_BRP), length(B_BT_AK), length(B_LG_CBS))
  checkmate::assert_numeric(D_SLV, lower = -30, upper = 250, any.missing = FALSE)
  # add checks Sven for all inputs, gewas alleen bouwland gewas
  
  # make data.table to save scores
  dt = data.table(
    D_SLV = D_SLV,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    B_LG_CBS = B_LG_CBS,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Evaluate S availability for arable land  -----
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[,sbal := calc_sbal_arable(D_SLV, B_LU_BRP, B_BT_AK, B_LG_CBS)]
  dt.arable[,value := evaluate_parabolic(sbal, x.top = 0)]
  
  # Evaluate S availability for maize land -----
  dt.maize <- dt[crop_n == "mais"]
  dt.maize[,value := evaluate_logistic(D_SLV, b = 5, x0 = 5, v = 5)]
  
  # Evaluate S availability for grassland -----
  dt.grass <- dt[crop_n == "gras"]
  dt.grass[,value := evaluate_logistic(D_SLV, b = 5, x0 = 5, v = 5)]
  
  # Combine the tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return output
  return(value)
}
