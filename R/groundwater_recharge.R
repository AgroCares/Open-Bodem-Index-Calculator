#' Calculate groundwater recharge through precipitation surplus in the Netherlands
#' 
#' This function calculates the volume of water (in mm / ha) that infiltrates to the groundwater given the crop rotation plan.
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_gw_recharge <- function(B_LU_BRP, M_GREEN){
  
  crop_code = crop_name = crop_makkink = psp = A_PREC_MEAN = A_ET_MEAN = mcf = NULL
  
  # Check input
  arg.length <- max(length(B_LU_BRP), length(M_GREEN))
  
  # check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  crops.makkink <- as.data.table(OBIC::crops.makkink)
  
  # melt makkink
  dt.mak <- melt.data.table(crops.makkink,id.vars = 'crop_makkink', variable.name = 'month',value.name = "mcf")
  dt.mak[,month := as.integer(month)]
  
  
  # Collect input data in a table
  dt <- data.table(year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   M_GREEN = M_GREEN
  )
  
  # merge with obic crop
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # extend data.table for 12 months
  dt.gws <- CJ(year = dt$year,month = 1:12)
  
  # Merge tables
  dt <- merge(dt, dt.gws, by = "year")
  
  
  # merge makkink data by month and crop_cat
  # be aware: some crops grow from summer up to spring next year
  dt <- merge(dt,dt.mak, by = c("crop_makkink", "month"), all.x = TRUE)
  
  # Order by year
  dt <- setorder(dt,year)
  
  
  # Select years with wintercereals
  year_wc <- unique(dt[B_LU_BRP == 233|B_LU_BRP == 235, year])
  
  for(i in year_wc){
    
    dt[year == i-1 & month == 10|
         year == i-1 & month == 11|
         year == i-1 & month == 12, c("crop_name","crop_cover","mcf") := list("winter cereal", 1, c(0.5,0.6,0.6))]
    
  }
  
  
  # Set M_GREEN to TRUE for maize and potato cultivation
  dt[grepl('mais|aardappel',crop_name), M_GREEN := TRUE]
  
  
  ## Select years with catch crops and remove years before winter cereal cultivation -> winter cereal is the catch crop
  year_cc <- unique(dt[M_GREEN == TRUE, year])
  year_cc <- year_cc[!year_cc %in% (year_wc - 1)]
  
  # Add catchcrop for last year in rotaton
  if(year_cc[length(year_cc)] == arg.length){
    
    dt[year == arg.length & month %in% 10:12, c("crop_name","mcf"):=list("catch crop",c(0.74,0.64,0.6))]
    year_cc <- year_cc[! year_cc %in% arg.length]
    
  }
  
  # Add catch crops to other years
  for(i in year_cc){
    
    dt[year == i & month == 10|
         year == i & month == 11|
         year == i & month == 12|
         year == i+1 & month == 1|
         year == i+1 & month == 2|
         year == i+1 & month == 3, c("crop_name","mcf"):=list("catch crop",c(0.74,0.64,0.6,0.6,0.6,0.6))]
  }
  
  
  # Load weather data
  dt.weather <- OBIC::weather.obic
  
  # merge dt with weather data using month
  dt <- merge(dt,dt.weather, by = 'month')
  
  # calculate precipitation surplus
  dt[, psp := A_PREC_MEAN - A_ET_MEAN * mcf]
  
  
  # calculate the precipitation surplus per year
  out <- dt[,list(psp = sum(psp)),by = "year"]
  
  # return value
  D_PSP <- out[,psp]
  
  # return 
  return(D_PSP)
  
}


#' Calculate the indicator for the groundwater recharge through precipitation surplus in the Netherlands
#' 
#' This function calculates the indicator for the the groundwater recharge through precipitation surplus calculated by \code{\link{calc_gw_recharge}}
#' 
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_gw_recharge}}
#' 
#' @export
ind_gw_recharge <- function(D_PSP) {
  
  I_W_GWR <- evaluate_logistic(D_PSP,0.023,300,3)
  
  return(I_W_GWR)
  
}