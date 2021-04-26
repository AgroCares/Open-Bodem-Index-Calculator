#' Calculate groundwater recharge through precipitation surplus in the Netherlands
#' 
#' This function calculates the volume of water (in mm / ha) that infiltrates to the groundwater given the crop rotation plan.
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_gw_recharge <- function(ID, B_LU_BRP,M_GREEN){
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  crops.makkink <- as.data.table(OBIC::crops.makkink)
  
  # Check input
  arg.length <- max(length(ID), length(B_LU_BRP), length(M_GREEN))
  
  # check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  
  # long term weather data per month in de Bilt
  dt.weather <- data.table(month = 1:12,
                           prec = c(77,58,50,41,58,68,88,90,74,80,73,89),
                           eva  = c(9,16,38,66,88,97,104,83,54,30,12,7))
  
  # melt makkink
  dt.mak <- melt(crops.makkink,id.vars = 'crop_makkink', variable.name = 'month',value.name = "mcf")
  dt.mak[, month := as.integer(factor(month,levels = colnames(crops.makkink)[-1]))]
  
  # Collect input data in a table
  dt <- data.table(id = 1:arg.length,
                   ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   M_GREEN = M_GREEN
                   )
  
  # merge with OBIC crop
  dt <- merge(dt, crops.obic[, list(crop_code, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # extend data.table for 
  dt.gws <- CJ(id = dt$id,month = 1:12)
  
  # add crop category
  dt.gws <- merge(dt.gws, dt[,list(id,crop_makkink,M_GREEN)], by = "id")
  
  # merge with weather data using month
  dt.gws <- merge(dt.gws,dt.weather, by = 'month')
  
  # merge makkink data by month and crop_cat
  # be aware: some crops grow from summer up to spring next year... to be done
  dt.gws <- merge(dt.gws,dt.mak, by = c("crop_makkink", "month"), all.x = TRUE)
  
  # adjust makkink when catch crop (or green manure) is used (values of bladrammenas)
  dt.gws[M_GREEN ==TRUE & month == 10, mcf := 0.72]
  dt.gws[M_GREEN ==TRUE & month == 11, mcf := 0.64]
  
  # calculate precipitation surplus
  dt.gws[, psp := prec - eva * mcf]
  
  # calculate the precipitation surplus per year
  out <- dt.gws[,list(psp = sum(psp)),by = "id"]
  
  # sort dt on the original order
  setorder(dt,id)
  
  # return value
  value <- out[,psp]
  
  # return 
  return(value)
  
}
