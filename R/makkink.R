#' Add Makkink correction factors and crop cover to crop rotation table
#' 
#' This function adds Makkink correction factors for ET and crop cover to the crop rotation table
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_makkink <- function(ID, B_LU_BRP,M_GREEN){
  
  crop_code = crop_name = crop_makkink = mcf = crop_cover = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  crops.makkink <- as.data.table(OBIC::crops.makkink)
  
  # Check input
  arg.length <- max(length(ID), length(B_LU_BRP), length(M_GREEN))
  
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  
  # Add weather data here?


  # melt makkink
  dt.mak <- melt(crops.makkink,id.vars = 'crop_makkink', variable.name = 'month',value.name = "mcf")
  dt.mak[,month := as.integer(month)]
  
  # Collect input data in a table
  dt <- data.table(year = 1:arg.length,
                   ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   M_GREEN = M_GREEN
  )
  
  # merge with OBIC crop
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # extend data.table for 12 months
  dt.gws <- CJ(year = dt$year,month = 1:12)
  
  # add crop category
  dt.gws <- merge(dt.gws, dt[,list(year,crop_makkink,M_GREEN)], by = "year")
  
  ## Add weather data here?
  # merge with weather data using month
  #dt.gws <- merge(dt.gws,dt.weather, by = 'month')
  
  # merge makkink data by month and crop_cat
  # be aware: some crops grow from summer up to spring next year... -> dealt with in calc_crop_rotation
  dt.gws <- merge(dt.gws,dt.mak, by = c("crop_makkink", "month"), all.x = TRUE)
  
  # adjust makkink when catch crop (or green manure) is used (values of bladrammenas)
  # Extend period to spring of the next year?
  dt.gws[M_GREEN == TRUE & month == 10, mcf := 0.72]
  dt.gws[M_GREEN == TRUE & month == 11, mcf := 0.64]
  
  # Add crop cover
  dt.gws[,crop_cover := fifelse(mcf>0.36,1,0)]
  
  # sort dt on the original order
  out <- setorder(dt.gws,year)
  
  # return value
  value <- out
  
  # return 
  return(value)
}

  