#' Add Makkink correction factors and crop cover to crop rotation table
#'
#' This function adds Makkink correction factors for ET and crop cover to the crop rotation table
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
#'
#' @export
calc_makkink <- function(B_LU_BRP){

  crop_code = crop_name = crop_makkink = mcf = crop_cover = NULL
  
  
  
  # Check input
  arg.length <- length(B_LU_BRP)

  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)


  # melt makkink
  dt.mak <- melt(OBIC::crops.makkink,id.vars = 'crop_makkink', variable.name = 'month',value.name = "mcf")
  dt.mak[,month := as.integer(month)]

  # Collect input data in a table
  dt <- data.table(year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP
  )

  # merge with crops.obic
  dt <- merge(dt, OBIC::crops.obic[, list(crop_code, crop_name, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")

  # extend data.table for 12 months
  dt.gws <- CJ(year = dt$year,month = 1:12)

  # add crop category
  dt.gws <- merge(dt.gws, dt[,list(year,crop_makkink)], by = "year")


  # merge makkink data by month and crop_cat
  # be aware: some crops grow from summer up to spring next year... -> dealt with in calc_crop_rotation
  dt.gws <- merge(dt.gws,dt.mak, by = c("crop_makkink", "month"), all.x = TRUE)

  # Add crop cover
  dt.gws[,crop_cover := fifelse(mcf>0.36,1,0)]

  # sort dt on the original order
  out <- setorder(dt.gws,year)

  # return value
  value <- out

  # return
  return(value)
}

