#' Calculate the NLV
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param n.org (numeric) The organic nitrogen content of the soil in g  N / kg
#' @param c.org (numeric) The organic carbon content of the soil in kg C / ha
#' @param crop (numeric) The crop code (gewascode) from the BRP
#' @param soiltype (character) The type of soil
#' @param bulk_density (numeric) The bulk density of the soil in kg / m3
#' @param cn_ratio (numeric) 
#' @param grass.age (numeric) The age of the grass if present
#' 
#' @import data.table
#' 
#' @export
calc_nlv <- function(n.org, c.org, crop, soiltype, bulk_density, cn_ratio, grass.age) {
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(n.org), length(c.org), length(crop), length(soiltype), length(bulk_density), length(grass.age))
  checkmate::assert_numeric(n.org, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(c.org, lower = 0, upper = 500, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(crop, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(crop, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(soiltype, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(soiltype, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(bulk_density, lower = 0, upper = 1500, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cn_ratio, lower = 0, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(grass.age, lower = 0, upper = 99, len = arg.length)
  
  # Settings
  param.a <- 20 # Age of organic matter
  param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
  param.cn.micro <- 10 # CN ratio of micro organisms
  param.t <- 5 / 12 # 5 months a year
  param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
  # Collect data in a table
  a = c.ass = c.diss = crop_code = id = soiltype.n = crop_n = NULL
  dt <- data.table(
    id = 1:arg.length,
    n.org = n.org,
    c.org = c.org,
    crop = crop,
    soiltype = soiltype,
    bulk_density = bulk_density,
    cn_ratio = cn_ratio,
    grass.age = grass.age,
    value.nlv = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "crop", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by = "soiltype")
  
  # Calculate NLV for grass
  dt.grass <- dt[crop_n == "gras"]
  dt.grass[soiltype.n == "zand" & grass.age < 4, a := 30.79]
  dt.grass[soiltype.n == "zand" & grass.age >= 4 & grass.age < 7, a := 28.36]
  dt.grass[soiltype.n == "zand" & grass.age >= 7 & grass.age < 10, a := 27.78]
  dt.grass[soiltype.n == "zand" & grass.age >= 10, a := 26.57]
  dt.grass[soiltype.n == "klei" & grass.age < 4, a := 34.25]
  dt.grass[soiltype.n == "klei" & grass.age >= 4 & grass.age < 7, a := 31.54]
  dt.grass[soiltype.n == "klei" & grass.age >= 7 & grass.age < 10, a := 30.90]
  dt.grass[soiltype.n == "klei" & grass.age >= 10, a := 29.56]
  
  dt.grass[soiltype.n == "zand", value.nlv := 78 + a * n.org ^ 1.0046]
  dt.grass[soiltype.n == "klei", value.nlv := 31.7 + a * n.org ^ 1.0046]
  dt.grass[soiltype.n == "veen", value.nlv := 250]
  
  # Calculate the NLV for arable land
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[, c.diss := c.org * exp(4.7 * ((param.a + param.b * param.t)^-0.6)) - param.a^-0.6]
  dt.arable[, c.ass := c.diss / param.diss.micro]
  dt.arable[, value.nlv := ((c.diss + c.ass) / cn_ratio) - (c.ass / param.cn.micro)]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable), fill = TRUE)
  setorder(dt, id)
  value.nlv <- dt[, value.nlv]
  
  return(value.nlv)
}
