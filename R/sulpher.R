#' Calculate the SLV
#' 
#' This function calculates a S-balance given the SLV (Sulpher supplying capacity) of a soil
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (in procent)
#' @param A_S_RT (numeric) The total Sulpher content of the soil (in mg S per kg)
#' @param D_BDS (numeric) The bulk density of the soil (in kg per m3)
#' 
#' @import data.table
#' 
#' @export
calc_slv <- function(B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS,A_SOM_LOI,A_S_RT, D_BDS) {
  
  a = c.ass = c.diss = id = crop_code = soiltype = soiltype.n = crop_category = NULL
  minip.a = D_OC = A_CS_RAT = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_S_RT), length(A_SOM_LOI), length(B_LU_BRP), 
                    length(B_SOILTYPE_AGR), length(B_AER_CBS),length(D_BDS))
  checkmate::assert_numeric(A_S_RT, lower = 0, upper = 10000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                                                 'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                                                 'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                                                 'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                                                 'Veenkoloni\xebn en Oldambt','Bouwhoek en Hogeland'), empty.ok = FALSE)
  checkmate::assert_numeric(D_BDS, lower = 100, upper = 1900, any.missing = FALSE, len = arg.length)
  
  # Settings
  param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
  param.cs.micro <- 100 # CS ratio of micro organisms
  param.t <- 5 / 12 # 5 months a year
  param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_S_RT = A_S_RT,
    D_BDS = D_BDS,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Calculate SLV for grass (deze formule: Stot = g/kg en dichtheid in g/cm3)
  dt.grass <- dt[crop_category == "grasland"]
  dt.grass[, value := 17.8 * A_S_RT * 0.001 * D_BDS * 0.001]
  
  # Calculate SLV for maize for 0-25 cm depth
  dt.maize <- dt[crop_category == "mais"]
  dt.maize[, value := 41.2 * A_S_RT * 0.001 * D_BDS * 2.5 * 0.001]
  # correction for the length of growing season (43.5%) 
  # ref: NMI rapport 1252.N.07; den Boer et al. 2007 Zwavelvoorziening van snijmaïs
  dt.maize[, value := value * 0.435]
  
  # Calculate the SLV for arable land
  dt.arable <- dt[crop_category == "akkerbouw"]
  
    # set initial age of the organic matter
    dt.arable[, minip.a := 20]
    dt.arable[grepl('duinzand',B_SOILTYPE_AGR), minip.a := 14.5]
    dt.arable[grepl('moerige_klei',B_SOILTYPE_AGR), minip.a := 35]
  
    # calculate C-stock (kg/ ha) and CS ratio (sven: check unit)
    dt.arable[,D_OC := A_SOM_LOI * 100 * 100 * 0.3 * D_BDS * 0.01]
    dt.arable[,A_CS_RAT := A_SOM_LOI * 10 / (A_S_RT * 0.001)]
  
    # calculate S-mineralization via MINIP (Postma & Bussink, 2004)
    dt.arable[, c.diss := D_OC * (1 - exp(4.7 * ((minip.a + param.b * param.t)^-0.6 - minip.a^-0.6)))]
    dt.arable[, c.ass := c.diss / param.diss.micro]
    dt.arable[, value := ((c.diss + c.ass) / A_CS_RAT) - (c.ass / param.cs.micro)]
    dt.arable[value > 150, value := 150]
  
  # Calculate the SLV for nature land
  dt.nature <- dt[crop_category == "natuur"]
  dt.nature[,value := 1.5 * A_S_RT * 0.001 * D_BDS * 0.001]
      
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.maize,dt.arable,dt.nature), fill = TRUE)
  dt[value > 250, value := 250]
  dt[value < -30, value := -30]
  setorder(dt, id)
  value <- dt[, value]
  
  # return S-supply
  return(value)
}

#' Calculate the indicator for delta S-balans arable
#' 
#' This function calculates the change in S-balans compared to averaged S-supply as given in fertilizer recommendation systems.
#' 
#' @param D_SLV (numeric) The value of SLV  calculated by \code{\link{calc_slv}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @export
calc_sbal_arable <- function(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS) {
  
  id = crop_code = soiltype = soiltype.n = crop_category = cropclass = NULL
  clust = slv_av = sfert = sreq = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(D_SLV), length(B_LU_BRP), length(B_SOILTYPE_AGR), length(B_AER_CBS))
  checkmate::assert_numeric(D_SLV, lower = -30, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    D_SLV = D_SLV,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # add S supply from soil and fertilizer 
  # given regional avareged deposition, groundwater supply, S supply by manure and irrigation
  
  # ensure lower case character
  dt[,B_AER_CBS := tolower(B_AER_CBS)]
  
  # add cluster variable to be used later (related to soil type and agronomic region)
  # Remark YF: Not all B_AER_CBS is covered, resulting in NAs for 'clust'.
  # For example, dekzand soils in Rivierngebied miss a clust value.
  dt[grepl('klei',B_SOILTYPE_AGR) & grepl('bouwh|oldambt',B_AER_CBS), clust := 1]
  dt[grepl('klei',B_SOILTYPE_AGR) & grepl('rivier|zuidwestelijk',B_AER_CBS), clust := 2]
  dt[grepl('klei',B_SOILTYPE_AGR) & grepl('ijsselmeer',B_AER_CBS), clust := 3]
  dt[grepl('klei',B_SOILTYPE_AGR) & grepl('noord|westelijk holland',B_AER_CBS), clust := 4]
  dt[grepl('klei',B_SOILTYPE_AGR) & grepl('hollands/utrechts weidegebied|waterland',B_AER_CBS), clust := 5]
  dt[B_SOILTYPE_AGR=='veen', clust := 6]
  dt[grepl('dal|zand|xxx',B_SOILTYPE_AGR) & grepl('noord|oldambt',B_AER_CBS), clust := 7]
  dt[grepl('dal|zand|xxx',B_SOILTYPE_AGR) & grepl('oostelijk|centraal|zuidelijk|zuidwest-brabant',B_AER_CBS), clust := 8]
  dt[B_SOILTYPE_AGR=='loess',clust := 9]
  dt[grepl('klei',B_SOILTYPE_AGR) & is.na(clust), clust := 10]
  dt[grepl('dal|zand|xxx',B_SOILTYPE_AGR) & is.na(clust), clust := 11]
  
  # add crop S requirement classes  
  dt[,cropclass := calc_cropclass(B_LU_BRP,B_SOILTYPE_AGR,nutrient='S')]
  
  # estimate required S supply from soil and fertilizers
  dt[clust==1, slv_av := 20]
  dt[clust==2, slv_av := 21]
  dt[clust==3, slv_av := 45]
  dt[clust==4, slv_av := 32]
  dt[clust==5, slv_av := 41]
  dt[clust==6, slv_av := 45]
  dt[clust==7, slv_av := 10]
  dt[clust==8, slv_av := 10]
  dt[clust==9, slv_av := 16]
  # For combinations that are outside table 6.2 of Handboek Bodem & Bemesting the average slv_av per soiltype is used
  dt[clust==10, slv_av := mean(c(20, 21, 45, 32, 41))]
  dt[clust==11, slv_av := mean(c(10, 10))]
  
  # estimate required fertilizer dose
  dt[, sfert := 0]
  dt[cropclass == 'class1' & clust %in% c(1,8), sfert := 50]
  dt[cropclass == 'class1' & clust == 7, sfert := 55]
  dt[cropclass == 'class1' & clust == 9, sfert := 45]
  dt[cropclass == 'class1' & clust == 2, sfert := 25]
  dt[cropclass == 'class1' & clust == 4, sfert := 15]
  dt[cropclass == 'class1' & clust %in% c(3,5), sfert := 10]
  dt[cropclass == 'class2' & clust == 7, sfert := 25]
  dt[cropclass == 'class2' & clust %in% c(1,8), sfert := 20]
  dt[cropclass == 'class2' & clust == 9, sfert := 15]
  dt[cropclass == 'class2' & clust == 2, sfert := 10]
  dt[cropclass == 'class3' & clust %in% c(1,7,8,9), sfert := 10]
  # For combinations that are outside table 6.2 of Handboek Bodem & Bemesting the average sfert per soiltype is used
  dt[cropclass == 'class1' & clust == 10, sfert := mean(c(50, 25, 10, 15, 10, 0))]
  dt[cropclass == 'class1' & clust == 11, sfert := mean(c(55, 50))]
  dt[cropclass == 'class2' & clust == 10, sfert := mean(c(20, 0, 0, 0, 0, 0))]
  dt[cropclass == 'class2' & clust == 11, sfert := mean(c(25, 20))]
  dt[cropclass == 'class3' & clust == 10, sfert := mean(c(10, 0, 0, 0, 0, 0))]
  dt[cropclass == 'class3' & clust == 11, sfert := mean(c(10, 10))]
  
  # total S requirement (kg S / ha)
  dt[,sreq := slv_av + sfert]
  
  # estimated SLV compared to total S requirement
  dt[,value := D_SLV - sreq]
  
  dt[value > 250, value := 250]
  dt[value < -30, value := -30]
  
  # extract value from dt 
  setorder(dt, id)
  value <- dt[, value]
  
  # return value change in S-balance
  return(value)
}

#' Calculate the indicator for SLV
#' 
#' This function calculates the indicator for the the S-index by using the SLV calculated by \code{\link{calc_slv}}
#' 
#' @param D_SLV (numeric) The value of SLV  calculated by \code{\link{calc_slv}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @export
ind_sulpher <- function(D_SLV,B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS) {
  
  id = crop_code = soiltype = soiltype.n = crop_category = sbal = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(D_SLV), length(B_LU_BRP), length(B_SOILTYPE_AGR), length(B_AER_CBS))
  checkmate::assert_numeric(D_SLV, lower = -30, upper = 250, any.missing = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, len = arg.length)
  
  # make data.table to save scores
  dt = data.table(
    id = 1:arg.length,
    D_SLV = D_SLV,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_AER_CBS = B_AER_CBS,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Evaluate S availability for arable land  -----
  dt.arable <- dt[crop_category == "akkerbouw"]
  if(nrow(dt.arable)>0){
    dt.arable[,sbal := calc_sbal_arable(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]
    dt.arable[,value := evaluate_logistic(sbal, b = 0.5, x0 = -4, v = 3)]
  }

  # Evaluate S availability for maize land -----
  dt.maize <- dt[crop_category == "mais"]
  dt.maize[,value := evaluate_logistic(D_SLV, b = 0.29, x0 = 15, v = 1.7)]
  
  # Evaluate S availability for grassland -----
  dt.grass <- dt[crop_category == "grasland"]
  dt.grass[,value := evaluate_logistic(D_SLV, b = 0.29, x0 = 15, v = 1.7)]
  
  # EValuate S availability for nature ----
  dt.nature <- dt[crop_category =='natuur']
  dt.nature[,value := 1]
  
  # Combine the tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return output
  return(value)
}
