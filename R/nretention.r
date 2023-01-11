#' Calculate the N leaching
#' 
#' This function calculates the potential N leaching of a soil.
#' 
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param D_NLV (numeric) The N supplying capacity of a soil (kg N ha-1 jr-1) calculated by \code{\link{calc_nlv}}
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016) 
#' @param leaching_to (character) whether it computes N leaching to groundwater ("gw") or to surface water ("ow")
#' 
#' @import data.table
#' 
#' @examples
#' calc_nleach('dekzand',265,'GtIII',145,'Zuidwest-Brabant','gw')
#' calc_nleach('rivierklei',1019,'GtIV',145,'Rivierengebied','ow')
#' 
#' @return 
#' The potential nitrogen leaching from the soil originating from soil nitrogen mineralization processes. A numeric value.
#' 
#' @export
calc_nleach <- function(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to){
  
  soiltype = crop_code = crop_category = soiltype.n = croptype.nleach = B_GT = med_nlv = NULL
  nleach_table = bodem = gewas = nf = id = leaching_to_set = NULL
  n_eff = anr.cor = n_sp.nlv = n_sp.nfert = n_sp = NULL
  nf_sand.other = nf_sand.south = nf_clay = nf_peat = nf_loess = NULL
    
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Read table of N leaching fraction
  nleach_table <- as.data.table(OBIC::nleach_table)
  nleach_table <- nleach_table[leaching_to_set == leaching_to]
  
  # Check input
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP), length(B_GWL_CLASS),length(D_NLV), length(B_AER_CBS))
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_NLV, lower = -30, upper = 250, len = arg.length) 
  checkmate::assert_choice(leaching_to, choices = c("gw", "ow"), null.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                                                 'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                                                 'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                                                 'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                                                 'Veenkoloni\u00EBn en Oldambt','Veenkolonien en Oldambt','Bouwhoek en Hogeland'), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_LU_BRP = B_LU_BRP, 
    B_GWL_CLASS = B_GWL_CLASS,
    D_NLV = D_NLV,
    B_AER_CBS = B_AER_CBS,
    value = NA_real_
  )
  
  # add soil type, crop categories and allowed N dose
  cols <- colnames(crops.obic)[grepl('^crop_cat|^crop_code|^nf_',colnames(crops.obic))]
  dt <- merge(dt, crops.obic[, mget(cols)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Re-categorize crop types
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "akkerbouw"]
  dt[crop_category == "grasland" , croptype.nleach := "gras"]
  
  # ensure correct GWL_CLASS
  dt[,B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
  
  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, nleach_table[, list(bodem, gewas, B_GT, nf)], 
              by.x = c("soiltype.n", "croptype.nleach", "B_GWL_CLASS"), 
              by.y = c("bodem", "gewas", "B_GT"), sort =FALSE, all.x = TRUE)
  
  # select the allowed effective N dose (in Dutch: N-gebruiksnorm), being dependent on soil type and region
  sand.south <- c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant')
  dt[grepl('zand|dal',B_SOILTYPE_AGR), n_eff := nf_sand.other]
  dt[grepl('zand|dal',B_SOILTYPE_AGR) & B_AER_CBS %in% sand.south, n_eff := nf_sand.south]
  dt[grepl('klei',B_SOILTYPE_AGR), n_eff := nf_clay]
  dt[grepl('veen',B_SOILTYPE_AGR), n_eff := nf_peat]
  dt[grepl('loess',B_SOILTYPE_AGR), n_eff := nf_loess]
  
  # remove columns not needed any more
  cols <- colnames(dt)[grepl('^nf_',colnames(dt))]
  dt[,(cols) := NULL]
  
  # soil- and crop-specific median NLV
  # median NLV values of all Dutch agricultural fields (N = 772574)
  dt[soiltype.n == "klei" & crop_category == "grasland", med_nlv := 139]
  dt[soiltype.n == "klei" & crop_category == "akkerbouw", med_nlv := 84]
  dt[soiltype.n == "klei" & crop_category == "mais", med_nlv := 38]
  dt[soiltype.n == "zand" & crop_category == "grasland", med_nlv := 139]
  dt[soiltype.n == "zand" & crop_category == "akkerbouw", med_nlv := 50]
  dt[soiltype.n == "zand" & crop_category == "mais", med_nlv := 18]
  dt[soiltype.n == "veen" & crop_category == "grasland", med_nlv := 250]
  dt[soiltype.n == "veen" & crop_category == "akkerbouw", med_nlv := 117]
  dt[soiltype.n == "veen" & crop_category == "mais", med_nlv := 48]
  dt[crop_category == "natuur", med_nlv := 250]

  # estimate N-efficiency of the fertilizer added (25 = default deposition, used for N-gebruiksnorm)
  # by default 0.8 for fertilizers, 0.9 for mineralized N and decreasing to 0 at high N-availability levels
  # Nitrogen recovery
  dt[, anr.cor := (D_NLV + n_eff)/(med_nlv + n_eff + 25)]
  dt[, anr.cor := pmin(1, 0.8 * anr.cor^-5)]
  dt[, n_sp.nlv := (1 - anr.cor * 0.9) * D_NLV]

  # compute (potential and soil derived) N leaching to groundwater D_NGW (mg NO3/L) or surface water D_NSW (kgN/ha)
  dt[, value := nf * n_sp.nlv]
  
  # when Groundwatertrap is unknown, set N leaching as 0 <-- to be checked if this is okay,
  dt[B_GWL_CLASS == 'unknown', value := 0]
  
  # When NLV is negative (= net immobilization), no leaching is assumed
  dt[D_NLV < 0, value := 0]
  
  # Extract relevant variable and return
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}


#' Calculate the indicator for N retention for groundwater or surface water
#' 
#' This function calculates the indicator for the N retention of the soil by using the N leaching to groundwater or surface water calculated by \code{\link{calc_nleach}}
#' 
#' @param D_NW (numeric) The value of N leaching calculated by \code{\link{calc_nleach}}
#' @param leaching_to (character) whether it evaluates N leaching to groundwater ("gw") or to surface water ("ow")
#'
#' @examples 
#' ind_nretention(D_NW = 15,leaching_to = 'gw')
#' ind_nretention(D_NW = c(.2,5.6,15.6),leaching_to = 'ow')
#'  
#' @return 
#' The evaluated score for the soil function to supply nitrogen for crop uptake. A numeric value between 0 and 1.
#' 
#' @export
ind_nretention <- function(D_NW, leaching_to){
  
  # Check inputs
  checkmate::assert_numeric(D_NW, lower = 0 , upper = 250, any.missing = FALSE)
  checkmate::assert_choice(leaching_to, choices = c("gw", "ow"), null.ok = FALSE)
  
  if (leaching_to == "gw") {
    # Evaluate the N retention for groundwater 
    value <- OBIC::evaluate_logistic(x = D_NW, b = 0.36, x0 = 12, v = 0.96, increasing = FALSE) 

  } else if (leaching_to == "ow") {
    # Evaluate the N retention for surfacewater
    value <- OBIC::evaluate_logistic(x = D_NW, b = 0.54, x0 = 5, v = 0.75, increasing = FALSE)
  }

  return(value)
}

