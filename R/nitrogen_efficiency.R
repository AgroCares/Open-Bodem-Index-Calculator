#' Calculate nitrogen use efficiency and leaching based on N surplus
#' 
#' This function gives an indication of the nitrogen use efficiency, the function calculates the N surplus and the resulting N leaching
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soilBRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016) 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param D_PBI (numeric) The value of phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' @param D_K (numeric) The value of K-index calculated by \code{\link{calc_potassium_availability}}
#' @param D_PH_DELTA (numeric) The pH difference with the optimal pH.
#' @param D_NLV (numeric) The value of NLV  calculated by \code{\link{calc_nlv}} 
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param B_FERT_NORM_FR (numeric) The fraction of the application norm utilized
#' 
#'         
#' @export
calc_n_efficiency <- function(B_LU_BRP, B_SOILTYPE_AGR, B_GWL_CLASS, B_AER_CBS, A_SOM_LOI, A_CLAY_MI,
                              D_PBI, D_K, D_PH_DELTA, D_NLV, M_GREEN = FALSE, B_FERT_NORM_FR = 1){
  
  crops.obic = soils.obic = leaching_to_set = crop_catergory = bodem = gewas = B_GT = NULL
  nf = n_eff = nf_sand.other = nf_sand.south = nf_clay = nf_peat = nf_loess = NULL
  soiltype = soiltype.n = croptype.nleach = crop_category = deposition = NULL
  I_P = I_K = I_PH = cf_pkph = decomposition = n_space = catchcrop = n_sp = D_NLEACH = NULL
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),length(B_SOILTYPE_AGR),length(B_GWL_CLASS),length(B_AER_CBS),length(B_FERT_NORM_FR), length(M_GREEN))
  
  # Check B parameters
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(OBIC::soils.obic$soiltype))
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = c('GtI','GtII','GtIII','GtIV','GtV','GtVI','GtVII','GtVIII',
                                                  'GtIIb','GtIIIb','GtVb'), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                                                  'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                                                  'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                                                  'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                                                  'Veenkoloni\u00EBn en Oldambt','Veenkolonien en Oldambt','Bouwhoek en Hogeland'), empty.ok = FALSE)
  checkmate::assert_numeric(B_FERT_NORM_FR, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  
  # Check SOM and CLAY
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE)
  
  checkmate::assert_logical(M_GREEN, any.missing = FALSE, len = arg.length)
  
  
  # Import data into table
  dt <- data.table(ID = 1,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_AER_CBS = B_AER_CBS,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   D_PBI = D_PBI,
                   D_K = D_K,
                   D_PH_DELTA = D_PH_DELTA,
                   D_NLV = D_NLV,
                   M_GREEN = M_GREEN,
                   B_FERT_NORM_FR = B_FERT_NORM_FR
                   )
  
  
  # Read table of N leaching fraction
  nleach_table <- as.data.table(OBIC::nleach_table)
  nleach_table <- nleach_table[leaching_to_set == 'gw']
  
  
  # add soil type, crop categories and allowed N dose
  cols <- colnames(OBIC::crops.obic)[grepl('^crop_cat|^crop_code|^nf_',colnames(OBIC::crops.obic))]
  dt <- merge(dt, OBIC::crops.obic[, mget(cols)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, OBIC::soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Re-categorize crop types
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "akkerbouw"]
  dt[crop_category == "grasland" , croptype.nleach := "gras"]
  
  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, nleach_table[, list(bodem, gewas, B_GT, nf)], 
              by.x = c("soiltype.n", "croptype.nleach", "B_GWL_CLASS"), 
              by.y = c("bodem", "gewas", "B_GT"), sort = FALSE, all.x = TRUE)
  
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
  
  # Calculate P, K and pH status
  dt[,I_P := ind_phosphate_availability(D_PBI)]
  dt[,I_K := ind_potassium(D_K,B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI)]
  dt[,I_PH := ind_ph(D_PH_DELTA)]
  
  
  # Determine correction factor if P, K and pH are optimal
  dt[,cf_pkph := fifelse(I_P > 0.9 & I_K > 0.9 & I_PH > 0.9,0.9,1) ]
  
  # Set N deposition
  dt[,deposition := 25] # Fixed value
  
  # Calcualte the N application space according to the norm
  dt[,n_space := (1 - B_FERT_NORM_FR) * n_eff]
  
  # N uptake by catchcrop
  # Check values https://www.wur.nl/upload_mm/c/8/1/6b63d919-1690-4f07-981a-07b3b6a3e7f1_1705577_Oene%20Oenema%20bijlage%201.pdf
  dt[,catchcrop := fifelse(M_GREEN, 75,0)]
  
  # Calculate N surplus
  dt[,n_sp := max(0,((deposition + D_NLV) - n_space - catchcrop) * cf_pkph)]
  
  # compute (potential and soil derived) N leaching to groundwater D_NGW (mg NO3/L) 
  dt[, D_NLEACH := nf * n_sp]
  
  return(dt[,D_NLEACH])
  
}



#' Calculate an indicator value for nitrogen use efficiency and leaching based on N surplus
#' 
#' This function gives an indicator value for nitrogen use efficiency calculated by \code{\link{calc_n_efficiency}}, this function makes use of \code{\link{ind_nretention}}
#' 
#' @param D_NLEACH (numeric) The value of N leaching calculated by \code{\link{calc_n_efficiency}}
#' 
#'         
#' @export
ind_n_efficiency <- function(D_NLEACH){
  
    # Evaluate the N retention for groundwater 
    I_W_NGW <- ind_nretention(D_NLEACH,'gw')
   
  
  return(I_W_NGW)
  
}

