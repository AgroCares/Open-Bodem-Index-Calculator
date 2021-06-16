#' Evaluate the carbon sequestration rate
#' 
#' This function is a wrapper function that calculates the score for carbon sequestration
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
ind_carbon_sequestration <- function(){
  
  
  
  # Check inputs
  
  
  # Create optimal input
  B_LU_BRP_optimal <- rep(233,10)
  M_GREEN_optimal <- rep(FALSE,10)
  compost_in_optimal <- rep(55,10)
  catchcrop_optimal <- rep(0,10)
  
  
  # Create minimal input
  M_GREEN_minimal <- rep(FALSE,10)
  manure_in_minimal <- rep(0,10)
  compost_in_minimal <- rep(0,10)
  catchcrop_minimal <- rep(0,10)
  
  
  # If not provided, load weather data
  if(any(is.null(A_T_MEAN)|is.null(A_PREC_MEAN)|is.null(A_ET_MEAN))){
    
    weather <- OBIC::weather.obic
    
    if(is.null(A_T_MEAN)){A_T_MEAN <- weather$A_T_MEAN}
    if(is.null(A_PREC_MEAN)){A_PREC_MEAN <- weather$A_PREC_MEAN}
    if(is.null(A_ET_MEAN)){A_ET_MEAN <- weather$A_ET_MEAN}
    
  }
  
  
  # Calculate carbon input
  carbon_input_current <- calc_carbon_input(ID, B_LU_BRP, A_P_AL, A_P_WA, M_GREEN, effective, manure_type, manure_in, compost_in)
  
  carbon_input_optimal <- calc_carbon_input(ID, B_LU_BRP = B_LU_BRP_optimal, A_P_AL, A_P_WA, M_GREEN = FALSE, effective = TRUE, manure_type = 'slurry', 
                                            manure_in = NULL, compost_in = compost_in_optimal)
  
  # Determine carbon application events
  event_current <- calc_events_current(ID, B_LU_BRP = B_LU_BRP, manure_in = carbon_input_current$manure_in, compost_in = carbon_input_current$compost_in,
                                       catchcrop = carbon_input_current$catchcrop)
  
  event_optimal <- calc_events_current(ID, B_LU_BRP = B_LU_BRP_optimal, manure_in = carbon_input_optimal$manure_in, compost_in = carbon_input_optimal$compost_in,
                                       catchcrop = carbon_input_optimal$catchcrop)
  
  event_minimal <- calc_events_minimal(ID, B_LU_BRP = B_LU_BRP, manure_in = manure_in_minimal, compost_in = compost_in_minimal,
                                       catchcrop = catchcrop_minimal)
  
  # Determine rotations
  rotation_current <- calc_crop_rotation(ID,B_LU_BRP,M_GREEN)
  
  rotation_optimal <- calc_crop_rotation(ID,B_LU_BRP = B_LU_BRP_optimal, M_GREEN = M_GREEN_optimal)
  
  rotation_minimal <- calc_crop_rotation(ID,B_LU_BRP,M_GREEN = M_GREEN_minimal)
  
  
  # Calculate correction factors
  factors_current <- calc_cor_factors(A_T_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, crop_cover = rotation_current$crop_cover, 
                                      mcf = rotation_current$mcf, renewal, depth)
  
  factors_optimal <- calc_cor_factors(A_T_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, crop_cover = rotation_optimal$crop_cover, 
                                      mcf = rotation_optimal$mcf, renewal, depth)
  
  factors_minimal <- calc_cor_factors(A_T_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, crop_cover = rotation_minimal$crop_cover, 
                                      mcf = rotation_minimal$mcf, renewal, depth)
  
  
  # Initialize C pools
  cpools <- calc_cpools(B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI, history = history, depth = depth, a = a, b = b, c = c, d = d)
  
  
  # Run RothC
  result_current <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                               IOM0 = cpools[1], CDPM0 = cpools[2], CRPM0 = cpools[3], CBIO0 = cpools [4], CHUM0 = cpools[5], 
                               event = event_current, cor_factors = factors_current, k1 = k1, k2 = k2, k3 = k3, k4 = k4, depth = depth)
  
  result_optimal <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                               IOM0 = cpools[1], CDPM0 = cpools[2], CRPM0 = cpools[3], CBIO0 = cpools [4], CHUM0 = cpools[5], 
                               event = event_optimal, cor_factors = factors_optimal, k1 = k1, k2 = k2, k3 = k3, k4 = k4, depth = depth)
  
  result_minimal <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, 
                               IOM0 = cpools[1], CDPM0 = cpools[2], CRPM0 = cpools[3], CBIO0 = cpools [4], CHUM0 = cpools[5],
                               event = event_minimal, cor_factors = factors_minimal ,k1 = k1, k2 = k2, k3 = k3, k4 = k4, depth = depth)
  
  
  # Calculate index score
  relative_score <- (result_current[length(time),OSm] - result_minimal[length(time),OSm]) / (result_optimal[length(time),OSm] - result_minimal[length(time),OSm])
  index_score <- OBIC::evaluate_logistic(relative_score,11,0.6,1.5)
  
  return(index_score) 
  
}



#' Calculate the carbon inputs on a field
#' 
#' This function calculates the carbon inputs to the field based on manure type, P status of the soil and management practices
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param effective (boolean) A vector that tells whether the catch crop was effective (i.e. did it grow sufficiently), (optional, option: yes or no)
#' @param manure_type (character) The type of manure applied on the field, options: 'slurry' or 'solid', should be a vector with value per year
#' @param manure_in (numeric) Amount of C applied on the soil via manure (kg C/ha), should be a vector with value per year
#'     
#' @export
calc_carbon_input<- function(ID, B_LU_BRP, A_P_AL, A_P_WA, M_GREEN = FALSE, effective = TRUE, manure_type = 'slurry', manure_in = NULL, compost_in = NULL){
  
  year = manure_in = manure_OC_Pration = slurry_OC_Pratio = crops.obic = NULL
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(A_P_AL),length(A_P_WA), length(M_GREEN))
  
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   A_P_AL = A_P_AL,
                   A_P_WA = A_P_WA,
                   M_GREEN = M_GREEN,
                   effective = effective,
                   manure_type = manure_type,
                   manure_in = manure_in)
  
  
  # Import and merge with crops.obic
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt, crops.obic, by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  
  if(is.null(manure_in)){
  
   if(manure_type == 'slurry'){
      ## Cropland
      # manure input in arable systems, assuming 70% dairy slurry and 30% pig slurry, 85% organic
    
      slurry_OC_Pratio <- ((0.3 * 7 / 0.33 + 0.7 * 33 / 0.7) * 0.5)
      dt[crop_n == 'akkerbouw' & A_P_WA <= 25, manure_in := 0.85 * 120 * slurry_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 25 & A_P_WA <= 36, manure_in := 0.85 * 75 * slurry_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 36 & A_P_WA <= 55, manure_in := 0.85 * 60 * slurry_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 55, manure_in := 0.85 * 50 * slurry_OC_Pratio]
    
      ## Grassland
      # manure input in grassland systems, assuming 100% dairy slurry
    
      slurry_OC_Pratio <- 33 / 0.7 * 0.5
      dt[crop_n=='gras' & A_P_AL <= 16,manure_in := 120 * slurry_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 16 & A_P_AL <= 27,manure_in := 100 * slurry_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 27 & A_P_AL <= 50,manure_in := 90 * slurry_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 50,manure_in := 80 * slurry_OC_Pratio]
    }
  
    if(manure_type == 'solid'){
      ## Cropland 
      # manure input in arable systems, assuming 70% dairy FYM and 30% pig FYM, 85% organic  
      manure_OC_Pratio <- ((0.3 * 6 / 0.33 + 0.7 * 25 / 0.7) * 0.5)
      dt[crop_n == 'akkerbouw' & A_P_WA <= 25, manure_in := 0.85 * 120 * manure_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 25 & A_P_WA <= 36, manure_in := 0.85 * 75 * manure_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 36 & A_P_WA <= 55, manure_in := 0.85 * 60 * manure_OC_Pratio]
      dt[crop_n == 'akkerbouw' & A_P_WA > 55, manure_in := 0.85 * 50 * manure_OC_Pratio]
    
      ## Grassland 
      # manure input in grassland systems, assuming 100% dairy FYM
      manure_OC_Pratio <- 25 / 0.7 * 0.5
      dt[crop_n=='gras' & A_P_AL <= 16,manure_in := 120 * manure_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 16 & A_P_AL <= 27,manure_in := 100 * manure_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 27 & A_P_AL <= 50,manure_in := 90 * manure_OC_Pratio]
      dt[crop_n=='gras' & A_P_AL > 50,manure_in := 80 * manure_OC_Pratio]
    }
  }
  
  # Set M_GREEN to TRUE for mais and potato cultivation
  dt[grepl('mais|aardappel',crop_name), M_GREEN := TRUE]
  
  
  # C input from catch crop
  dt[M_GREEN == TRUE, catchcrop := 2800]
  dt[M_GREEN != TRUE, catchcrop := 0]
  
  # Correction for effectiveness of catch crop
  dt[effective == FALSE, catchcrop := 0.4 * catchcrop]
  
  
  ## Input compost
  dt[,compost_in := compost_in]
  
  
  # Format output
  out <- setorder(dt,year)
  carbon_input <- out[,list(year,manure_in,compost_in,catchcrop)]
  
  return(carbon_input)
}



#' Determine the carbon application events for current management
#' 
#' This function determines the carbon application events for current management
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_events_current <- function(ID, B_LU_BRP, manure_in, compost_in, catchcrop){
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(year), length(M_GREEN), length(manure_in), length(catchcrop))
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   manure_in = manure_in,
                   catchcrop = catchcrop)
  
  
  # Add hc for manure types
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt,crops.obic[,list(crop_code,hc,crop_makkink,crop_eos,crop_eos_residue)], by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  # Import and merge with carbon application data
  carbon_application <- OBIC::carbon_application
  
  dt <- merge(dt, carbon_application, by = 'crop_makkink')
  
  
  
  ## Carbon from crops ---
  # Set NA values to 0
  dt[,crop_eos := fifelse(is.na(crop_eos),0,crop_eos)][,crop_eos_residue := fifelse(is.na(crop_eos_residue),0,crop_eos_residue)]
  
  # Calculate total C input from crops
  dt[,res_in := (crop_eos + crop_eos_residue)/hc]
  
  # Calculate DPM/RPM ratio
  dt[,ratio := -2.174 * hc + 2.20]
  
  # Calculate inputs for DPM and RPM pools
  dt.residue <- dt[,list(year,ratio,res_in,t_residue)]
  dt.residue[, c("CDPM","CRPM") := list(res_in * ratio / (1 + ratio), res_in * 1 / (1 + ratio))]
    
  # Moment of application
  dt.residue[, time := year - 1  + t_residue]
    
  # Event for plant residue application
  dt.residue <- dt.residue[,list(CDPM,CRPM,time)]
  event.residue <- melt(dt.residue,id.vars = "time", variable.name = "var")
    
  
    
  ## Carbon from manure ---
  # Make manure table
  dt.manure <- dt[,list(B_LU_BRP,year,t_manure,manure_in)]
  
  # Calculate carbon pools
  dt.manure[,c("CDPM","CRPM","CHUM") := list(manure_in * 0.49, manure_in * 0.49, manure_in * 0.02)]
    
  # Moment of application
  dt.manure[,time := year - 1  + t_manure]
  dt.manure[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.manure <- dt.manure[,.(CDPM,CRPM,CHUM,time)]
  event.manure <- melt(dt.manure,id.vars = "time", variable.name = "var")
    
  
    
  ## Carbon from Compost ---
  # Make compost table
  dt.compost <- dt[,list(B_LU_BRP,year,t_manure,compost_in)]
  
  # Calculate carbon pools
  dt.compost[,c("CDPM","CRPM") := list(compost_in * 0.15 / (1 + 0.15), compost_in * 1 / (1 + 0.15))]
  
  # Moment of application
  dt.compost[,time := year - 1  + t_manure]
  dt.compost[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.compost <- dt.compost[,.(CDPM,CRPM,time)]
  event.compost <- melt(dt.compost,id.vars = "time", variable.name = "var")
  
    
  
  ## Carbon  from catch crops ---
  # Make catch crop table
  dt.catchcrop <- dt[,list(B_LU_BRP,year,t_manure,catchcrop)]
  
  # Calculate carbon pools
  dt.catchcrop[,c("CDPM","CRPM") := list(catchcrop * 1.37 / (1 + 1.37), catchcrop * 1 / (1 + 1.37))]
  
  # Moment of application
  dt.catchcrop[,time := year - 1  + t_manure]
  dt.catchcrop[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.catchcrop <- dt.catchcrop[,.(CDPM,CRPM,time)]
  event.catchcrop <- melt(dt.catchcrop,id.vars = "time", variable.name = "var")
  
  
  # Event total application
  dt.event <- rbind(event.residue,event.manure,event.compost,event.catchcrop)
  setorder(dt.event,time)
  dt.event[,method := "add"]

  
  # Extend event to 50 years
  dt.event2 <- copy(dt.event)
  dt.event2[,id := .I]
  
  Blok = CJ(blok = seq(0,40,10),id = dt.event2$id)
  dt.event2 <- dt.event2[rep(id,5)]
  
  dt.event2[,id := .I]
  Blok[,id := .I]  
  
  dt.event3 = merge(Blok,dt.event2,by="id",allow.cartesian = T)
  dt.event3[,time := time + blok][,c("blok","id") := NULL]
  dt.event3 <- dt.event3[time > 0]
  setorder(dt.event3,time)
  
  return(dt.event3)
    
  }


  
#' Determine the carbon application events for minimal management
#' 
#' This function determines the carbon application events for minimal management
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_events_minimal <- function(ID, B_LU_BRP, manure_in, compost_in, catchcrop){
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(M_GREEN), length(manure_in), length(catchcrop))
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   manure_in = manure_in,
                   catchcrop = catchcrop)
  
  
  # Add hc for manure types
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt,crops.obic[,list(crop_code,hc,crop_makkink,crop_eos,crop_eos_residue)], by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  # Import and merge with carbon application data
  carbon_application <- OBIC::carbon_application
  
  dt <- merge(dt, carbon_application, by = 'crop_makkink')
  
  
  
  ## Carbon from crops ---
  # Set NA values to 0
  dt[,crop_eos := fifelse(is.na(crop_eos),0,crop_eos)]
  
  # Calculate total C input from crops
  dt[,res_in := crop_eos/hc]
  
  # Calculate DPM/RPM ratio
  dt[,ratio := -2.174 * hc + 2.20]
  
  # Calculate inputs for DPM and RPM pools
  dt.residue <- dt[,list(year,ratio,res_in,t_residue)]
  dt.residue[, c("CDPM","CRPM") := list(res_in * ratio / (1 + ratio), res_in * 1 / (1 + ratio))]
  
  # Moment of application
  dt.residue[, time := year - 1  + t_residue]
  
  # Event for plant residue application
  dt.residue <- dt.residue[,list(CDPM,CRPM,time)]
  
  dt.event <- melt(dt.residue,id.vars = "time", variable.name = "var")
  setorder(dt.event,time)
  dt.event[,method := "add"]
  
  
  # Extend event to 50 years
  dt.event2 <- copy(dt.event)
  dt.event2[,id := .I]
  
  Blok = CJ(blok = seq(0,40,10),id = dt.event2$id)
  dt.event2 <- dt.event2[rep(id,5)]
  
  dt.event2[,id := .I]
  Blok[,id := .I]  
  
  dt.event3 = merge(Blok,dt.event2,by="id",allow.cartesian = T)
  dt.event3[,time := time + blok][,c("blok","id") := NULL]
  dt.event3 <- dt.event3[time > 0]
  setorder(dt.event3,time)
  
  return(dt.event3)
}



#' Determine the crop rotation of a field
#' 
#' This function determines crop cover and makkink correction factors based on the cultivated crops
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param effective (boolean) A vector that tells whether the catch crop was effective (i.e. did it grow sufficiently), (optional, option: yes or no)
#'     
#' @export
calc_crop_rotation <- function(ID,B_LU_BRP,M_GREEN = FALSE, effective = TRUE){ 
  
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(M_GREEN))
  
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   effective = effective)
  
  # Import and merge with crops.obic
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt, crops.obic[,list(crop_code,crop_name)], by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  # Add Makkink data
  dt.mak <- calc_makkink(ID, B_LU_BRP, M_GREEN)
  
  # Merge Makkink data with field data
  dt <- merge(dt.mak,dt,by = 'year')
  
  
  # Select years with wintercereals
  year_wc <- unique(dt[B_LU_BRP == 233|B_LU_BRP == 235, year])
  
  for(i in year_wc){
    
    dt[year == i-1 & month == 10|
       year == i-1 & month == 11|
       year == i-1 & month == 12, c("crop_name","crop_cover","mcf") := list("winter cereal", 1, c(0.5,0.6,0.6))]
    
  }
  
  
  # Set M_GREEN to TRUE for mais and potato cultivation
  dt[grepl('mais|aardappel',crop_name), M_GREEN := TRUE]
  
  
  ## Select years with catch crops
  year_cc <- unique(dt[M_GREEN == TRUE & effective == TRUE, year])
  year_cc_ne <- unique(dt[M_GREEN == TRUE & effective == FALSE, year])
  
  
  # Add catch crops that were effective
  # Add catch crops for year 10 separately
  if(length(year_cc) != 0){
    if(year_cc[length(year_cc)] == 10){
   
      dt[year == 10 & month %in% 10:12, c("crop_name","crop_cover","mcf"):=list("catch crop",1,c(0.74,0.64,0.6))]
      year_cc <- year_cc[! year_cc %in% c(10)] 
    
      }
  }
      
  
  # Add catch crops to other years  
    for(i in year_cc){
    
      dt[year == i & month == 10|
         year == i & month == 11|
         year == i & month == 12|
         year == i+1 & month == 1|
         year == i+1 & month == 2|
         year == i+1 & month == 3, c("crop_name","crop_cover","mcf"):=list("catch crop",1,c(0.74,0.64,0.6,0.6,0.6,0.6))]
    }
  
  # Add catch crops that were not effective  
  for(i in year_cc_ne){
    
    dt[year == i & month == 10|
       year == i & month == 11|
       year == i & month == 12|
       year == i+1 & month == 1|
       year == i+1 & month == 2|
       year == i+1 & month == 3, c("crop_name","crop_cover","mcf"):=list("catch crop",1,0.5)]
  }  
  
  
  # Standardize months
  dt[,month:=1:120]
  
  # Format output
  rotation <- dt[,list(year, month, B_LU_BRP, mcf, crop_cover)]
  
  return(rotation)
}



