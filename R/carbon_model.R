#' Evaluate the carbon sequestration rate
#' 
#' This function is a wrapper function that calculates the evaluation of the C pools over time for the different management types and produces a score for carbon sequestration
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' @param A_TEMP_MEAN (numeric) Mean monthly temperature (dC), should be a vector of 12 elements, optional
#' @param A_PREC_MEAN (numeric) Mean monthly precipitation (mm), should be a vector of 12 elements, optional
#' @param A_ET_MEAN (numeric) Mean actual evapo-transpiration (mm), should be a vector of 12 elements, optional
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop, optional
#' @param manure_in (numeric) Annual amount of C applied to the soil via manure (kg C/ha), should be a vector with a value per year, optional
#' @param compost_in (numeric) Annual amount of C applied to the soil via compost (kg C/ha), should be a vector with a value per year, optional
#' @param manure_type (character) The type of manure applied on the field, options: 'slurry' or 'solid', should be a vector with a value per year, optional
#' @param history (character) The manure history of the soil, optional (options: 'default', 'grass_rn' for grassland renewal, 'manure' for intensive manure application and 'manual')
#' @param effectivity (boolean) A vector that tells whether the catch crop was effective (i.e. did it grow sufficiently), optional
#' @param renewal (numeric) The years in which grassland renewal takes place (vector of years), optional
#' @param c_fractions (numeric) A vector of the fractions of total carbon in the IOM, DPM, RPM and BIO pool (-), the fraction of the HUM pool is derived form these values; if not provided, default values are used, optional
#' @param dec_rates (numeric) A vector of the decomposition rate constants for the DPM, RPM, BIO and HUM pool (/year); if not provided, default values are used, optional
#' 
#' @export
calc_c_seq_field <- function(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_P_AL, A_P_WA, A_DEPTH = 0.3,
                                     A_TEMP_MEAN = NULL, A_PREC_MEAN = NULL, A_ET_MEAN = NULL, M_GREEN = NULL, 
                                     manure_in = NULL, compost_in = NULL, manure_type = 'slurry', history = 'default', effectivity = TRUE, renewal = NULL, 
                                     c_fractions = c(0.0558,0.015,0.125,0.015), dec_rates = c(10,0.3,0.66,0.02)){
  
  crops.obic = soils.obic = time = OSm = NULL
  
  
  # Import RothC correction factors
  a <- cor_factors[1]
  b <- cor_factors[2]
  c <- cor_factors[3]
  d <- cor_factors[4]
 
  # Import decomposition rate constants
  k1 <- dec_rates[1]
  k2 <- dec_rates[2]
  k3 <- dec_rates[3]
  k4 <- dec_rates[4]
  
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 10)
  
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = 10) # Add length?
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = 10) # Add length?
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE) # Add lenght?
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE)
  
  checkmate::assert_numeric(A_TEMP_MEAN, lower = -30, upper = 50, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_PREC_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_ET_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = 10)
  checkmate::assert_logical(effectivity,any.missing = FALSE, len = 10)
  
  checkmate::assert_numeric(manure_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_numeric(compost_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_character(manure_type,any.missing = FALSE, len = 10)
  checkmate::assert_subset(manure_type, choices = c('slurry','solid'), empty.ok = FALSE)
  checkmate::assert_character(history,any.missing = FALSE, len = 10)
  checkmate::assert_subset(history, choices = c('defautl','grass_rn','manure','manual'), empty.ok = FALSE)
  
  checkmate::assert_numeric(renewal, any.missing = FALSE, min.len = 1, max.len = 10)
  checkmate::assert_subset(renewal, choices = 1:10, empty.ok = FALSE)
  
  checkmate::assert_numeric(a, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(b, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(c, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(d, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  
  checkmate::assert_numeric(k1, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k2, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k3, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k4, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  
  # Check if renewal only occurs in years with grass
  
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
  if(any(is.null(A_TEMP_MEAN)|is.null(A_PREC_MEAN)|is.null(A_ET_MEAN))){
    
    weather <- OBIC::weather.obic
    
    if(is.null(A_TEMP_MEAN)){A_TEMP_MEAN <- weather$A_TEMP_MEAN}
    if(is.null(A_PREC_MEAN)){A_PREC_MEAN <- weather$A_PREC_MEAN}
    if(is.null(A_ET_MEAN)){A_ET_MEAN <- weather$A_ET_MEAN}
    
  }
  
  
  # Calculate carbon input
  carbon_input_current <- calc_carbon_input(B_LU_BRP, A_P_AL, A_P_WA, M_GREEN, effectivity, manure_type, manure_in, compost_in)
  
  carbon_input_optimal <- calc_carbon_input(B_LU_BRP = B_LU_BRP_optimal, A_P_AL, A_P_WA, M_GREEN = FALSE, effectivity = TRUE, manure_type = 'slurry', 
                                            manure_in = NULL, compost_in = compost_in_optimal)
  
  # Determine carbon application events
  event_current <- calc_events_current(B_LU_BRP = B_LU_BRP, manure_in = carbon_input_current$manure_in, compost_in = carbon_input_current$compost_in,
                                       catchcrop = carbon_input_current$catchcrop)
  
  event_optimal <- calc_events_current(B_LU_BRP = B_LU_BRP_optimal, manure_in = carbon_input_optimal$manure_in, compost_in = carbon_input_optimal$compost_in,
                                       catchcrop = carbon_input_optimal$catchcrop)
  
  event_minimal <- calc_events_minimal(B_LU_BRP = B_LU_BRP, manure_in = manure_in_minimal, compost_in = compost_in_minimal,
                                       catchcrop = catchcrop_minimal)
  
  # Determine rotations
  rotation_current <- calc_crop_rotation(B_LU_BRP,M_GREEN)
  
  rotation_optimal <- calc_crop_rotation(B_LU_BRP = B_LU_BRP_optimal, M_GREEN = M_GREEN_optimal)
  
  rotation_minimal <- calc_crop_rotation(B_LU_BRP,M_GREEN = M_GREEN_minimal)
  
  
  # Calculate correction factors
  factors_current <- calc_cor_factors(A_TEMP_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, A_DEPTH, 
                                      crop_cover = rotation_current$crop_cover, mcf = rotation_current$mcf, renewal)
  
  factors_optimal <- calc_cor_factors(A_TEMP_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, A_DEPTH,
                                      crop_cover = rotation_optimal$crop_cover, mcf = rotation_optimal$mcf, renewal)
  
  factors_minimal <- calc_cor_factors(A_TEMP_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, A_DEPTH, 
                                      crop_cover = rotation_minimal$crop_cover, mcf = rotation_minimal$mcf, renewal)
  
  
  # Initialize C pools
  cpools <- calc_cpools(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH, history, c_fractions)
  
  
  # Run RothC
  result_current <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH, event = event_current, 
                               pool_size = cpools, cor_factors = factors_current, dec_rates)
  
  result_optimal <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH, event = event_optimal,
                               pool_size = cpools, cor_factors = factors_optimal, dec_rates)
  
  result_minimal <- calc_rothc(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH, event = event_minimal,
                               pools_size = cpools, cor_factors = factors_minimal, dec_rates)
  
  
  # Calculate index score
  relative_score <- (result_current[length(time),OM] - result_minimal[length(time),OM]) / (result_optimal[length(time),OM] - result_minimal[length(time),OM])
  index_score <- OBIC::evaluate_logistic(relative_score,11,0.6,1.5)
  
  out <- list(result_current,
              result_optimal,
              result_minimal,
              index_score)
  
  return(out) 
  
}



#' Calculate the carbon inputs on a field
#' 
#' This function calculates the carbon inputs to the field based on manure type, P status of the soil and management practices
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop, optional
#' @param effectivity (boolean) A vector that tells whether the catch crop was effective (i.e. did it grow sufficiently), optional
#' @param manure_type (character) The type of manure applied on the field, options: 'slurry' or 'solid', should be a vector with a value per year
#' @param manure_in (numeric) Annual amount of C applied to the soil via manure (kg C/ha), should be a vector with a value per year, optional, if NU
#' @param compost_in (numeric) Annual amount of C applied to the soil via compost (kg C/ha), should be a vector with a value per year, optional
#'     
#' @export
calc_carbon_input <- function(B_LU_BRP, A_P_AL, A_P_WA, M_GREEN = FALSE, effectivity = TRUE, manure_type = 'slurry', manure_in = NULL, compost_in = NULL){
  
  crop_n = crop_name = crops.obic = catchcrop = manure_OC_Pration = slurry_OC_Pratio =  NULL
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = 10) # Add length?
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = 10) # Add length?
  
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = 10)
  checkmate::assert_logical(effectivity,any.missing = FALSE, len = 10)
  
  checkmate::assert_numeric(manure_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_numeric(compost_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_character(manure_type,any.missing = FALSE, len = 10)
  checkmate::assert_subset(manure_type, choices = c('slurry','solid'), empty.ok = FALSE)
  
  
  # Collect data in a table
  dt <- data.table(year = 1:10,
                   B_LU_BRP = B_LU_BRP,
                   A_P_AL = A_P_AL,
                   A_P_WA = A_P_WA,
                   M_GREEN = M_GREEN,
                   effectivity = effectivity,
                   manure_type = manure_type,
                   manure_in = manure_in,
                   compost_in = compost_in)
  
  
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
  dt[effectivity == FALSE, catchcrop := 0.4 * catchcrop]
  
  
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
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param manure_in (numeric) Annual amount of C applied to the soil via manure (kg C/ha), should be a vector with a value per year, optional
#' @param compost_in (numeric) Annual amount of C applied to the soil via compost (kg C/ha), should be a vector with a value per year, optional
#' @param catchcrop (numeric) Amount of C applied to the soil via catch crop (kg C/ha), should be a vector with a value per year, optional
#' @param grass_fertilization (numeric) Timing of fertilization application, optional (options: 1, 2, 3)
#'     
#' @export
calc_events_current <- function(B_LU_BRP, manure_in, compost_in, catchcrop,grass_fertilization){
  
  grass_fertilization = crop_code = hc = crop_makkink = crop_eos = crop_eos_residue = crop_eos_ressidue = application = NULL
  CDPM = CRPM = CHUM = CBIO = time = t_manure = t_residue = res_in = ratio = method = id = blok = NULL
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  checkmate::assert_numeric(manure_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_numeric(compost_in, lower = 0, upper = 20000, any.missing = FALSE) # Check 
  
  # Subset for grassland -> check subset
  checkmate::assert_numeric(grass_fertilization, any.missing = FALSE, len = 10)
  checkmate::assert_subset(grass_fertilization, choices = c(1,2,3), empty.ok = FALSE)
  
  # Check catchcrop?
  
  # Collect data in a table
  dt <- data.table(year = 1:10,
                   B_LU_BRP = B_LU_BRP,
                   manure_in = manure_in,
                   catchcrop = catchcrop,
                   grass_fertilization = grass_fertilization)
  
  
  # Add hc for manure types
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt,crops.obic[,list(crop_code,hc,crop_makkink,crop_eos,crop_eos_residue)], by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  # Import and merge with carbon application data
  carbon.application.obic <- OBIC::carbon.application.obic
  
  dt <- merge(dt, carbon.application.obic, by = 'crop_makkink')
  
  
  ## Carbon from manure on grassland ---
  if(length(dt[crop_makkink == 'grasland', year])>0){
    
    # Make manure table for grassland  
    dt.manure_grass <- dt[crop_makkink == 'grasland',list(year,manure_in)]
  
    # Make fertilizer timing table
    application_grass <- dt[,list(year,grass_fertilization)]
  
    # Add different timings for manure application
    dt.manure_grass[,c('t1','t2','t3','t4','t5') := list(year - 1 + 3/12, year - 1 + 5/12, year - 1 + 6.5/12, year - 1 + 7.5/12, year - 1 + 9/12)]
  
    dt.manure_grass <- melt.data.table(dt.manure_grass, id.vars = c('year','manure_in'), variable.name = 'application', value.name = 'time')
  
    # Merge year with fertilizer timing
    dt.manure_grass <- merge(dt.manure_grass,application_grass,by = 'year')
  
    ## Determine fertilizer distribution
    # Fertilization in March, half June and September
    dt.manure_grass[grass_fertilization == 1 & application == "t1", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.417, manure_in * 0.49 * 0.417, manure_in * 0.02 * 0.417)]
    dt.manure_grass[grass_fertilization == 1 & application == "t3", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.333, manure_in * 0.49 * 0.333, manure_in * 0.02 * 0.333)]        
    dt.manure_grass[grass_fertilization == 1 & application == "t5", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.25, manure_in * 0.49 * 0.25, manure_in * 0.02 * 0.25)]
    
    # Fertilization in March and half July
    dt.manure_grass[grass_fertilization == 2 & application == "t1", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.5, manure_in * 0.49 * 0.5, manure_in * 0.02 * 0.5)]
    dt.manure_grass[grass_fertilization == 2 & application == "t4", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.5, manure_in * 0.49 * 0.5, manure_in * 0.02 * 0.5)]        
  
    # Fertilization in May and September
    dt.manure_grass[grass_fertilization == 3 & application == "t2", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.5, manure_in * 0.49 * 0.5, manure_in * 0.02 * 0.5)]
    dt.manure_grass[grass_fertilization == 3 & application == "t5", c("CDPM","CRPM","CHUM") := list(manure_in * 0.49 * 0.5, manure_in * 0.49 * 0.5, manure_in * 0.02 * 0.5)]        
    
    # Make an event table
    event.manure_grass <- dt.manure_grass[,list(CDPM,CRPM,CHUM,time)]
  
    event.manure_grass <- melt.data.table(event.manure_grass, id.vars = 'time', na.rm = TRUE)
  
  }else{ 
      
    event.manure_grass <- data.table(time = 1, variable = 'CDPM', value = 0) 
    }
  
  
  ## Carbon from manure for other crops ---
  if(length(dt[crop_makkink != 'grasland', year])){
    
    # Make manure table for other crops
    dt.manure <- dt[crop_makkink != 'grasland',list(B_LU_BRP,year,t_manure,manure_in)]
  
    # Calculate carbon pools
    dt.manure[,c("CDPM","CRPM","CHUM") := list(manure_in * 0.49, manure_in * 0.49, manure_in * 0.02)]
  
    # Moment of application
    dt.manure[,time := year - 1  + t_manure]
    dt.manure[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
    # Event for manure application
    dt.manure <- dt.manure[,list(CDPM,CRPM,CHUM,time)]
    event.manure <- melt(dt.manure,id.vars = "time")
  
  }else{ 
    
    event.manure <- data.table(time = 1, variable = 'CDPM', value = 0) 
  }
  
  
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
  event.residue <- melt(dt.residue,id.vars = "time")
  
  
  
  ## Carbon from clippings ---
  if(length(dt[crop_makkink == 'grasland', year])>0){
    
  # Make table for grassland  
  dt.clip_grass <- dt[crop_makkink == 'grasland',list(year)]
  dt.clip_grass <- dt.clip_grass[,c('time',"CDPM","CRPM") :=  list(year - 1 + 0.83,346,354)]
  
  # Remove unnecessary columns and melt table
  dt.clip_grass[,year := NULL]
  event.clip_grass <- melt(dt.clip_grass, id.vars = "time")
  
  }else{ 
    
    event.clip <- data.table(time = 1, variable = 'CDPM', value = 0) 
  }
  

  ## Carbon from Compost ---
  # Make compost table
  dt.compost <- dt[,list(B_LU_BRP,year,t_manure,compost_in)]
  
  # Calculate carbon pools
  dt.compost[,c("CDPM","CRPM") := list(compost_in * 0.15 / (1 + 0.15), compost_in * 1 / (1 + 0.15))]
  
  # Moment of application
  dt.compost[,time := year - 1  + t_manure]
  dt.compost[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.compost <- dt.compost[,list(CDPM,CRPM,time)]
  event.compost <- melt(dt.compost,id.vars = "time")
  
    
  
  ## Carbon  from catch crops ---
  # Make catch crop table
  dt.catchcrop <- dt[,list(B_LU_BRP,year,t_manure,catchcrop)]
  
  # Calculate carbon pools
  dt.catchcrop[,c("CDPM","CRPM") := list(catchcrop * 1.37 / (1 + 1.37), catchcrop * 1 / (1 + 1.37))]
  
  # Moment of application
  dt.catchcrop[,time := year - 1  + t_manure]
  dt.catchcrop[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for catch crop application
  dt.catchcrop <- dt.catchcrop[,list(CDPM,CRPM,time)]
  event.catchcrop <- melt(dt.catchcrop,id.vars = "time")
  
  
  # Event total application
  dt.event <- rbind(event.manure_grass,event.manure,event.residue,event.clip_grass,event.compost,event.catchcrop)
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
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param manure_in (numeric) Annual amount of C applied to the soil via manure (kg C/ha), should be a vector with a value per year, optional
#' @param compost_in (numeric) Annual amount of C applied to the soil via compost (kg C/ha), should be a vector with a value per year, optional
#' @param catchcrop (numeric) Amount of C applied to the soil via catch crop (kg C/ha), should be a vector with a value per year, optional
#'     
#' @export
calc_events_minimal <- function(B_LU_BRP, manure_in, compost_in, catchcrop){
  
  crop_code = hc = crop_makkink = crop_eos = crop_eos_residue = res_in = ratio = t_residue = time = CDPM = CRPM = method = id = blok = NULL
  
  # Check inputs
  
  
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(manure_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  checkmate::assert_numeric(compost_in, lower = 0, upper = 20000, any.missing = FALSE) # Check upper
  
  # Catchcrop?
  
  # Collect data in a table
  dt <- data.table(year = 1:10,
                   B_LU_BRP = B_LU_BRP,
                   manure_in = manure_in,
                   catchcrop = catchcrop)
  
  
  # Add hc for manure types
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt,crops.obic[,list(crop_code,hc,crop_makkink,crop_eos,crop_eos_residue)], by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  # Import and merge with carbon application data
  carbon.application.obic <- OBIC::carbon.application.obic
  
  dt <- merge(dt, carbon.application.obic, by = 'crop_makkink')
  
  
  
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
#' This function determines crop cover and Makkink correction factors based on the cultivated crops
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop, optional
#' @param effectivity (boolean) A vector that tells whether the catch crop was effective (i.e. did it grow sufficiently), optional
#'     
#' @export
calc_crop_rotation <- function(B_LU_BRP,M_GREEN = FALSE, effectivity = TRUE){ 
  
  crop_code = crop_name = crop_makkink = mcf = crop_cover = ode = NULL
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = 10)
  checkmate::assert_logical(effectivity,any.missing = FALSE, len = 10)
  
  
  # Collect data in a table
  dt <- data.table(year = 1:10, # check argument length
                   B_LU_BRP = B_LU_BRP,
                   effectivity = effectivity)
  
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
  year_cc <- unique(dt[M_GREEN == TRUE & effectivity == TRUE, year])
  year_cc_ne <- unique(dt[M_GREEN == TRUE & effectivity == FALSE, year])
  
  
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



