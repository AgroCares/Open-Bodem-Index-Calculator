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
  
  
  carbon_input <- calc_carbon_input(ID, B_LU_BRP, A_P_AL, A_P_WA, M_GREEN, effective, manure_type, manure_in)
  
  rotation <- calc_crop_rotation(ID,B_LU_BRP,M_GREEN)
  
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
calc_carbon_input<- function(ID, B_LU_BRP, A_P_AL, A_P_WA, M_GREEN = FALSE, effective = TRUE, manure_type = 'slurry', manure_in = NULL){
  
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
  
  ##### Effectiveness of the catch crop?
  # C input from catch crop
  dt[M_GREEN == TRUE, catchcrop := 2800]
  dt[M_GREEN != TRUE, catchcrop := 0]
  
  # Correction for effectiveness of catch crop
  dt[effective == FALSE, catchcrop := 0.4 * catchcrop]
  
  ## Compost?
  
  
  # Format output
  out <- setorder(dt,year)
  out <- out[,list(year,manure_in,catchcrop)]
  
  return(out)
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
    

  # Standerdize months
  dt[,month:=1:120]
  
  # Format output
  out <- dt[,list(year, month, B_LU_BRP, mcf, crop_cover)]
  
  return(out)
}


#' Determine the carbon application events for current managment
#' 
#' This function determines the carbon application events for current management
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
calc_events_current <- function(ID, year, B_LU_BRP, manure_in, catchcrop){
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(year), length(M_GREEN), length(manure_in), length(catchcrop))
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = year,
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
  event.res <- melt(dt.residue,id.vars = "time", variable.name = "pool")
    
  
    
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
  event.manure <- melt(dt.manure,id.vars = "time", variable.name = "pool")
    
  
    
  ## Carbon from Compost ---
  # Make compost table
  dt.compost <- dt[,list(B_LU_BRP,year,t_manure,compost_in)]
  
  # Calculate carbon pools
  dt.compost[,c("CDPM","CRPM") := list(compost_in * 0.15 / (1 + 0.15), compost_in * 1 / (1 + 0.15))]
  
  # Moment of application
  dt.compost[,time := year - 1  + t_manure]
  dt.compost[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.compost <- dt.compost[,.(CDPM,CRPM,CHUM,time)]
  event.compost <- melt(dt.compost,id.vars = "time", variable.name = "pool")
  
    
  
  ## Carbon  from catch crops ---
  # Make catch crop table
  dt.catchcrop <- dt[,list(B_LU_BRP,year,t_manure,catchcrop)]
  
  # Calculate carbon pools
  dt.catchcrop[,c("CDPM","CRPM") := list(catchcrop * 1.37 / (1 + 1.37), catchcrop * 1 / (1 + 1.37))]
  
  # Moment of application
  dt.catchcrop[,time := year - 1  + t_manure]
  dt.catchcrop[B_LU_BRP == 233|B_LU_BRP == 235,time := year - 2 + t_manure]
  
  # Event for manure application
  dt.catchcrop <- dt.catchcrop[,.(CDPM,CRPM,CHUM,time)]
  event.catchcrop <- melt(dt.catchcrop,id.vars = "time", variable.name = "pool")
  
  
  # Event total application
  dt.event <- rbind(event.residue,event.manure,event.compost,event.catchcrop)
  setorder(dt.event,time)
  dt.event[,method := "add"]

  return(dt.event)
    
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
calc_events_minimal <- function(ID, year, B_LU_BRP, manure_in, catchcrop){
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(year), length(M_GREEN), length(manure_in), length(catchcrop))
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   year = year,
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
  dt.event <- melt(dt.residue,id.vars = "time", variable.name = "pool")
  
  
  setorder(dt.event,time)
  dt.event[,method := "add"]
  
  return(dt.event)
  }


