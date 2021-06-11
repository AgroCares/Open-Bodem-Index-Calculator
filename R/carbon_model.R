#' Evaluate the carbon sequestration rate
#' 
#' This function is a wrapper function that calculates the score for carbon sequestration
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric)
#' @param A_P_WA (numeric)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'     
#' @export
ind_carbon_sequestration <- function(){
  
  
  
  # Check inputs
  
  
  carbon_input <- calc_carbon_input(ID,B_LU_BRP,A_P_AL,A_P_WA,M_GREEN,manure_type)
  
  
  
}



#' Calculate the carbon inputs on a field
#' 
#' This function calculates the carbon inputs to the field based on manure type, P status of the soil and management practices
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_P_AL (numeric)
#' @param A_P_WA (numeric)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param manure_type (character) The type of manure applied on the field, options: 'slurry' or 'solid'
#'     
#' @export
calc_carbon_input<- function(ID, B_LU_BRP, A_P_AL, A_P_WA, M_GREEN, manure_type){
  
  id = manure_in = manure_OC_Pration = slurry_OC_Pratio = crops.obic = NULL
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),  length(A_P_AL),length(A_P_WA), length(M_GREEN))
  
  
  # Collect data in a table
  dt <- data.table(ID = ID,
                   id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   A_P_AL = A_P_AL,
                   A_P_WA = A_P_WA,
                   M_GREEN = M_GREEN,
                   manure_type = manure_type
                   )
  
  # Import and merge with crops.obic
  crops.obic <- OBIC::crops.obic
  
  dt <- merge(dt, crops.obic, by.x = 'B_LU_BRP', by.y = 'crop_code')
  
  
  # C input from manure
  # Variate per year?
  
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

  
  
  ##### Effectiveness of the catch crop?
  # C input from catch crop
  dt[M_GREEN == TRUE, catchcrop := 850]
  dt[M_GREEN != TRUE, catchcrop := 0]
  
  # After cultivation of maize or potato, it is compulsory to plant a catch crop
  dt[grepl('Mais|Aardappel',crop_name), catchcrop := 850]
  
  
  ## Compost?
  
  
  # Format output
  out <- setorder(dt,id)
  
  
  return(out)
}