#' Calculate the 'performance' of sustainable soil management
#' 
#' This function evaluates the contribution of sustainable soil management following the Label Sustainable Soil Management.
#'  
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param D_SOM_BAL (numeric) The organic matter balance of the soil (in kg EOS per ha)
#' @param D_CP_POTATO (numeric) The fraction potato crops in crop rotation
#' @param D_CP_RUST (numeric) The fraction rustgewassen in crop rotation
#' @param D_CP_RUSTDEEP (numeric) The fraction diepe rustgewassen in crop rotation (-)
#' @param D_CP_GRASS (numeric) The fraction grassland in crop rotation
#' @param D_GA (numeric) The age of the grassland (years)
#' @param M_GREEN (boolean) measure. are catchcrops sown after main crop (option: yes or no)
#' @param M_NONBARE (boolean) measure. is parcel for 80 percent of the year cultivated and 'green' (option: yes or no)
#' @param M_EARLYCROP (boolean) measure. use of early crop varieties to avoid late harvesting (option: yes or no)
#' @param M_SLEEPHOSE (boolean) measure. is sleepslangbemester used for slurry application (option: yes or no)
#' @param M_DRAIN (boolean) measure. are under water drains installed in peaty soils (option: yes or no)
#' @param M_DITCH (boolean) measure. are ditched maintained carefully and slib applied on the land (option: yes or no)
#' @param M_UNDERSEED (boolean) measure. is maize grown with grass underseeded (option: yes or no)
#' @param M_LIME (boolean) measure. Has field been limed in last three years (option: yes or no)
#' @param M_NONINVTILL (boolean) measure. Non inversion tillage (option: yes or no)
#' @param M_SSPM (boolean) measure. Soil Structure Protection Measures, such as fixed driving lines, low pressure tires, and light weighted machinery (option: yes or no)
#' @param M_SOLIDMANURE (boolean) measure. Use of solid manure (option: yes or no)
#' @param M_STRAWRESIDUE (boolean) measure. Application of straw residues (option: yes or no)
#' @param M_MECHWEEDS (boolean) measure. Use of mechanical weed protection (option: yes or no)
#' @param M_PESTICIDES_DST (boolean) measure. Use of DST for pesticides (option: yes or no)
#' 
#' @import data.table
#' 
#' @export
calc_management <- function(A_SOM_LOI,B_LU_BRP, B_SOILTYPE_AGR,B_GWL_CLASS,
                            D_SOM_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                            M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN, M_DITCH, M_UNDERSEED,
                            M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST
                            ) {
  
  id = crop_code = crop_name = soiltype = soiltype.n = crop_n = crop_category = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(B_LU_BRP), length(B_SOILTYPE_AGR), length(D_SOM_BAL),length(B_GWL_CLASS),
                    length(D_CP_POTATO), length(D_CP_GRASS), length(D_CP_RUST), length(D_CP_RUSTDEEP),length(D_GA),
                    length(M_GREEN), length(M_NONBARE), length(M_EARLYCROP),length(M_SLEEPHOSE),length(M_DRAIN),
                    length(M_DITCH),length(M_UNDERSEED),
                    length(M_LIME), length(M_NONINVTILL), length(M_SSPM), length(M_SOLIDMANURE),length(M_STRAWRESIDUE),
                    length(M_MECHWEEDS),length(M_PESTICIDES_DST)
                    )
 
  # add checks Sven
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_NONBARE,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_EARLYCROP,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_SLEEPHOSE,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_DRAIN,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_DITCH,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_UNDERSEED,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_LIME,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_NONINVTILL,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_SSPM,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_SOLIDMANURE,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_STRAWRESIDUE,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_MECHWEEDS,any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_PESTICIDES_DST,any.missing = FALSE, len = arg.length)
  
  # Settings if needed
  gt_wet <- c('GtI','GtII','GtIIb','GtIII','GtIIIb') # wet soils with undeep groundwater table
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_SOM_LOI = A_SOM_LOI,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    D_SOM_BAL = D_SOM_BAL,
    D_CP_GRASS = D_CP_GRASS,
    D_CP_POTATO = D_CP_POTATO,
    D_CP_RUST = D_CP_RUST,
    D_CP_RUSTDEEP = D_CP_RUSTDEEP,
    D_GA = D_GA,
    B_GWL_CLASS = B_GWL_CLASS,
    M_GREEN = M_GREEN,
    M_NONBARE = M_NONBARE,
    M_EARLYCROP = M_EARLYCROP,
    M_SLEEPHOSE = M_SLEEPHOSE,
    M_DRAIN = M_DRAIN,
    M_DITCH = M_DITCH,
    M_UNDERSEED = M_UNDERSEED,
    M_LIME = M_LIME,
    M_NONINVTILL = M_NONINVTILL,
    M_SSPM = M_SSPM,
    M_SOLIDMANURE = M_SOLIDMANURE,
    M_STRAWRESIDUE = M_STRAWRESIDUE,
    M_MECHWEEDS = M_MECHWEEDS,
    M_PESTICIDES_DST = M_PESTICIDES_DST,
    value = 0
  )
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # ensure that crop name is in lower case (temporarily solution)
  dt[,crop_name := tolower(crop_name)]
  

  # evaluation of measures in an arable system -----
  
    # subset data.table
    dt.arable = dt[crop_n == 'akkerbouw' & crop_category != 'mais']
    
    # measure 1. is the parcel for 80% of the year grown by a crop (add 3 points)
    # calculate from fraction rustgewassen (suggestion of Wim)
    dt.arable[D_CP_RUST > 0.60, value := value + 3] 
    
    # measure 2. is minimal 40% of the deep rooting crops (add one point)
    dt.arable[D_CP_RUST > 0.4, value := value + 1]
    dt.arable[D_CP_RUST > 0.4 & D_CP_RUSTDEEP > 0.4, value := value + 1]
    
    # measure 3. crop rotation of potato is at minimum 1:4 (add two points)
    dt.arable[D_CP_POTATO > 0.15 & D_CP_POTATO <= 0.25, value := value + 2]
    
    # measure 4. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
    dt.arable[grepl('aardappel|bieten, suiker',crop_name) & M_EARLYCROP==TRUE, value := value + 1]
    
    # measure 7. add minus points when arable crops are grown on wet peat soils
    dt.arable[B_GWL_CLASS %in% gt_wet & soiltype.n == 'veen', value := value - 5]
    
    # measure 8. are soil protection measures taken?
    dt.arable[M_SSPM == TRUE, value := value + 1]
    
    # measure 9. are soils frequently limed?
    dt.arable[M_LIME == TRUE, value := value + 1]
    
    # measure 10. are soils receiving preferential solid manure above slurry
    dt.arable[M_SOLIDMANURE == TRUE, value := value + 1]
    
    # measure 11. is straw incorporated when straw residues are present
    dt.arable[M_STRAWRESIDUE == TRUE, value := value + 1]
    
    # measure 12. are weeds removed with mechanical machinery rather than chemicals
    dt.arable[M_MECHWEEDS == TRUE, value := value + 1]
    
    # measure 13. are pesticides used with decision supporting tools to minimize dose
    dt.arable[M_PESTICIDES_DST == TRUE, value := value + 1]
    
    # measure 14. add organic matter balance (and when positive, add one point)
    dt.arable[D_SOM_BAL > 0, value := value + 2]
    
    # negative scores may not occur
    dt.arable[value < 0, value := 0]
  
  # evaluation of meaures in maize system -----
  dt.maize = dt[crop_category == 'mais']
  
    # measure 1. maize crop in combination with grassland
    dt.maize[M_UNDERSEED == TRUE, value := value + 1]
    
    # measure 2. are catchcrops sown after main crop (alleen klei, zand en löss al verplicht)
    dt.maize[soiltype.n =='klei' & M_GREEN == TRUE, value := value + 1]
    
    # measure 3. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
    dt.maize[M_EARLYCROP==TRUE, value := value + 1]
    
    # measure 4. is heavy machinery avoided by slurry application
    dt.maize[soiltype.n !='veen' & M_SLEEPHOSE==TRUE, value := value + 1]
    
    # measure 5. add organic matter balance (and when positive, add one point)
    dt.maize[D_SOM_BAL > 0, value := value + 2]
    
    # measure 6. is clover used in grassland
    dt.maize[soiltype.n !='veen' & grepl('klaver',crop_name), value := value + 1]
    
    # measure 7. add minus points when arable crops are grown on wet peat soils
    dt.maize[B_GWL_CLASS %in% gt_wet & soiltype.n == 'veen', value := value - 5]
    
    # measure 8. are soil protection measures taken?
    dt.maize[M_SSPM == TRUE, value := value + 1]
    
    # measure 9. are soils frequently limed?
    dt.maize[M_LIME == TRUE, value := value + 1]
    
    # measure 10. are soils receiving preferential solid manure above slurry
    dt.maize[M_SOLIDMANURE == TRUE, value := value + 1]
    
    # measure 11. is straw incorporated when straw residues are present
    dt.maize[M_STRAWRESIDUE == TRUE, value := value + 1]
    
    # measure 12. are weeds removed with mechanical machinery rather than chemicals
    dt.maize[M_MECHWEEDS == TRUE, value := value + 1]
    
    # measure 13. is organic matter higher than 3%
    dt.maize[A_SOM_LOI < 3, value := value - 1]
    
    # negative scores may not occur
    dt.maize[value < 0, value := 0]
  
  # evaluation of measures in a grassland system -----
  
  # subset data.table
  dt.grass = dt[crop_n == 'gras']
  
    # measure 1. age of the grass
    dt.grass[D_GA > 3 & !B_GWL_CLASS %in% gt_wet, value := value + 1]
    dt.grass[D_CP_GRASS == 1 & !B_GWL_CLASS %in% gt_wet, value := value + 4]
    
    # measure 2. is clover used in grassland
    dt.grass[soiltype.n !='veen' & grepl('klaver',crop_name), value := value +1]
   
    # measure 3. permanent grass for GtI, GtII and GtIII
    dt.grass[D_GA > 8 & B_GWL_CLASS %in% gt_wet, value := value + 5]
    dt.grass[D_GA > 8 & B_GWL_CLASS %in% gt_wet & soiltype.n =='veen', value := value + 3]
    
    # measure 4. make use of under water drains
    dt.grass[soiltype.n =='veen' & M_DRAIN == TRUE, value := value + 3]
    
    # measure 5. clean ditches frequently 
    dt.grass[soiltype.n =='veen' & M_DITCH, value := value + 2]
    
    # measure 6. is heavy machinery avoided by slurry application
    dt.grass[M_SLEEPHOSE==TRUE, value := value + 1]
  
    # measure 7. are soils also receivingsolid manure 
    dt.grass[M_SOLIDMANURE == TRUE, value := value + 1]
    
    # measure 8. are soil protection measures taken?
    dt.grass[M_SSPM == TRUE, value := value + 1]
    
    # measure 9. are soils frequently limed?
    dt.grass[M_LIME == TRUE, value := value + 1]
    
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}


#' Calculate the indicator for sustainable management
#' 
#' This function calculates the the sustainability of strategic management options as calculated by \code{\link{calc_management}}
#' The main source of this indicator is developed for Label Duurzaam Bodembeheer (Van der Wal, 2016)
#' 
#' The current function allows a maximum score of 16 points for arable systems, 12 for maize 
#' and 9 for grass (non-peat) and 17 for grass on peat. 
#' 
#' @param D_MAN (numeric) The value of Sustainable Management  calculated by \code{\link{calc_management}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' 
#' @export
ind_management <- function(D_MAN,B_LU_BRP,B_SOILTYPE_AGR) {
  
  id = crop_code = soiltype = soiltype.n = crop_n = crop_name = crop_category = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(D_MAN), length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(D_MAN, lower = 0, upper = 17, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    D_MAN = D_MAN,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    value = NA_real_
  )
  
  # merge crop table with data.table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Evaluate the Sustainability of Soil Management
  dt[crop_n == 'gras', value := D_MAN / 9]
  dt[crop_n == 'gras' & soiltype.n == 'veen', value := D_MAN / 17]
  dt[crop_n == 'akkerbouw', value := D_MAN / 16]
  dt[crop_category == 'mais' & soiltype.n == 'zand', value := D_MAN / 11]
  dt[crop_category == 'mais' & soiltype.n == 'klei', value := D_MAN / 12]
  dt[crop_category == 'mais' & soiltype.n == 'veen', value := D_MAN / 9]
  
  # Ensure no vales above 1
  dt[value > 1, value := 1]
  
  # values exact zero not desired (due to non relevance zero in scoring method)
  dt[value==0, value := 0.05]
  
  # round value
  dt[,value := round(value,2)]
  
  # prepare output
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}
