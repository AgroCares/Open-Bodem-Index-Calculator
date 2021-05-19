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
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
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
                            M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN, M_DITCH, M_UNDERSEED,
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
                    length(D_CP_POTATO), length(D_CP_GRASS), length(D_CP_RUST), length(D_CP_RUSTDEEP),length(D_GA),length(M_COMPOST),
                    length(M_GREEN), length(M_NONBARE), length(M_EARLYCROP),length(M_SLEEPHOSE),length(M_DRAIN),
                    length(M_DITCH),length(M_UNDERSEED),
                    length(M_LIME), length(M_NONINVTILL), length(M_SSPM), length(M_SOLIDMANURE),length(M_STRAWRESIDUE),
                    length(M_MECHWEEDS),length(M_PESTICIDES_DST)
                    )
 
  # add checks Sven
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(M_COMPOST, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
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
    M_COMPOST = M_COMPOST,
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
    dt.arable = dt[crop_category == 'akkerbouw']
    
    # measure 1. is the parcel for 80% of the year grown by a crop (add 3 points)
    dt.arable[D_CP_RUST > 0.60 | M_NONBARE == TRUE, value := value + 3] 
    
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
    dt.arable[M_SSPM == TRUE | M_SLEEPHOSE == TRUE, value := value + 1]
    
    # measure 9. are soils frequently limed?
    dt.arable[M_LIME == TRUE, value := value + 1]
    
    # measure 10. are soils receiving preferential solid manure above slurry
    dt.arable[M_SOLIDMANURE == TRUE | M_COMPOST > 0 | M_GREEN == TRUE, value := value + 1]
    
    # measure 11. is straw incorporated when straw residues are present
    dt.arable[M_STRAWRESIDUE == TRUE, value := value + 1]
    
    # measure 12. are weeds removed with mechanical machinery rather than chemicals
    dt.arable[M_MECHWEEDS == TRUE, value := value + 1]
    
    # measure 13. are pesticides used with decision supporting tools to minimize dose
    dt.arable[M_PESTICIDES_DST == TRUE, value := value + 1]
    
    # measure 14. add organic matter balance (and when positive, add one point)
    dt.arable[D_SOM_BAL > 0, value := value + 2]
    
    # measure 15. is NKG applied to minimize tillage?
    dt.arable[M_NONINVTILL == TRUE, value := value + 1]
    
    # negative scores may not occur
    dt.arable[value < 0, value := 0]
  
  # evaluation of measures in maize system -----
  dt.maize = dt[crop_category == 'mais']
  
    # measure 1. maize crop in combination with grassland
    dt.maize[M_UNDERSEED == TRUE | M_NONBARE == TRUE, value := value + 1]
    
    # measure 2. are catchcrops sown after main crop (alleen klei, zand en löss al verplicht)
    dt.maize[soiltype.n =='klei' & M_GREEN == TRUE, value := value + 1]
    
    # measure 3. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
    dt.maize[M_EARLYCROP==TRUE, value := value + 1]
    
    # measure 4. is heavy machinery avoided by slurry application
    dt.maize[soiltype.n !='veen' & M_SLEEPHOSE==TRUE, value := value + 1]
    
    # measure 5. add organic matter balance (and when positive, add one point)
    dt.maize[D_SOM_BAL > 0, value := value + 2]
    
    # measure 6. add minus points when arable crops are grown on wet peat soils
    dt.maize[B_GWL_CLASS %in% gt_wet & soiltype.n == 'veen', value := value - 5]
    
    # measure 7. are soil protection measures taken?
    dt.maize[M_SSPM == TRUE, value := value + 1]
    
    # measure 8. are soils frequently limed?
    dt.maize[M_LIME == TRUE, value := value + 1]
    
    # measure 9. are soils receiving compost or preferential solid manure above slurry
    dt.maize[M_SOLIDMANURE == TRUE| M_COMPOST > 0, value := value + 1]
    
    # measure 10. is NKG applied to minimize tillage?
    dt.maize[M_NONINVTILL == TRUE, value := value + 1]
    
    # measure 11. is straw incorporated when straw residues are present
    dt.maize[M_STRAWRESIDUE == TRUE , value := value + 1]
    
    # measure 12. are weeds removed with mechanical machinery rather than chemicals
    dt.maize[M_MECHWEEDS == TRUE, value := value + 1]
    
    # measure 13. is organic matter higher than 3%
    dt.maize[A_SOM_LOI < 3, value := value - 1]
    
    # negative scores may not occur
    dt.maize[value < 0, value := 0]
  
  # evaluation of measures in a grassland system -----
  
  # subset data.table
  dt.grass = dt[crop_category == 'grasland']
  
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
  
    # measure 7. are soils also receiving solid manure or compost 
    dt.grass[M_SOLIDMANURE == TRUE | M_COMPOST == TRUE, value := value + 1]
    
    # measure 8. are soil protection measures taken?
    dt.grass[M_SSPM == TRUE, value := value + 1]
    
    # measure 9. are soils frequently limed?
    dt.grass[M_LIME == TRUE, value := value + 1]
    
    
  # subset data.table
  dt.nature = dt[crop_category == 'natuur']
  
    # measure 1. are soils also receiving solid manure or compost 
    dt.nature[M_SOLIDMANURE == TRUE | M_COMPOST > 0, value := value + 2]
    
    # measure 2. are soils 80% of the year green / cultivated 
    dt.nature[M_NONBARE == TRUE, value := value + 1] 
    
    # measure 3. are soil protection measures taken?
    dt.nature[M_SSPM == TRUE, value := value + 1]
    
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return Evaluation of Soil Management
  return(value)
}


#' Calculate the 'performance' of sustainable soil management given a required ecosystem service
#' 
#' This function evaluates the contribution of sustainable soil management for a given ecosystem service
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
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
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
#' @param type (character) type of ecosystem service to evaluate the impact of soil management. Options: I_M_SOILFERTILITY, I_M_CLIMATE, I_M_WATERQUALITY, and I_M_BIODIVERSITY
#' 
#' @import data.table
#' 
#' @export
calc_man_ess <- function(A_SOM_LOI,B_LU_BRP, B_SOILTYPE_AGR,B_GWL_CLASS,
                         D_SOM_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                         M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN, M_DITCH, M_UNDERSEED,
                         M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST,
                         type) {
  
  id = crop_code = crop_name = soiltype = soiltype.n = crop_n = crop_category = NULL
  measure = m_value = value_ms = fvalue = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  management.obic <- as.data.table(OBIC::management.obic)
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(B_LU_BRP), length(B_SOILTYPE_AGR), length(D_SOM_BAL),length(B_GWL_CLASS),
                    length(D_CP_POTATO), length(D_CP_GRASS), length(D_CP_RUST), length(D_CP_RUSTDEEP),length(D_GA),length(M_COMPOST),
                    length(M_GREEN), length(M_NONBARE), length(M_EARLYCROP),length(M_SLEEPHOSE),length(M_DRAIN),
                    length(M_DITCH),length(M_UNDERSEED),
                    length(M_LIME), length(M_NONINVTILL), length(M_SSPM), length(M_SOLIDMANURE),length(M_STRAWRESIDUE),
                    length(M_MECHWEEDS),length(M_PESTICIDES_DST)
  )
  
  # add checks Sven
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(M_COMPOST, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
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
  checkmate::assert_subset(type,c('I_M_SOILFERTILITY','I_M_CLIMATE','I_M_WATERQUALITY','I_M_BIODIVERSITY'))
  
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
    M_COMPOST = M_COMPOST,
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
    M_PESTICIDES_DST = M_PESTICIDES_DST
  )
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # ensure that crop name is in lower case (temporarily solution)
  dt[,crop_name := tolower(crop_name)]
  
  # estimate default management scores for arable systems regarding sustainable soil use in general
  
    # subset data.table
    dt.arable = dt[crop_category == 'akkerbouw']
    
    # add a column value
    dt.arable[,value := 0]
    
    # measure 1. is the parcel for 80% of the year grown by a crop (add 3 points)
    dt.arable[D_CP_RUST > 0.60 | M_NONBARE == TRUE, value := value + 3] 
    
    # measure 2. is minimal 40% of the deep rooting crops (add one point)
    dt.arable[D_CP_RUST > 0.4, value := value + 1]
    dt.arable[D_CP_RUST > 0.4 & D_CP_RUSTDEEP > 0.4, value := value + 1]
    
    # measure 3. crop rotation of potato is at minimum 1:4 (add two points)
    dt.arable[D_CP_POTATO <= 0.25, value := value + 2]
    
    # measure 7. add minus points when arable crops are grown on wet peat soils
    dt.arable[B_GWL_CLASS %in% gt_wet & soiltype.n == 'veen', value := value - 5]
    
    if(grepl('FERTILI|CLIMA',type)){
      
      # measure 14. add organic matter balance (and when positive, add one point)
      dt.arable[D_SOM_BAL > 0, value := value + 2]
  
    }
    
  
  # estimate management scores for arable systems given specific soil ecosystem functions
    
    if(nrow(dt.arable) > 0) {
      
      # melt data.table to join with management table
      
      # which columns are in id.vars
      cols <- c('id','crop_name','crop_category','M_COMPOST')
      
      # which columsn are in measure.vars
      colsm <- colnames(dt)[grepl('^M_',colnames(dt)) & !grepl('COMPOST',colnames(dt))]
      
      # melt the data.table
      dt.man <- melt(dt, id.vars = cols, 
                     measure.vars = colsm,
                     variable.name = 'measure', value.name = 'm_value')
      
      # merge with management.obic
      dt.man <- merge(dt.man,management.obic[,list(measure,type = get(type))], by = 'measure', all.x = TRUE)
      
      # subset data.table
      dt.arable.man = dt.man[crop_category == 'akkerbouw']
      
      # filter on the relevant measures
      dt.arable.man[type==0, m_value := NA]
      
      # dcast 
      dt.arable.man <- dcast(dt.arable.man,id + crop_name + crop_category + M_COMPOST ~ measure,value.var = 'm_value')
      
      # management specific score
      dt.arable.man[, value_ms := 0]
      
      # measure 4. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
      dt.arable.man[grepl('aardappel|bieten, suiker',crop_name) & M_EARLYCROP == TRUE, value_ms := value_ms + 1]
      
      # measure 8. are soil protection measures taken?
      dt.arable.man[M_SSPM == TRUE | M_SLEEPHOSE == TRUE, value_ms := value_ms + 1]
      
      # measure 9. are soils frequently limed?
      dt.arable.man[M_LIME == TRUE, value_ms := value_ms + 1]
      
      # measure 10. are soils receiving preferential solid manure above slurry
      dt.arable.man[M_SOLIDMANURE == TRUE | M_COMPOST > 0 | M_GREEN ==TRUE, value_ms := value_ms + 1]
      
      # measure 11. is straw incorporated when straw residues are present
      dt.arable.man[M_STRAWRESIDUE == TRUE, value_ms := value_ms + 1]
      
      # measure 12. are weeds removed with mechanical machinery rather than chemicals
      dt.arable.man[M_MECHWEEDS == TRUE, value_ms := value_ms + 1]
      
      # measure 13. are pesticides used with decision supporting tools to minimize dose
      dt.arable.man[M_PESTICIDES_DST == TRUE, value_ms := value_ms + 1]
      
      # measure 15. is NKG applied to minimize tillage?
      dt.arable.man[M_NONINVTILL == TRUE, value_ms := value_ms + 1]
      
      # merge with baseline for carbon
      dt.arable <- merge(dt.arable,dt.arable.man[,list(id,value_ms)],by='id')
      
      # add final value for arable systems
      dt.arable[,fvalue := value + value_ms]
      
    } else {
      
      # if no arable crops are present, add column fvalue
      dt.arable[,fvalue := value]
    }
    
  
  # negative scores may not occur for default
  dt.arable[fvalue < 0, fvalue := 0]
  
  
  # evaluation of measures in maize system -----
  
    # estimate default management scores for arable systems regarding sustainable soil use in general
  
      # subset data.table
      dt.maize = dt[crop_category == 'mais']
  
      # add a column value
      dt.maize[,value := 0]
  
      # measure 6. add minus points when arable crops are grown on wet peat soils
      dt.maize[B_GWL_CLASS %in% gt_wet & soiltype.n == 'veen', value := value - 5]
      
      # measure 13. is organic matter higher than 3%
      dt.maize[A_SOM_LOI < 3, value := value - 1]
      
      if(grepl('FERTILI|CLIMA',type)){
        
        # measure 5. add organic matter balance (and when positive, add one point)
        dt.maize[D_SOM_BAL > 0, value := value + 2]
        
      }
  
    # estimate management scores for maize systems given specific soil ecosystem functions
    if(nrow(dt.maize) > 0){
      
      # melt data.table to join with management table
      
      # which columns are in id.vars
      cols <- c('id','crop_category','soiltype.n','M_COMPOST')
      
      # which columsn are in measure.vars
      colsm <- colnames(dt)[grepl('^M_',colnames(dt)) & !grepl('COMPOST',colnames(dt))]
      
      # melt the data.table
      dt.man <- melt(dt, id.vars = cols, 
                     measure.vars = colsm,
                     variable.name = 'measure', value.name = 'm_value')
      
      # merge with management.obic
      dt.man <- merge(dt.man,management.obic[,list(measure,type = get(type))], by = 'measure', all.x = TRUE)
      
      # subset data.table
      dt.maize.man = dt.man[crop_category == 'mais']
      
      # filter on the relevant measures
      dt.maize.man[type==0, m_value := NA]
      
      # dcast 
      dt.maize.man <- dcast(dt.maize.man,id + crop_category + soiltype.n + M_COMPOST ~ measure,value.var = 'm_value')
      
      # management specific score
      dt.maize.man[, value_ms := 0]
      
      # evaluate the impact of measures
      
      # measure 1. maize crop in combination with grassland
      dt.maize.man[M_UNDERSEED == TRUE | M_NONBARE == TRUE, value_ms := value_ms + 1]
      
      # measure 2. are catchcrops sown after main crop (alleen klei, zand en löss al verplicht)
      dt.maize.man[soiltype.n =='klei' & M_GREEN == TRUE, value_ms := value_ms + 1]
      
      # measure 3. use of early varieties in relevant cultures to avoid harvesting after september (stimulating catch crop too)
      dt.maize.man[M_EARLYCROP==TRUE, value_ms := value_ms + 1]
      
      # measure 4. is heavy machinery avoided by slurry application
      dt.maize.man[soiltype.n !='veen' & M_SLEEPHOSE==TRUE, value_ms := value_ms + 1]
      
      # measure 7. are soil protection measures taken?
      dt.maize.man[M_SSPM == TRUE, value_ms := value_ms + 1]
      
      # measure 8. are soils frequently limed?
      dt.maize.man[M_LIME == TRUE, value_ms := value_ms + 1]
      
      # measure 9. are soils receiving compost or preferential solid manure above slurry
      dt.maize.man[M_SOLIDMANURE == TRUE| M_COMPOST > 0, value_ms := value_ms + 1]
      
      # measure 10. is NKG applied to minimize tillage?
      dt.maize.man[M_NONINVTILL == TRUE, value_ms := value_ms + 1]
      
      # measure 11. is straw incorporated when straw residues are present
      dt.maize.man[M_STRAWRESIDUE == TRUE, value_ms := value_ms + 1]
      
      # measure 12. are weeds removed with mechanical machinery rather than chemicals
      dt.maize.man[M_MECHWEEDS == TRUE, value_ms := value_ms + 1]
      
      # merge with baseline for carbon
      dt.maize <- merge(dt.maize,dt.maize.man[,list(id,value_ms)],by='id')
      
      # add final value for arable systems
      dt.maize[,fvalue := value + value_ms]
      
    }  else {
      
      # if no maize present, add column fvalue
      dt.maize[,fvalue := value]
    }
    
    
    # negative scores may not occur for default
    dt.maize[fvalue < 0, fvalue := 0]
 
  
  # evaluation of measures in a grassland system -----
  
    # estimate default management scores for grassland systems regarding sustainable soil use in general
    
      # subset data.table
      dt.grass = dt[crop_category == 'grasland']
      
      # add a column value
      dt.grass[,value := 0]
      
      # measure 1. age of the grass
      dt.grass[D_GA > 3 & !B_GWL_CLASS %in% gt_wet, value := value + 1]
      dt.grass[D_CP_GRASS == 1 & !B_GWL_CLASS %in% gt_wet, value := value + 4]
      
      # measure 2. is clover used in grassland
      dt.grass[soiltype.n !='veen' & grepl('klaver',crop_name), value := value +1]
      
      # measure 3. permanent grass for GtI, GtII and GtIII
      dt.grass[(D_GA > 8 | D_CP_GRASS == 1) & B_GWL_CLASS %in% gt_wet, value := value + 5]
      dt.grass[(D_GA > 8 | D_CP_GRASS == 1) & B_GWL_CLASS %in% gt_wet & soiltype.n =='veen', value := value + 3]
    
   # estimate management scores for maize systems given specific soil ecosystem functions
    
    if(nrow(dt.grass) > 0){
    
      # melt data.table to join with management table
      
      # which columns are in id.vars
      cols <- c('id','soiltype.n','crop_category','M_COMPOST')
      
      # which columsn are in measure.vars
      colsm <- colnames(dt)[grepl('^M_',colnames(dt)) & !grepl('COMPOST',colnames(dt))]
      
      # melt the data.table
      dt.man <- melt(dt, id.vars = cols, 
                     measure.vars = colsm,
                     variable.name = 'measure', value.name = 'm_value')
      
      # merge with management.obic
      dt.man <- merge(dt.man,management.obic[,list(measure,type = get(type))], by = 'measure', all.x = TRUE)
      
      # subset data.table
      dt.grass.man = dt.man[crop_category == 'grasland']
      
      # filter on the relevant measures
      dt.grass.man[type==0, m_value := NA]
      
      # dcast 
      dt.grass.man <- dcast(dt.grass.man,id + soiltype.n + crop_category + M_COMPOST ~ measure,value.var = 'm_value')
      
      # management specific score
      dt.grass.man[, value_ms := 0]
      
      # evaluate the impact of measures specific for an ecosystem service
      
      # measure 4. make use of under water drains
      dt.grass.man[soiltype.n =='veen' & M_DRAIN == TRUE, value_ms := value_ms + 3]
      
      # measure 5. clean ditches frequently 
      dt.grass.man[soiltype.n =='veen' & M_DITCH, value_ms := value_ms + 2]
      
      # measure 6. is heavy machinery avoided by slurry application
      dt.grass.man[M_SLEEPHOSE==TRUE, value_ms := value_ms + 1]
      
      # measure 7. are soils also receiving solid manure or compost 
      dt.grass.man[M_SOLIDMANURE == TRUE | M_COMPOST == TRUE, value_ms := value_ms + 1]
      
      # measure 8. are soil protection measures taken?
      dt.grass.man[M_SSPM == TRUE, value_ms := value_ms + 1]
      
      # measure 9. are soils frequently limed?
      dt.grass.man[M_LIME == TRUE, value_ms := value_ms + 1]
      
      # merge with baseline for carbon
      dt.grass <- merge(dt.grass,dt.grass.man[,list(id,value_ms)],by='id')
      
      # add final value for arable systems
      dt.grass[,fvalue := value + value_ms]
      
    } else{
      
      # if no grassland is present, add column fvalue
      dt.grass[,fvalue := value]
      
    }
    
    
    # negative scores may not occur for default
    dt.grass[fvalue < 0, fvalue := 0]
      
      
  # subset data.table
  dt.nature = dt[crop_category == 'natuur']
  
  # add value to count relevance and impact of measures
  dt.nature[,value := 0]
  
  # measure 1. are soils also receiving solid manure or compost 
  dt.nature[M_SOLIDMANURE == TRUE | M_COMPOST > 0, value := value + 2]
  
  # measure 2. are soils 80% of the year green / cultivated 
  dt.nature[M_NONBARE == TRUE, value := value + 1] 
  
  # measure 3. are soil protection measures taken?
  dt.nature[M_SSPM == TRUE, value := value + 1]
  
  # add final value for nature (not specified for an ecosystem service)
  dt.nature[,fvalue := value]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable,dt.maize,dt.nature), fill = TRUE)
  
  # sorder output op id
  setorder(dt, id)
  
  # extract value
  value <- dt[, fvalue]
  
  # return Evaluation of Soil Management for a given Ecosystem Service
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
  
  # add visible bindings
  id = crop_code = soiltype = soiltype.n = crop_n = crop_name = crop_category = NULL
  variable = weight_nonpeat = weight_peat = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # load weights.obic (set indicator to zero when not applicable)
  w <- as.data.table(OBIC::weight.obic)
  w <- w[grepl('^M_',variable)]
  
  # Check inputs
  arg.length <- max(length(D_MAN), length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(D_MAN, lower = 0, upper = 18, any.missing = FALSE)
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
  
  # find maximum score
  w.max <- w[, list(weight_nonpeat = sum(weight_nonpeat[weight_nonpeat>0]),
                    weight_peat = sum(weight_peat[weight_peat>0])), by = 'crop_category']
  
  # add default scores 
  # due to i) corrections due to OR-OR statements, ii) points for crop rotation plan/clover, and iii) extra points (>+1 )  (see calc_management)
  w.max[crop_category=='akkerbouw',weight_nonpeat := weight_nonpeat - 2 + 6 + 2]
  w.max[crop_category=='akkerbouw',weight_peat := weight_peat - 2 + 6 + 2]
  w.max[crop_category=='mais', weight_nonpeat := weight_nonpeat - 2 + 2 + 0]
  w.max[crop_category=='mais', weight_peat := weight_peat - 2 + 2 + 0]
  w.max[crop_category=='grasland', weight_nonpeat := weight_nonpeat - 1 + 6 + 0]
  w.max[crop_category=='grasland', weight_peat := weight_peat - 1 + 8 + 3]
  
  # merge max score to actual dt
  dt <- merge(dt, w.max, by='crop_category')
  
  # evaluate the Sustainability of Soil Management
  dt[, value := D_MAN / fifelse(B_SOILTYPE_AGR == 'veen',weight_peat,weight_nonpeat)]
  
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

#' Calculate the indicator for sustainable management given a required ecoystem service
#' 
#' This function calculates the the sustainability of strategic management options for a given ecoystem service as calculated by \code{\link{calc_man_ess}}
#' The main source of this indicator is developed for Label Duurzaam Bodembeheer (Van der Wal, 2016)
#' 
#' @param D_MAN (numeric) The value of Sustainable Management calculated by \code{\link{calc_man_ess}}
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param type (character) type of ecosystem service to evaluate the impact of soil management. Options: I_M_SOILFERTILITY, I_M_CLIMATE, I_M_WATERQUALITY, and I_M_BIODIVERSITY
#' 
#' @export
ind_man_ess <- function(D_MAN,B_LU_BRP,B_SOILTYPE_AGR,type) {
  
  # add visible bindings
  id = crop_code = soiltype = soiltype.n = crop_n = crop_name = crop_category = NULL
  variable = weight_nonpeat = weight_peat = NULL
  ortype = meas_group = value_peat = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # load weights.obic (set indicator to zero when not applicable)
  w <- as.data.table(OBIC::weight.obic)
  w <- w[grepl('^M_',variable)]
  
  # load management measure table
  man.obic <- as.data.table(OBIC::management.obic)
  man.obic <- melt(man.obic,
                   id.vars = c('measure'),
                   measure.vars = c('I_M_SOILFERTILITY','I_M_CLIMATE','I_M_WATERQUALITY','I_M_BIODIVERSITY'),
                   variable.name = 'meas_group', value.name = 'value')
  
  w2 <- merge(w,man.obic,by.x = 'variable', by.y = 'measure',allow.cartesian = TRUE)
  w2 <- w2[value > 0]
  w2[, value := as.numeric(value)]
  
  # add corrections for OR-OR counting in calc_man_ess
  w2[,ortype := 0]
  w2[grepl('COMPOST|GREEN|SOLID',variable) & crop_category=='akkerbouw', ortype := 1]
  w2[grepl('SLEEP|SSPM',variable) & crop_category=='akkerbouw', ortype := 2]
  w2[grepl('COMPOST|SOLID',variable) & crop_category != 'akkerbouw', ortype := 3]
  w2[grepl('UNDERSEED|NONBARE',variable) & crop_category=='mais', ortype := 4]
  w2[, value := value / max(1,sum(value[ortype > 0])),by = c('crop_category','meas_group','ortype')]
  
  # add extra points for measures that have always more than +1 impact
  w2[,value_peat := value]
  w2[grepl('DRAIN',variable) & crop_category == 'grasland', value_peat := value + 2]
  w2[grepl('DITCH',variable) & crop_category == 'grasland', value_peat := value + 1]
  w2[crop_category=='akkerbouw' & grepl('NONBARE',variable), value := value + 2]
  w2[crop_category=='natuur' & grepl('MANURE',variable), value := value + 1]
  
  # add extra points for measures that have more than +1 impact peat soil
  w2[, weight_peat := weight_peat * value_peat]
  w2[, weight_nonpeat := weight_nonpeat * value]
  
  
  # Check inputs
  arg.length <- max(length(D_MAN), length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(D_MAN, lower = 0, upper = 18, any.missing = FALSE)
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
  
  # find maximum score per measure-soil-land use combination
  w.max <- w2[, list(weight_nonpeat = sum(weight_nonpeat[weight_nonpeat>0]),
                     weight_peat = sum(weight_peat[weight_peat>0])), by = c('crop_category','meas_group')]
  
  # add extra points due to general crop rotation and soil properties as well as SOM balance for non peat soils
  w.max[grepl('FERTILI|CLIMA',meas_group) & grepl('mais|akkerb',crop_category), weight_nonpeat := weight_nonpeat + 2]
  w.max[crop_category=='akkerbouw', weight_nonpeat := weight_nonpeat + 4]
  w.max[crop_category=='grasland', weight_nonpeat := weight_nonpeat + 2]
  w.max[crop_category=='grasland', weight_nonpeat := weight_nonpeat + 7]
  w.max[crop_category=='grasland', weight_nonpeat := weight_nonpeat + 4]
  
  # add extra points due to general crop rotation and soil properties as well as SOM balance for non peat soils
  w.max[grepl('FERTILI|CLIMA',meas_group) & grepl('mais|akkerb',crop_category), weight_peat := weight_peat + 2]
  w.max[crop_category=='akkerbouw', weight_peat := weight_peat + 4]
  w.max[crop_category=='grasland', weight_peat := weight_peat + 2]
  w.max[crop_category=='grasland', weight_peat := weight_peat + 7]
  w.max[crop_category=='grasland', weight_peat := weight_peat + 4]
  
  # filter w.max for the relevant ecosystem service
  w.max <- w.max[meas_group==type]
  
  # merge max score to actual dt
  dt <- merge(dt, w.max, by='crop_category')
  
  # evaluate the Sustainability of Soil Management
  dt[, value := D_MAN / fifelse(B_SOILTYPE_AGR == 'veen',weight_peat,weight_nonpeat)]
  
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