#' Estimate default values for management 
#' 
#' This function adds default management input variables given soil type and land use
#' 
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param M_NONBARE (boolean) A soil measure. Is parcel for 80 percent of the year cultivated and 'green' (optional, option: yes or no)
#' @param M_EARLYCROP (boolean) A soil measure. Use of early crop varieties to avoid late harvesting (optional, option: yes or no)
#' @param M_SLEEPHOSE (boolean) A soil measure. Is sleephose used for slurry application (optional, option: yes or no)
#' @param M_DRAIN (boolean) A soil measure. Are under water drains installed in peaty soils (optional, option: yes or no)
#' @param M_DITCH (boolean) A soil measure. Are ditched maintained carefully and slib applied on the land (optional, option: yes or no)
#' @param M_UNDERSEED (boolean) A soil measure. Is grass used as second crop in between maize rows (optional, option: yes or no) 
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
#' @examples 
#' add_management(ID = 1, B_LU_BRP = 256, B_SOILTYPE_AGR = 'dekzand')
#' add_management(ID = 1, B_LU_BRP = c(256,1019), B_SOILTYPE_AGR = rep('dekzand',2))
#' 
#' @returns 
#' A data.table with all default estimates for the management measures that are used for the Label Sustainable Soil Management.
#' For each B_LU_BRP 15 management measures are given, all as boolean variables except for M_COMPOST being a numeric value.
#' 
#' @export
add_management <- function(ID,B_LU_BRP, B_SOILTYPE_AGR,
                           M_GREEN = NA, M_NONBARE = NA, M_EARLYCROP = NA, M_COMPOST = NA_real_,
                           M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
                           M_LIME = NA, M_NONINVTILL = NA, M_SSPM = NA, M_SOLIDMANURE = NA,
                           M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA){
  
  # add visual bindings
  crop_code = crop_category = soiltype = soiltype.n = id = . = NULL
  
  # Check input
  arg.length <- max(length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   M_GREEN = M_GREEN,
                   M_NONBARE = M_NONBARE, 
                   M_EARLYCROP = M_EARLYCROP,
                   M_COMPOST = M_COMPOST,
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
  

  # add categories for crop type
  crops.obic <- as.data.table(OBIC::crops.obic)
  dt <- merge(dt, crops.obic[, .(crop_code, crop_category)],by.x = "B_LU_BRP", by.y = "crop_code",all.x=TRUE)
  
  # add rougher categories of soil type
  soils.obic <- as.data.table(OBIC::soils.obic)
  dt <- merge(dt, soils.obic[, .(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype",all.x=TRUE)
  
  # set measures to defaults
  
    # compost dose
    dt[is.na(M_COMPOST), M_COMPOST := 0]
  
    # early varieties to avoid late harvesting
    dt[is.na(M_EARLYCROP),M_EARLYCROP := TRUE]
    dt[grepl("klei|veen",soiltype.n),M_EARLYCROP := FALSE]
    dt[crop_category %in% c("grasland","natuur"), M_EARLYCROP := FALSE]
    
    # sleephose for slurry application
    dt[is.na(M_SLEEPHOSE), M_SLEEPHOSE := FALSE]
    dt[grepl("klei|veen",soiltype.n) & grepl('gras',crop_category), M_SLEEPHOSE := TRUE]
    
    # under water drains in peaty soils
    dt[is.na(M_DRAIN), M_DRAIN := FALSE]
    
    # are ditched maintained carefully and slib applied on the land (option: yes or no)
    dt[is.na(M_DITCH), M_DITCH := TRUE] 
    
    # is grass used as crop in between maize rows (option: yes or no)
    dt[is.na(M_UNDERSEED), M_UNDERSEED := FALSE]
    dt[crop_category == "mais" & soiltype.n == "zand", M_UNDERSEED := TRUE]
    
    # is there green manure used and soil non-bare
    dt[is.na(M_GREEN), M_GREEN := TRUE]
    dt[is.na(M_NONBARE), M_NONBARE := FALSE]
    
    # is the soil limed in last three years
    dt[is.na(M_LIME), M_LIME := TRUE]
    dt[crop_category %in% c("grasland","natuur","mais"), M_LIME := TRUE]
    
    # is the soil ploughed minimally (NKG)
    dt[is.na(M_NONINVTILL), M_NONINVTILL := FALSE]
    
    # is the soil structure prevented with measures avoiding compaction?
    dt[is.na(M_SSPM), M_SSPM := FALSE]
    
    # is the soil fertilized with solid manure preferentially above slurry or fertilizer
    dt[is.na(M_SOLIDMANURE), M_SOLIDMANURE := TRUE]
    
    # are weeds preferentially removed by mechanical machinery (rather than pesticides)
    dt[is.na(M_MECHWEEDS), M_MECHWEEDS := FALSE]
    
    # are straw residues incorporated in crops where residues are available
    dt[is.na(M_STRAWRESIDUE), M_STRAWRESIDUE := TRUE]
    dt[crop_category %in% c("grasland","natuur"), M_LIME := M_STRAWRESIDUE]
    
    # use of DST for pesticides to minimize pesticide use
    dt[is.na(M_PESTICIDES_DST), M_PESTICIDES_DST := FALSE]
    
  # setorder
  setorder(dt,id)
  
  # select relevant output
  out <- dt[,.(M_GREEN, M_NONBARE, M_EARLYCROP,M_COMPOST,M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED,
               M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE,M_MECHWEEDS,M_PESTICIDES_DST)]
  
  # return
  return(out)
}
