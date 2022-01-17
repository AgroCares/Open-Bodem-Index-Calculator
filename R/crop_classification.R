#' Determine classification rules for crops used to prepare crops.obic
#' 
#' This function determines crop classes given crop response to P, K and S fertilizers
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param nutrient (character) The nutrient for which crop classification is needed. Options include P, K and S.
#' 
#' @import data.table
#' 
#' @export
calc_cropclass <- function(B_LU_BRP,B_SOILTYPE_AGR, nutrient = NULL) {
  
  crop_category = crop_code = crop_name = soiltype = id = soiltype.n = NULL
  
  # Load data
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    id = 1:length(B_LU_BRP),
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    value = NA_character_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  setorder(dt, id)
  
  # lower case and character crop names
  dt[,crop_name := tolower(as.character(crop_name))]
  
  # general selection strings for all nutrients
  nat = 'bos|rand|griend|natuur|houtwal|onbeteeld|onbekend|struweel|oever|riet|rietzoom|wandel|wilg|brandn|cultuur|poel|bomenrij'
  bkw = 'bomen|peren|bloem|rozen|facelia|hop|lupine|kers|bramen|heesters|elzen|pruimen|zonne|conifer|kruid|vaste|heg|boom|wijn|bloemkwek|heesters|^appelen|buxus|bes'
  grs = 'grasland|raaigras|rietzwenkgras|veldbeemdgras'
  sms = 'mais|corncob|snijmai'
  
  # Determine crop classication for P ----
  if(nutrient == 'P') {

    # crop name selection strings for phosphorus
    cr0 = 'andijv|augurk|bleeksel|raap|paksoi|pastin|peterselie|selderij|^sla|snijbiet|spinaz|venkel|kroten|chinese kool'
    cr1 = 'aardappel|augurk|boon|erwt|bonen|knofl|koolrabi|knolselderij|peul|rammenas|spruitkool|uien|zaaiui'
    cr2 = 'suikerbiet|bieten, suiker|bieten, voeder|voederbiet|zaadbiet|vlas|karwij|raapsteel|radicchio|radijs'
    cr3 = 'bloembol|gladiool|krokus|amaryl|narcis|tulp|lelie|gerst|klaver|wikke|luzerne|cichorei|pastinaak'
    cr4 = 'granen|hennep|haver|sorghum|triticale|spelt|graszoden|tarwe|spitskool|rogge|grasza|witte kool|koolza|aardbei|asperg|biesl|witte kool|bloemko|boerenko|rodekool|savooi|broccol|bladkool|courget|koolra|kroot|pompo|prei|rabarber|schorse|sluitko'
    
    # distinghuish P sensitivity crop classes
    dt[grepl(grs,crop_name),value := 'gras']
    dt[grepl(sms,crop_name),value := 'mais'] 
    dt[grepl(cr0,crop_name),value := 'class0']
    dt[grepl(cr1,crop_name),value := 'class1']
    dt[grepl(cr2,crop_name),value := 'class2']
    dt[grepl(cr3,crop_name),value := 'class3']
    dt[grepl(cr4,crop_name),value := 'class4']
    dt[grepl(nat,crop_name),value := 'natuur'] 
    dt[grepl(bkw,crop_name),value := 'bkw']
    dt[grepl('witlof|peen',crop_name) & grepl('zand|dalgrond',soiltype.n),value := 'class0']
    
    # replace missing ones with low sensitivity
    dt[is.na(value),value :='class4']
  }
  
  
  # Determine crop classication for K ----
  if(nutrient == 'K') {
    
    # crop name selection strings for potassium on sandy soils and peat (zand, dekzand, dalgrond en veen)
    cr2 = 'aardappelen, poot|aardappelen, zetme|aardappelen, bestr|bloemkool'
    cr3 = 'bieten, voeder'
    cr1 = 'broccoli|cichorei|^sla|aardappelen, consumptie|bieten, suiker|rode bieten|klaver|wikke|luzerne|uien|spinazie|spruit|wortel|peen|prei|augurk|knols|schorse|aardbei|vlas|karwij|kool'  
    cr4 = 'raapzaad|asperges|tagetes|granen|hennep|haver|sorghum|triticale|spelt|graszoden|tarwe|rogge|grasza|gerst|mais|corncob|snijmai|bonen|boon|erwt'
   
    # distinghuish K sensitivity crop classes for grassland, nature and tree & bulbs
    dt[grepl(grs,crop_name), value := 'gras']
    dt[grepl(nat,crop_name), value := 'natuur'] 
    dt[grepl(bkw,crop_name), value := 'bkw']
    
    # distinghuish K sensitivity arable crop classes on sandy soils
    soilsel <- 'zand|dekzand|dalgrond|veen'
    dt[grepl(cr1,crop_name) & grepl(soilsel,soiltype.n),value := 'class1']
    dt[grepl(cr2,crop_name) & grepl(soilsel,soiltype.n),value := 'class2']
    dt[grepl(cr3,crop_name) & grepl(soilsel,soiltype.n),value := 'class3']
    dt[grepl(cr4,crop_name) & grepl(soilsel,soiltype.n),value := 'class4']
    
    # crop name selection strings for potassium on loess soils
    cr1 = 'broccoli|bol|cichorei|^sla|bieten, voeder|aardappelen, consumptie|bieten, suiker|rode bieten|klaver|wikke|luzerne|uien|spinazie|spruit|wortel|peen|prei|augurk|knols|schorse|aardbei|vlas|karwij|kool'  
    cr2 = 'aardappelen, poot|aardappelen, zetme|aardappelen, bestr|bloemkool|erwt|boon|bonen|luzerne|witlof|spruitkool' 
    cr3 = 'raapzaad|blauwmaan|asperges|tagetes|granen|hennep|haver|sorghum|triticale|spelt|graszoden|tarwe|rogge|grasza|gerst|mais|corncob|snijmai|zaad'
    
    # distinghuish K sensitivity arable crop classes on loess soils
    dt[grepl(cr1,crop_name) & soiltype == 'loess',value := 'class1']
    dt[grepl(cr2,crop_name) & soiltype == 'loess',value := 'class2']
    dt[grepl(cr3,crop_name) & soiltype == 'loess',value := 'class3']
    
    # crop name selection strings for potassium on clay soils
    cr1 = 'witte kool|rodekool|aardappelen, consumptie|^ui|uien|peen|prei|knolsel|augurk|schorsen|aardbei'
    cr2 = 'bieten, suiker|rode bieten|^vlas|vezelvlas|karwij|asperge'
    cr3 = 'luzerne|aardappelen, poot|aardappelen, zetme|aardappelen, bestr|bieten, voeder|peters|erwt|bonen|boon|klaver|wikke|luzene|witlof|bloemkool|spruitkool|bol'
    cr4 = 'raapzaad|blauwmaan|tagetes|granen|hennep|haver|sorghum|triticale|spelt|graszoden|tarwe|rogge|grasza|gerst|mais|corncob|snijmai|raapzaad|graszaad|koolzaad|kanarie|lijnzaad'
    cr5 = 'spinazie, productie'
    
    # distinghuish K sensitivity arable crop classes on clay soils
    dt[grepl(cr1,crop_name) & grepl('klei',soiltype.n), value := 'class1']
    dt[grepl(cr2,crop_name) & grepl('klei',soiltype.n), value := 'class2']
    dt[grepl(cr3,crop_name) & grepl('klei',soiltype.n), value := 'class3']
    dt[grepl(cr4,crop_name) & grepl('klei',soiltype.n), value := 'class4']
    dt[grepl(cr5,crop_name) & grepl('klei',soiltype.n), value := 'class5']
    
    # replace missing ones with low sensitivity
    dt[is.na(value), value := 'class4']
  }
  
  # Determine crop classication for S ----
  if(nutrient == 'S'){
    
    # crop name selection strings for sulphur
    cr1 ='spruitkool|sluitkool'
    cr2 = 'bloemkool|chinese kool|knolsel|koolz'
    cr3 = 'peen|aardappelen, zetme|aardappelen, bestr|aardappelen, consumptie|boerenkool|broccoli|granen|hennep|haver|sorghum|triticale|spelt|graszoden|tarwe|rogge|grasza|gerst|mais|corncob|snijmai|prei|uien|^ui|zaaiui|erwt|boon|bonen'
    cr4 = 'aardappelen, poot|^sla|bieten, suiker|vlas'
    
    # distinghuish K sensitivity crop classes for grassland, nature and tree & bulbs
    dt[grepl(grs,crop_name), value := 'gras']
    dt[grepl(nat,crop_name), value := 'natuur'] 
    dt[grepl(bkw,crop_name), value := 'bkw']
    
    # distinghuish S sensitivity arable crop classes 
    dt[grepl(cr1,crop_name), value := 'class1']
    dt[grepl(cr2,crop_name), value := 'class2']
    dt[grepl(cr3,crop_name), value := 'class3']
    dt[grepl(cr4,crop_name), value := 'class4']
    
    # replace missing ones with low sensitivity
    dt[is.na(value), value := 'class4']
  }
  
  setorder(dt, id)
  value <- dt[, value]
  
  # return classifiction
  return(value)
  
}

