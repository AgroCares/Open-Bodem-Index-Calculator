#' Evaluate effects of measures 
#' 
#' This function quantifies the effects of 11 soil measures on the OBI score
#' 
#' @param dt.score (data.table) 
#' 
#' @import data.table
#' 
#' @export
obic_evalmeasure <- function(dt.score) {
  
  # Check inputs
  checkmate::assert_data_table(dt.score)
  
  ## Settings------------------------------------------------------------
  # Relative importance of 11 measures
  # (NOTE YF: these values can eventually be taken from the table)
  Prio_M <- rep(1, 11) # weight is 1 for all (= not priority)
  names(Prio_M) <- paste0("M", 1:11)
  
  # base of indicator variables
  nm <- c( "C_N","C_P","C_K","C_MG","C_S","C_PH","C_CEC","C_CU","C_ZN",
           "P_CR","P_SE","P_MS","P_BC","P_DU","P_CO","P_WRI","P_CEC",
           "B_DI","B_SF","B_SB")
  # name of indicator variables which are usred to calculate integrated scores (e.g. I_C_N)
  ind_nm <- paste0("I_", nm)
  
  # Threshold values of indicators (above which positive effects of measure is not considered)
  # Ordered as in 'nm'
  # (NOTE YF: these values can eventually be taken from the table)
  Dremp_value_C <- 0.6 # threshold value for chemical indicators
  Dremp_value_P <- 0.7 # threshold value for physical indicators
  Dremp_value_B <- 0.7 # threshold value for biological indicators
  Dremp_value <- c(rep(Dremp_value_C, 9), rep(Dremp_value_P, 8), rep(Dremp_value_B, 3))
  names(Dremp_value) <- nm
  
  ## Load in the datasets ------------------------------------------------
  # weighing factor to calculate scores
  w <- as.data.table(OBIC::weight.obic)
  w$rsid <- 1
  w <- dcast(w,rsid~var,value.var = 'weight')
  
  # effects of measures
  maatregel.obic <- as.data.table(OBIC::maatregel.obic)
  #load('C:/yuki_NMI/Open-Bodem-Index-Calculator/data/maatregel_obic.RData')
  
  # crop categories
  crops.obic <- as.data.table(OBIC::crops.obic)
  
  # soil categories
  soils.obic <- as.data.table(OBIC::soils.obic)
  
  
  # Merge soil and crop categories to dt.score ---------------------------------------
  # Make an additional category for loess
  soils.obic[soiltype == 'loess', soiltype.n := 'loess'] 
  setkey(soils.obic, soiltype)
  dt.score <- merge(dt.score, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # make a new column for crop category for evaluating measure effects
  crops.obic[grepl('grasland zonder herinzaai|grasland met herinzaai|natuur', crop_waterstress), crop_maatregel := "melkveehouderij"]
  crops.obic[grepl('granen|suikerbiet|aardappel|snijmais|overig', crop_waterstress), crop_maatregel := "akkerbouw"]
  crops.obic[grepl('bladgroenten|wintergroenten|zomergroenten|bloembollen', crop_waterstress), crop_maatregel := "groenten"]
  crops.obic[grepl('boomteelt|overig boomteelt|groot fruit|klein fruit', crop_waterstress), crop_maatregel := "boomtelt"]
  setkey(crops.obic, crop_code)
  dt.score <- merge(dt.score, crops.obic[, list(crop_code, crop_maatregel)], by.x = "B_LU_BRP", by.y = "crop_code")
  setkey(dt.score, ID)
  
  # Pre-processing of effect table -----------------------------------------
  # remove unnecessary rows
  maatregel.obic <- maatregel.obic[filter == 1]
  
  # convert '+' and '-' to points
  # TO DO: check if there is no other symbols used for Ef_M
  maatregel.obic[Ef_M == "+++", Ef_M_v := 3]
  maatregel.obic[Ef_M == "++", Ef_M_v := 2]
  maatregel.obic[Ef_M == "+", Ef_M_v := 1]
  maatregel.obic[Ef_M == "0", Ef_M_v := 0]
  maatregel.obic[Ef_M == "nr", Ef_M_v := 0]
  maatregel.obic[Ef_M == "-", Ef_M_v := -1]
  
  ## Calculation of scores of measures -----------------------------------------
  # Make a new datatable to store the effects of measures
  dt.recom <- copy(dt.score)
  
  # Compute, per record, if each indicator value is below the threshold value (e.g. 'Dremp_C_N', either 1 or 0)
  # The threshold values are indicator-specific
  dt.recom[, (paste0("Dremp_", nm)) := 0] # initialization
  for(j in nm){ # loop over all indicators
    col = paste0("I_", j)
    nr_blth <- which(dt.recom[,..col] < Dremp_value[j]) # row number in which indicator value is below the threshold
    set(dt.recom, i = nr_blth, j = paste0("Dremp_", j), value = 1)
  }
  
  
  for (m in 1:length(unique(maatregel.obic$maatregel_nr))){ # for each measure m
    # subset the effect table
    maatregel_sel <- subset(maatregel.obic,  maatregel_nr == m) 
    # drop the indicators which does not match the prescribed list 
    maatregel_sel<- maatregel_sel[grepl(paste0('^', ind_nm, '$', collapse = "|"), maatregel_sel$OBICvariable), ]
    # check if all needed indicator variables are included. 
    nmnr <- ind_nm[is.na(match(ind_nm, maatregel_sel$OBICvariable))] # name of missing indicators
    nrna <-  length(nmnr) # number of missings indicators
    
    # If some of the prescribed indicators are missing, fill their effect as 0
    if(nrna > 0){
      # show message on screen
      show(paste(nrna, "indicators are missing for measure", m, ':', paste(nmnr, collapse = ', ')))
      show( 'Their effects are now assumed to be 0.')
      # duplicate the first rows (initialization)
      maatregel_sel <- rbind(maatregel_sel, maatregel_sel[1:nrna, ])
      # fill NA first (except first 4 columns)
      maatregel_sel[c((nrow(maatregel_sel)-nrna+1):nrow(maatregel_sel)), c(5:ncol(maatregel_sel)) := NA]
      # fill missing indicator names
      maatregel_sel[c((nrow(maatregel_sel)-nrna+1):nrow(maatregel_sel)), OBICvariable :=  as.character(as.list(nmnr))]
      # fill 0 for effect
      colnm <- c("melkveehouderij..alleen.gras.","melkveehouderij.incl.mais.naast.gras","akkerbouw","groente", "boomteelt","klei","veen", "zand","loss")
      maatregel_sel[c((nrow(maatregel_sel)-nrna+1):nrow(maatregel_sel)), (colnm) := 0]
      maatregel_sel[c((nrow(maatregel_sel)-nrna+1):nrow(maatregel_sel)), Ef_M_v := 0]
    }
    
    # reorder the I_ variables (as in 'int_nm')
    maatregel_sel <- maatregel_sel[match(ind_nm, maatregel_sel$OBICvariable),]
    
    # Effect of the measure m based on crop type
    dt.recom[, (paste0("Ec_", nm)) := NA_real_] # initialization
    dt.recom[crop_maatregel == "melkveehouderij",
             (paste0("Ec_", nm)) := as.list(maatregel_sel$melkveehouderij..alleen.gras.)]
    # dt.recom[crop_maatregel == "melkveehouderij" & D_CP_MAIS < 0.2,
    #           (paste0("Ec_", nm)) := as.list(maatregel_sel$melkveehouderij..alleen.gras.)]
    # dt.recom[crop_maatregel == "melkveehouderij" & D_CP_MAIS >= 0.2,
    #           (paste0("Ec_", nm)) := as.list(maatregel_sel$melkveehouderij.incl.mais.naast.gras)]
    dt.recom[crop_maatregel == "akkerbouw",
             (paste0("Ec_", nm)) := as.list(maatregel_sel$akkerbouw)]
    dt.recom[crop_maatregel == "groenten",
             (paste0("Ec_", nm)) := as.list(maatregel_sel$groente)]
    dt.recom[crop_maatregel == "boomtelt",
             (paste0("Ec_", nm)) := as.list(maatregel_sel$boomteelt)]
    
    # Effect of the measure m based on soil type
    dt.recom[, (paste0("Es_", nm)) := NA_real_] # initialization
    dt.recom[soiltype.n == "klei", (paste0("Es_", nm)) := as.list(maatregel_sel$klei)]
    dt.recom[soiltype.n == "veen", (paste0("Es_", nm)) := as.list(maatregel_sel$veen)]
    dt.recom[soiltype.n == "zand", (paste0("Es_", nm)) := as.list(maatregel_sel$zand)]
    dt.recom[soiltype.n == "loess", (paste0("Es_", nm)) := as.list(maatregel_sel$loss)]
    
    # Combined effect based both on soil type and crop type (Take the smaller one)
    dt.recom[, (paste0("Rel_", nm)) := NA_real_] # initialization
    for(j in nm){ # loop over all indicators
      set(dt.recom, i = NULL, j = paste0("Rel_", j), 
          value = pmin(dt.recom[[paste0("Es_", j)]], dt.recom[[paste0("Ec_", j)]]))
    }
    
    ## separate positive and negative effects of the measure m
    # Positive effect (0 ~ +3) of the measure on each indicator, orderd as in 'nm'
    pEF_M <- ifelse(maatregel_sel$Ef_M_v >= 0, maatregel_sel$Ef_M_v, 0)
    names(pEF_M)<- nm
    # negative effect (-1) of the measure on each indicator, orderd as in 'nm'
    nEF_M <- ifelse(maatregel_sel$Ef_M_v < 0, maatregel_sel$Ef_M_v, 0)
    names(nEF_M)<- nm
    
    ## Chemical indicators ##
    # Calculate score of the measure for each chemical indicators
    dt.recom[, M_C_N := w$W_C_N * (Rel_C_N * pEF_M["C_N"] * Dremp_C_N + Rel_C_N * nEF_M["C_N"])]
    dt.recom[, M_C_P := w$W_C_P * (Rel_C_P * pEF_M["C_P"] * Dremp_C_P + Rel_C_P * nEF_M["C_P"])]
    dt.recom[, M_C_K := w$W_C_K * (Rel_C_K * pEF_M["C_K"] * Dremp_C_K + Rel_C_K * nEF_M["C_K"])]
    dt.recom[, M_C_MG := w$W_C_MG * (Rel_C_MG * pEF_M["C_MG"] * Dremp_C_MG + Rel_C_MG * nEF_M["C_MG"])]
    dt.recom[, M_C_S := w$W_C_S * (Rel_C_S * pEF_M["C_S"] * Dremp_C_S + Rel_C_S * nEF_M["C_S"])]
    dt.recom[, M_C_PH := w$W_C_PH * (Rel_C_PH * pEF_M["C_PH"] * Dremp_C_PH + Rel_C_PH * nEF_M["C_PH"])]
    dt.recom[, M_C_CEC := w$W_C_CEC * (Rel_C_CEC * pEF_M["C_CEC"] * Dremp_C_CEC + Rel_C_CEC * nEF_M["C_CEC"])]
    dt.recom[, M_C_CU := w$W_C_CU * (Rel_C_CU * pEF_M["C_CU"] * Dremp_C_CU + Rel_C_CU * nEF_M["C_CU"])]
    dt.recom[, M_C_ZN := w$W_C_ZN * (Rel_C_ZN * pEF_M["C_ZN"] * Dremp_C_ZN + Rel_C_ZN * nEF_M["C_ZN"])]
    
    # Calculate score of the measure for the chemical indicators, e.g. M1_S_C
    dt.recom[, (paste0("M", m, "_S_C")) := 	Prio_M[paste0('M', m)] * (
      M_C_N + M_C_P + M_C_K + M_C_MG + M_C_S + M_C_PH + M_C_CEC + M_C_CU + M_C_ZN)]
    
    
    ## Physical indicators ##
    # Calculate score of the measure for each physical indicators
    dt.recom[, M_P_CR :=  w$W_P_CR * (Rel_P_CR * pEF_M["P_CR"] * Dremp_P_CR + Rel_P_CR * nEF_M["P_CR"])]
    dt.recom[, M_P_SE :=  w$W_P_SE * (Rel_P_SE * pEF_M["P_SE"] * Dremp_P_SE + Rel_P_SE * nEF_M["P_SE"])]
    dt.recom[, M_P_MS :=  w$W_P_MS * (Rel_P_MS * pEF_M["P_MS"] * Dremp_P_MS + Rel_P_MS * nEF_M["P_MS"])] 
    dt.recom[, M_P_BC := w$W_P_BC * (Rel_P_BC * pEF_M["P_BC"] * Dremp_P_BC + Rel_P_BC * nEF_M["P_BC"])]
    dt.recom[, M_P_DU := w$W_P_DU * (Rel_P_DU * pEF_M["P_DU"] * Dremp_P_DU + Rel_P_DU * nEF_M["P_DU"])]
    dt.recom[, M_P_CO := w$W_P_CO * (Rel_P_CO * pEF_M["P_CO"] * Dremp_P_CO + Rel_P_CO * nEF_M["P_CO"])] 
    dt.recom[, M_P_CEC := w$W_P_CEC * (Rel_P_CEC * pEF_M["P_CEC"] * Dremp_P_CEC + Rel_P_CEC * nEF_M["P_CEC"])] 
    dt.recom[, M_P_WRI := w$W_P_WRI * (Rel_P_WRI * pEF_M["P_WRI"] * Dremp_P_WRI + Rel_P_WRI * nEF_M["P_WRI"])]
    
    # Calculate score of the measure for the physical indicators, e.g. M1_S_P
    dt.recom[, (paste0("M", m, "_S_P")) :=  Prio_M[paste0('M', m)] * (
      M_P_CR + M_P_SE + M_P_MS + M_P_BC + M_P_DU + M_P_CO + M_P_CEC + M_P_WRI)]
    
    ## Biological indicators ##
    # Calculate score of the measure for each biological indicators
    #dt.recom[, (paste0("M", m, "_S_B")) := Prio_M[paste0('M', m)] * ( 
    dt.recom[, M_B_DI :=  w$W_B_DI * (Rel_B_DI * pEF_M["B_DI"] * Dremp_B_DI + Rel_B_DI * nEF_M["B_DI"])]
    dt.recom[, M_B_SF :=  w$W_B_SF * (Rel_B_SF * pEF_M["B_SF"] * Dremp_B_SF + Rel_B_SF * nEF_M["B_SF"])]
    dt.recom[, M_B_SB :=  w$W_B_SB * (Rel_B_SB * pEF_M["B_SB"] * Dremp_B_SB + Rel_B_SB * nEF_M["B_SB"])]
    
    # Calculate score of the measure for the biological indicators, e.g. M1_S_P
    dt.recom[, (paste0("M", m, "_S_B")) := Prio_M[paste0('M', m)] * (M_B_DI + M_B_SF + M_B_SB)]
    
    
    # Save score of each indicator (by renaming the columns)
    # This part can be skipped if one does not want to add too many columns.
    setnames(dt.recom, 
             names(dt.recom)[grepl("M_", names(dt.recom))], 
             gsub("M_", paste0("M", m, "_"), names(dt.recom)[grepl("M_", names(dt.recom))]))
    
    # remove unnecessary columns
    dt.recom[, (c(paste0("Rel_", nm), paste0("Es_", nm), paste0("Ec_", nm))) := NULL]
  }
  
  return(dt.recom)
}


#' Recommend measurements for better soil managment
#' 
#' This function gives recommendations better soil managament based on the OBI score
#' 
#' @param dt.recom (data.table) 
#' @param extensive(Boolean) whether the output table includes evaluation scores of each  measures (TRUE) or only names of top 3 measures
#' 
#' @import data.table
#' 
#' @export
obic_recommendations <- function(dt.recom, extensive = FALSE) {
  
  # Check inputs
  checkmate::assert_data_table(dt.recom)
  
  RM_C_1 = RM_C_2 = RM_C_3 = RM_P_1 = RM_P_2 = RM_P_3 = RM_B_1 = RM_B_2 = RM_B_3 = NULL
  
  ## Chemical recommondations ------------------------------------------------  
  # Choose best 3 measures for chemical indicators
  # In case of ties, the one with most left (i.e. smallest maatregel_nr) is taken first.
  cols = colnames(dt.recom)[grepl("_S_C", colnames(dt.recom))]
  dt.recom[, first_C := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols)))], .SDcols = cols]
  dt.recom[, second_C := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 1))], .SDcols = cols]
  dt.recom[, third_C := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 2))], .SDcols = cols]
  
  # Store scores of the 1st, 2nd, and 3rd measures
  dt.recom[, first_C_score := dt.recom[[first_C]][.I], by=first_C]
  dt.recom[, second_C_score := dt.recom[[second_C]][.I], by=second_C]
  dt.recom[, third_C_score := dt.recom[[third_C]][.I], by=third_C]
  
  # Get measure names to recommend (e.g. "M3")
  dt.recom[, RM_C_1 := substr(first_C, 1, 2)]
  dt.recom[, RM_C_2 := substr(second_C, 1, 2)]
  dt.recom[, RM_C_3 := substr(third_C, 1, 2)]
  
  ## when the score of the selected measure is <=0, discard the advice 
  dt.recom[first_C_score <= 0, RM_C_1 := "no suitable advice"]
  dt.recom[second_C_score <= 0, RM_C_2 := "no suitable advice"]
  dt.recom[third_C_score <= 0, RM_C_3 := "no suitable advice"]
  
  ## when no indicator is below the threshold level, give no advice.
  # count number of indicators which are below threshold level
  cols = colnames(dt.recom)[grepl("Dremp_C_", colnames(dt.recom))]
  dt.recom[, nr_blth_C := rowSums(.SD), .SDcols = cols]
  dt.recom[nr_blth_C == 0, c("RM_C_1", "RM_C_2", "RM_C_3")  := "no advice needed"]
  
  
  ## Physical recommendations ------------------------------------------------
  # Choose best 3 measures for physical indicators
  # In case of ties, the one with most left (i.e. smallest maatregel_nr) is taken first.
  cols = colnames(dt.recom)[grepl("_S_P", colnames(dt.recom))]
  dt.recom[, first_P := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols)))], .SDcols = cols]
  dt.recom[, second_P := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 1))], .SDcols = cols]
  dt.recom[, third_P := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 2))], .SDcols = cols]
  
  # Store scores of the 1st, 2nd, and 3rd measures
  dt.recom[, first_P_score := dt.recom[[first_P]][.I], by=first_P]
  dt.recom[, second_P_score := dt.recom[[second_P]][.I], by=second_P]
  dt.recom[, third_P_score := dt.recom[[third_P]][.I], by=third_P]
  
  # Get measure names to recommend (e.g. "M3")
  dt.recom[, RM_P_1 := substr(first_P, 1, 2)]
  dt.recom[, RM_P_2 := substr(second_P, 1, 2)]
  dt.recom[, RM_P_3 := substr(third_P, 1, 2)]
  
  ## when the score of the selected measure is <=0, discard the advice 
  dt.recom[first_P_score <= 0, RM_P_1 := "no suitable advice"]
  dt.recom[second_P_score <= 0, RM_P_2 := "no suitable advice"]
  dt.recom[third_P_score <= 0, RM_P_3 := "no suitable advice"]
  
  ## when no indicator is below the threshold level, give no advice.
  # count number of indicators which are below threshold level
  cols = colnames(dt.recom)[grepl("Dremp_P_", colnames(dt.recom))]
  dt.recom[, nr_blth_P := rowSums(.SD), .SDcols = cols]
  dt.recom[nr_blth_P == 0, c("RM_P_1", "RM_P_2", "RM_P_3")  := "no advice needed"]
  
  ## Biological recommondations ----------------------------------------------
  # Choose best 3 measures for biological indicators
  # In case of ties, the one with most left (i.e. smallest maatregel_nr) is taken first.
  cols = colnames(dt.recom)[grepl("_S_B", colnames(dt.recom))]
  dt.recom[, first_B := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols)))], .SDcols = cols]
  dt.recom[, second_B := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 1))], .SDcols = cols]
  dt.recom[, third_B := colnames(.SD)[apply(.SD, 1, function(x) which(rank(x, ties.method = 'last') == length(cols) - 2))], .SDcols = cols]
  
  # Store scores of the 1st, 2nd, and 3rd measures
  dt.recom[, first_B_score := dt.recom[[first_B]][.I], by=first_B]
  dt.recom[, second_B_score := dt.recom[[second_B]][.I], by=second_B]
  dt.recom[, third_B_score := dt.recom[[third_B]][.I], by=third_B]
  
  # Get measure names to recommend (e.g. "M3")
  dt.recom[, RM_B_1 := substr(first_B, 1, 2)]
  dt.recom[, RM_B_2 := substr(second_B, 1, 2)]
  dt.recom[, RM_B_3 := substr(third_B, 1, 2)]
  
  ## when the score of the selected measure is <=0, discard the advice 
  dt.recom[first_B_score <= 0, RM_B_1 := "no suitable advice"]
  dt.recom[second_B_score <= 0, RM_B_2 := "no suitable advice"]
  dt.recom[third_B_score <= 0, RM_B_3 := "no suitable advice"]
  
  ## when no indicator is below the threshold level, give no advice.
  # count number of indicators which are below threshold level
  cols = colnames(dt.recom)[grepl("Dremp_B_", colnames(dt.recom))]
  dt.recom[, nr_blth_B := rowSums(.SD), .SDcols = cols]
  dt.recom[nr_blth_B == 0, c("RM_B_1", "RM_B_2", "RM_B_3")  := "no advice needed"]
  
  
  # remove unnecessary columns
  dt.recom[, (colnames(dt.recom)[grepl("first|second|third|Esc_|Dremp_|nr_blth_", colnames(dt.recom))]) := NULL]
  
  if(extensive == FALSE){
    # remove evaluation of each measure on each soil indicator (e.g. M1_)
    dt.recom[, (colnames(dt.recom)[grepl("^M", colnames(dt.recom))]) := NULL]
  }
  
  return(dt.recom)
  
}