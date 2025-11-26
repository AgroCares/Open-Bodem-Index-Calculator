#' Calculate groundwater recharge of a soil
#' 
#' This function calculates an index score for groundwater storage based on precipitation surplus, infiltration at saturation, sealing risk, drainage and subsoil compaction
#' 
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_psp}}
#' @param D_WRI_K (numeric) The value for top soil permeability (cm/d) as calculated by \code{\link{calc_permeability}}
#' @param I_P_SE (numeric) The indicator value for soil sealing (deprecated)
#' @param I_P_CO (numeric) The indicator value for occurrence of subsoil compaction (deprecated)
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param D_SE (numeric) The value of soil sealing calculated by \code{\link{calc_sealing_risk}}
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' @param D_P_CO (numeric) Compaction value based on visual soil assessment (see Details)
#' 
#' @examples 
#' ind_gw_recharge(B_LU_BRP = 265,D_PSP = 200, D_WRI_K = 10, I_P_SE = 0.6, I_P_CO = 0.9, 
#' B_DRAIN = FALSE, B_GWL_CLASS = 'V')
#' ind_gw_recharge(B_LU_BRP = 233, D_PSP = 400, D_WRI_K = 10, I_P_SE = 0.4, I_P_CO = 0.2, 
#' B_DRAIN = TRUE, B_GWL_CLASS = 'II')
#' 
#' @details
#' Soil compaction risk can be assessed either with the soilcompaction risk map 
#' (B_SC_WENR) or visual soil assessment (VSA) (D_P_CO). D_P_CO itself is calculated 
#' from the VSA scoring of earthworms, compaction, rooting depth, and puddling:
#' D_P_CO = (3 x A_EW_BCS + 3 x A_SC_BCS + 3 x A_RD_BCS  - 2 x A_P_BCS - A_RT_BCS)/18 where
#' the .*_BCS variables have the values 0, 1, or 2. When both B_SC_WENR and D_P_CO
#' are provided, D_P_CO is used as it is considered more accurate determination of compaction.
#' 
#'
#' @return 
#' The evaluated score for the soil function to improve groundwater recharge. A numeric value between 0 and 1.
#'          
#' @export
ind_gw_recharge <- function(B_LU_BRP, D_PSP, D_WRI_K, I_P_SE = NULL, I_P_CO = NULL,
                            B_DRAIN, B_GWL_CLASS, D_SE = NULL, B_SC_WENR = NULL,
                            D_P_CO = NULL){
  
  I_E_GWR = D_I_WRI_K = cf_compaction = cf_drain = D_I_PSP = NULL
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),length(D_WRI_K),length(D_PSP),length(I_P_SE),length(I_P_CO),length(B_DRAIN), length(D_SE), length(B_SC_WENR))
  
  if(is.null(D_SE)){
    if(is.null(I_P_SE)){
      # if both the SEALING arguments are missing, throw an error indicating D_SE should be provided
      checkmate::assert_numeric(D_SE, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
    } else{
      warning('Use of I_P_SE in function ind_gw_recharge() is deprecated, provide D_SE instead.')
      checkmate::assert_numeric(I_P_SE, any.missing = FALSE, len = arg.length)
    }
  } else{
    checkmate::assert_numeric(D_SE, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
    if(!is.null(I_P_SE)){
      warning('You have provided both D_SE and the deprecated I_P_SE to ind_gw_regarchge(), only D_SE will be used.')
    }
  }
  
  if(!is.null(I_P_CO)){
    warning('Use of I_P_CO in function ind_gw_recharge() is deprecated, provide B_SC_WENR instead.')
    checkmate::assert_numeric(I_P_CO, any.missing = FALSE, len = arg.length)
  } else {# check that either B_SC_WENR or D_P_CO, or both is provided
      if(all(is.null(D_P_CO) | is.na(D_P_CO))) {
        checkmate::assert_character(B_SC_WENR, any.missing = FALSE, len = arg.length)
        checkmate::assert_subset(B_SC_WENR, choices = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                                                        "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                                                        "Beperkt", "Zeer beperkt"),
                                 empty.ok = FALSE)
      } else {
        checkmate::assert_numeric(D_P_CO, lower = 0, upper = 1, any.missing = FALSE,
                                  len = arg.length)
      }
  }
  
  
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(D_WRI_K, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_PSP, lower = 0, upper = 1000,  any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(B_DRAIN, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = c(
    "I", "Ia", "Ic", "II", "IIa", "IIb", "IIc", "III", "IIIa", "IIIb", "IV",
    "IVc", "IVu", "sV", "sVb", "V", "Va", "Vad", "Vao", "Vb", "Vbd", "Vbo", "VI", 
    "VId", "VII", "VIId", "VIII", "VIIId", "VIIIo", "VIIo", "VIo"
  ), empty.ok = FALSE)
  

  # import data into table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   D_PSP = D_PSP,
                   D_WRI_K = D_WRI_K,
                   I_P_SE = I_P_SE,
                   I_P_CO = I_P_CO,
                   B_DRAIN = B_DRAIN,
                   B_GWL_CLASS = B_GWL_CLASS,
                   D_SE = D_SE,
                   B_SC_WENR = B_SC_WENR,
                   D_P_CO = D_P_CO)
  
  # Calculate the indicator value for water holding capacity
  dt[,D_I_WRI_K := ind_permeability(D_WRI_K)]
  
  # Calculate indicator value for precipitation surplus
  dt[,D_I_PSP := ind_psp(D_PSP,B_LU_BRP)]
  
  # calculate sealing risk score
  if(!is.null(D_SE)){
    dt[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
  }
  
  # calculate soil compaction risk score
  if(!all(is.null(dt$D_P_CO)| is.na(dt$D_P_CO))){
    dt[,I_P_CO := D_P_CO]
  } else{
    if(!is.null(B_SC_WENR)){
      dt[, I_P_CO := ind_compaction(B_SC_WENR)]
    }
  } 
  
  # Correct for subsoil compaction or drainage
  dt[B_DRAIN == TRUE & B_GWL_CLASS %in% c('IIIb','IV'), c('cf_drain','cf_compaction') := list(0.6,1)]
  dt[B_DRAIN == FALSE | !B_GWL_CLASS %in% c('IIIb','IV'),
     c('cf_drain','cf_compaction') := list(1,fifelse(I_P_CO < 0.5,0.8,1))]
  
  # Calculate aggregated score
  dt[,I_E_GWR := (0.7 * D_I_PSP + 0.15 * D_I_WRI_K + 0.15 * I_P_SE) * cf_compaction * cf_drain]
  
  # Calculate mean
  out <- dt[,I_E_GWR]
  
  return(out)
}



#' Calculate the permeability of the top soil
#' 
#' This function calculates the permeability of the top soil
#' 
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' 
#'         
#' @export
calc_permeability <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI){
  
  id = thetaS = thetaR = alfa = n = ksat = l = Pleem = mineral = D_WRI_K = NULL
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI,
    A_SOM_LOI = A_SOM_LOI,
    value = NA_real_
  )
  
  # Calculate mineral fractions
  dt[,mineral   := A_CLAY_MI + A_SAND_MI + A_SILT_MI]
  dt[,A_CLAY_MI := A_CLAY_MI * 100 / mineral]
  dt[,A_SILT_MI := A_SILT_MI * 100 / mineral]
  dt[,Pleem     := A_CLAY_MI + A_SILT_MI]
  
  
  # Calculate unsaturated permeability
  dt[,  c("Dichtheid", "thetaR", "thetaS", "alfa", "n", "ksat", "l") := pFpara_ptf_Wosten2001(A_CLAY_MI, Pleem, A_SOM_LOI, 150, 1)]
  
  # Calculate permeabilty, based on Wosten et al., 2001
  dt[,D_WRI_K := ksat * (((1 + alfa * thetaR^n)^(1 - 1/n) - alfa * thetaR^(n - 1))^2) / ((1 + alfa * thetaR^n)^((1 - 1/n) * (l + 2)))]
  
  # output
  return(dt[,D_WRI_K])
  
}



#' Calculate the indicator score for the permeability of the top soil
#' 
#' This function calculates the indicator score for the permeability of the top soil
#' 
#' 
#' @param D_WRI_K (numeric) The value for top soil permeability (cm/d) as calculated by \code{\link{calc_permeability}}
#' 
#'         
#' @export
ind_permeability <- function(D_WRI_K){
  
  # Check inputs
  checkmate::assert_numeric(D_WRI_K, lower = 0, any.missing = FALSE)
  
  D_I_WRI_K <- evaluate_logistic(D_WRI_K,0.08,50,0.4)
  
  return(D_I_WRI_K)
}

#' Modify a groundwater recharge indicator with a target
#' 
#' This function modifiers the groundwater recharge indicator with a correction
#' factor based on the soiltype and groundwaterclass. Wetter groundwaterclasses
#' give higher values, peat gives higher values than clay, and clay higher than sand.
#' 
#' @param D_RISK_GWR (numeric) Groundwater recharge risk (distance to target, i.e. the 1 - I_E_GWR)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' 
#' @examples
#' ind_gw_target(0.5, 'zeeklei', 'IVu')
#' 
#' @returns D_OPI_GW (numeric) The opportunity index for groundwater recharge. 
#' This is the groundwater recharge indicator score modified by a target (nl: opgave).
#' This target is a measure for how relevant this soil function is given, in this
#' case, the soiltype and groundwaterclass.
#' @export
ind_gw_target <- function(D_RISK_GWR, B_SOILTYPE_AGR, B_GWL_CLASS){
  
  # add visual binding
  cfgw = D_OPI_GW = NULL
  
  # check inputs
  arg.length <- max(length(D_RISK_GWR), length(B_SOILTYPE_AGR), length(B_GWL_CLASS))
  
  checkmate::assert_numeric(D_RISK_GWR, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(OBIC::soils.obic$soiltype))
  
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = c(
    "I", "Ia", "Ic", "II", "IIa", "IIb", "IIc", "III", "IIIa", "IIIb", "IV",
    "IVc", "IVu", "sV", "sVb", "V", "Va", "Vad", "Vao", "Vb", "Vbd", "Vbo", "VI", 
    "VId", "VII", "VIId", "VIII", "VIIId", "VIIIo", "VIIo", "VIo"
  ), empty.ok = FALSE)
  
  # gather as data.table
  dt <- data.table(
    D_RISK_GWR = D_RISK_GWR,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_GWL_CLASS = B_GWL_CLASS
  )
  
  # determine correction factor (high correction factor is higher target for groundwater recharge risk)
  dt[B_SOILTYPE_AGR == 'veen' & B_GWL_CLASS %in% 
       c('I', 'Ia', 'Ib',
         'II', 'IIa', 'IIb', 'IIc',
         'III', 'IIIa', 'IIIb'), cfgw := 0]
  dt[is.na(cfgw) & B_GWL_CLASS %in% 
       c('I', 'Ia', 'Ib',
         'II', 'IIa', 'IIb', 'IIc',
         'III', 'IIIa', 'IIIb'), cfgw := 0.25]
  dt[is.na(cfgw) & B_GWL_CLASS %in% 
       c('Va', 'Vao', 'Vad'), cfgw := 0.5] # very high GWL in winter, low in summer
  dt[is.na(cfgw) & B_GWL_CLASS %in% 
       c('Vb', 'Vbo', 'Vbd'), cfgw := 0.75] # high GWL in winter, low in summer
  dt[B_SOILTYPE_AGR %in% c('dekzand', 'dalgrond', 'loess', 'duinzand'), cfgw := pmin(cfgw*2, 1)] # double cf if in drought area, here drought sensitive area is assumed to be on sandy or loess soils.
  dt[is.na(cfgw), cfgw := 1]
  
  dt[, D_OPI_GW := (0.1 + cfgw/(1/0.9)) * OBIC::evaluate_logistic(D_RISK_GWR, b=6, x0=0.4, v=.7)]
  
  # calculate groundwater recharge score modified by target
  dt[,D_OPI_GW := 1-D_OPI_GW]
  
  
  return(dt$D_OPI_GW)
}
