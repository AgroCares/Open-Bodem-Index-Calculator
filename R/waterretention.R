#' Calculate indicators for water retention in topsoil
#'
#' This function calculates different kind of Water Retention Indices given the continuous pedotransferfunctions of Wosten et al. (2001)
#' These include : 'wilting point','field capacity','water holding capacity','plant available water' and 'Ksat'
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_OS_GV (numeric) The organic matter content of the soil (\%)
#' @param type (character) The type of waterretention index. Options include c('wilting point','field capacity','water holding capacity','plant available water','Ksat')
#'
#' @references Wosten et al. (2001) Pedotransfer functions: bridging the gap between available basic soil data and missing hydraulogic characteristics. Journal of Hydrology 251, 123-150.
#'
#' @import data.table  
#'
#' @export
calc_waterretention <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_OS_GV,
                                type = 'plant available water') {
  
  id = thetaS = thetaR = alfa = n = fc = wp = whc = paw = ksat = density = NULL
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI), length(A_OS_GV))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_character(type, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(type, choices = c('wilting point','field capacity','water holding capacity','plant available water','Ksat'), empty.ok = FALSE)
 
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI,
    A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
    A_OS_GV = A_OS_GV,
    value = NA_real_
  )
  # settings
  p.topsoil = 1
  p.fieldcapacity = 2.2
  p.wiltingpoint = 4.2
  p.depth = 0.3
  
  # express soil texture as fraction of total mineral part (if needed)
  dt[,A_CLAY_MI := A_CLAY_MI * 100 / (A_CLAY_MI + A_SAND_MI + A_SILT_MI)]
  dt[,A_SAND_MI := A_SAND_MI * 100 / (A_CLAY_MI + A_SAND_MI + A_SILT_MI)]
  dt[,A_SILT_MI := A_SILT_MI * 100 / (A_CLAY_MI + A_SAND_MI + A_SILT_MI)]
  
  # calculate water retention parameters given Wosten (1999), based on HYPRES
  dt[A_CLAY_MI <= 25, density := 1 / (0.02525 * A_OS_GV + 0.6541)]
  dt[A_CLAY_MI >  25, density := 6.7e-07 * A_OS_GV^4 - 7.792e-05 * A_OS_GV^3 + 0.00314712 * A_OS_GV^2 - 0.06039523 * A_OS_GV + 1.33932206]
  dt[,thetaR := 0.01]
  dt[,thetaS := 0.7919 + 0.001691 * A_CLAY_MI - 0.29619 * density - 0.000001491 * A_SILT_MI^2 + 0.0000821 * A_OS_GV^2 +
                0.02427 / A_CLAY_MI + 0.01113 / A_SILT_MI + 0.01472 * log(A_SILT_MI) - 0.0000733 * A_OS_GV * A_CLAY_MI - 
                0.000619 * density * A_CLAY_MI - 0.001183 * density * A_OS_GV - 0.0001664 * p.topsoil * A_SILT_MI]
  dt[,alfa := exp(-14.96 + 0.03135 * A_CLAY_MI + 0.0351 * A_SILT_MI + 0.646 * A_OS_GV + 15.29 * density - 0.192 * p.topsoil-
                  4.671 * density^2 - 0.000781 * A_CLAY_MI^2 - 0.00687 * A_OS_GV^2 + 0.0449 / A_OS_GV + 0.0663 * log(A_SILT_MI) +
                  0.1482 * log(A_OS_GV) - 0.04546 * density * A_SILT_MI - 0.4852 * density * A_OS_GV + 0.00673 * p.topsoil * A_CLAY_MI)]
  dt[,n := 1 + exp( - 25.23 - 0.02195 * A_CLAY_MI + 0.0074 * A_SILT_MI - 0.1940 * A_OS_GV + 45.5 * density - 7.24 * density^2 + 
                     0.0003658 * A_CLAY_MI^2 + 0.002885 * A_OS_GV^2 - 12.81/density - 0.1524/A_SILT_MI - 0.01958/A_OS_GV - 
                     0.2876 * log(A_SILT_MI) - 0.0709 * log(A_OS_GV) - 44.6 * log(density) - 0.02264 * density * A_CLAY_MI + 
                     0.0896 * density * A_OS_GV + 0.00718 * p.topsoil * A_CLAY_MI)]  
  dt[,wp := thetaR + (thetaS - thetaR) / ((1 + (abs(alfa*(-1 * 10^p.wiltingpoint)))^n)^(1 - 1 / n))]
  dt[,fc := thetaR + (thetaS - thetaR) / ((1 + (abs(alfa*(-1 * 10^p.fieldcapacity)))^n)^(1 - 1 / n))]
  dt[,whc := (thetaR + (thetaS - thetaR) / ((1 + (abs(alfa*(-1 * 10^0)))^n)^(1 - 1 / n))) * p.depth * 1000]
  dt[,paw := abs(fc - wp) * p.depth * 1000]
  
  # convert from % to mm (wp) and mm (fc)
  dt[,wp := wp * p.depth * 1000]
  dt[,fc := wp * p.depth * 1000]
  
  # calculate Ksat (saturated conductivity)
  dt[,ksat := 7.755 + 0.0352 * A_SILT_MI + 0.93 * p.topsoil - 0.967 * density^2 - 0.000484 * A_CLAY_MI^2 -
              0.000322 * A_SILT_MI^2 + 0.001 / A_SILT_MI - 0.0748 / A_OS_GV - 0.643 * log(A_SILT_MI) -
              0.01398 * density * A_CLAY_MI - 0.1673 * density * A_OS_GV + 0.2986 * p.topsoil * A_CLAY_MI -
              0.03305 * p.topsoil * A_SILT_MI]
  dt[A_OS_GV>25 & ksat < 0, ksat := 5]
  
  # select Water Retention index
  if(type=='wilting point'){dt[,value := wp]}
  if(type=='field capacity'){dt[,value := fc]}
  if(type=='water holding capacity'){dt[,value := whc]}
  if(type=='plant available water'){dt[,value := paw]}
  if(type=='Ksat'){dt[,value := round(ksat,2)]
    }
  
  # return selected Water Retention index
  value <- dt[, value]
  
  # return
  return(value)
  
}


#' Calculate indicator for Water Retention index
#'
#' This function evaluates different Water Retention Indices.
#' These include : 'wilting point','field capacity','water holding capacity','plant available water' and 'Ksat'
#'  
#' @param D_P_WRI (numeric) The value for Water Retention index (WRI) as calculated by \code{\link{calc_waterretention}}
#' @param type (character) The type of waterretention index. Options include c('wilting point','field capacity','water holding capacity','plant available water','Ksat')
#' 
#' @export
ind_waterretention <- function(D_P_WRI,type ='plant available water') {
  
  # Check inputs
  checkmate::assert_numeric(D_P_WRI, lower = 0, upper = 1000, any.missing = FALSE)
  checkmate::assert_character(type, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(type, choices = c('wilting point','field capacity','water holding capacity',
                                             'plant available water','Ksat'), empty.ok = FALSE)
  
  
  # Evaluate the Water Retention index (WRI)
  if(type == 'plant available water') {value <- evaluate_logistic(D_P_WRI, b = 0.03, x0 = 50, v = 0.8)}
  if(type == 'Ksat') {value <- evaluate_logistic(D_P_WRI, b = 0.2, x0 = 6, v = 0.3)}
  if(type == 'water holding capacity') {value <- evaluate_logistic(D_P_WRI, b = 0.03, x0 = 50, v = 0.8)}
  if(type == 'wilting point') {value <- evaluate_logistic(D_P_WRI, b = 0.05, x0 = 10, v = .1)}
  if(type == 'field capacity') {value <- evaluate_logistic(D_P_WRI, b = 0.05, x0 = 10, v = .1)}
  
  # return value
  return(value)
  
}
