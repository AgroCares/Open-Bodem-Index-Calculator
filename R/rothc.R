#' RothC model
#' 
#' This function contains the Rothemsted carbon model that calculates C decomposition over time
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param IOM0 (numeric) Initial size of the inert organic matter pool (kg C/ha)
#' @param CDPM0 (numeric) Initial size of the decomposable plant material pool (kg C/ha)
#' @param CRPM0 (numeric) Initial size of the resistant plant material pool (kg C/ha)
#' @param CBIO0 (numeric) Initial size of the microbial biomass pool (kg C/ha)
#' @param CHUM0 (numeric) Initial size of the humified organic matter pool (kg C/ha)
#' @param event (numeric) The carbon application events as calculated in calc_events_current, calc_events_minimal
#' @param cor_factors (numeric) Correction factors for temperature (a), soil moisture (b), crop cover (c) and grassland renewal (d)
#' @param k1 (numeric) Decomposition rate constant for the CPM pool (/year), optional
#' @param k2 (numeric) Decomposition rate constant for the RPM pool (/year), optional
#' @param k3 (numeric) Decomposition rate constant for the BIO pool (/year), optional
#' @param k4 (numeric) Decomposition rate constant for the HUM pool (/year), optional
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' 
#' @reference Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'     
#' @export
calc_rothc  <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI,IOM0,CDPM0,CRPM0,CBIO0,CHUM0,event,cor_factors, 
                        k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02, A_DEPTH = 0.3){
  
  crops.obic = B_SOILTYPE_AGR = soils.obic = ID = effectivity = time = OSm = NULL
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 10)
  
  
  checkmate::assert_numeric(k1, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k2, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k3, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(k4, lower = 0.001, upper = 50, any.missing = FALSE, min.len = 1, max.len = 1)
  
  
  # Import
  a <- cor_factors[,a]
  b <- cor_factors[,b]
  c <- cor_factors[,c]
  d <- cor_factors[,d]
  
  abc <- approxfun (x=seq(1:length(a))/12,y=a*b*c,method="linear",rule=2)
  d <- approxfun(x=seq(1:length(a))/12,y=d,method="constant",f=1,rule=2)
  
  # Roth C model  (time in years)
  rothC <- function (time, y, parms){with(as.list(c(y,parms)),{
    dCDPM   <- abc(time)*-k1*CDPM
    dCRPM   <- abc(time)*-k2*CRPM + (d(time) * CHUM * 0.33)
    dCBIO   <- abc(time)*-k3*CBIO - (abc(time)*-k3*CBIO*R1*0.46)-(dCDPM*R1*0.46)-(dCRPM*R1*0.46)-(abc(time)*-k4*CHUM*R1*0.46)
    dCHUM   <- abc(time)*-k4*CHUM - (abc(time)*-k4*CHUM*R1*0.54)-(dCDPM*R1*0.54)-(dCRPM*R1*0.54)-(abc(time)*-k3*CBIO*R1*0.54) - (d(time) * CHUM * 0.33)
    list(c(dCDPM,dCRPM,dCBIO,dCHUM))})}
  
  
  R1 = 1/((1.67*(1.85+1.6*exp(-0.0786*A_CLAY_MI)))+1)  
  y  = c(CDPM=CDPM0, CRPM=CRPM0, CBIO=CBIO0, CHUM=CHUM0)  
  
  # Model paramteres
  parms = c(k1=k1,k2=k2,k3=k3,k4=k4,R1=R1);times=seq(0,50,1/12)
  
  out = ode(y,times,rothC,parms,events=list(data=event))
  
  BD = calc_bulk_density(A_SOM_LOI = A_SOM_LOI, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_CLAY_MI = A_CLAY_MI)  
  CF4 = 10*0.58*(A_DEPTH*100*100)*BD/1000
 
  OSm = (out[,2]+out[,3]+out[,4]+out[,5]+IOM0)/CF4
  
  OMdynamic = data.table(time=out[,1],OSm)
  
  return(OMdynamic)
}



#' Calculate the correction factors for the RothC model
#' 
#' This function calculates the correction factors for the RothC model
#' 
#' @param A_T_MEAN (numeric) Mean monthly temperature (dC), should be a vector of 12 elements, optional
#' @param A_PREC_MEAN (numeric) Mean monthly precipitation (mm), should be a vector of 12 elements, optional
#' @param A_ET_MEAN (numeric) Mean actual evapo-transpiration (mm), should be a vector of 12 elements, optional
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param crop_cover (numeric) Crop cover of the soil (options: 1 of 0)
#' @param mcf (numeric) Makkink correction factor for evapo-transpiration
#' @param renewal (numeric) A vector with the years in which grassland renewal takes place, optional
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' 
#' @reference Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'     
#' @export
calc_cor_factors <- function(A_T_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, crop_cover, mcf, renewal = NULL, A_DEPTH = 0.3){
  
  time = NULL
  
  # Check inputs
  checkmate::assert_numeric(A_T_MEAN, lower = -30, upper = 50, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_PREC_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_ET_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE)
  
  checkmate::assert_integer(crop_cover, lower = 0, upper = 1, any.missing = FALSE, len = 600)
  checkmate::assert_numeric(mcf, lower = 0, upper = 2, any.missing = FALSE, len = 600)
  
  checkmate::assert_numeric(renewal, any.missing = FALSE, min.len = 1, max.len = 10)
  checkmate::assert_subset(renewal, choices = 1:10, empty.ok = FALSE)
  
  # RothC correction factors for temperature
  a         = 47.9/(1+exp(106/(A_T_MEAN+18.3)))
  
  # Calculate actual evapo-transpiration
  ET_ACT <- A_ET_MEAN * mcf
  
  PWL       = A_PREC_MEAN-ET_ACT
  CG        = crop_cover
  
  TSMDmax   = -(20+1.3*A_CLAY_MI-0.01*(A_CLAY_MI^2))*A_DEPTH/0.23
  BareSMD   = TSMDmax/1.8
  
  # RothC correction factor for soil moisture
  TSMD      = matrix();b=matrix();TSMD[1]=0;b[1]=1;TSMDmaxc=matrix()
  
  for (i in 2:120){
    if (CG[i]==1) {TSMD[i]=pmax(TSMDmax,pmin(0,(TSMD[i-1] + PWL[i])))
    } else if (TSMD[i-1]<BareSMD) {TSMD[i]=pmin(0,(TSMD[i-1] + pmax(0,PWL[i])))
    } else {TSMD[i]=pmax(BareSMD,pmin(0,TSMD[i-1]+PWL[i]))}
    TSMDmaxc[i]=(TSMDmax*CG[i]+BareSMD*ifelse(CG[i]==0,1,0))
    if (TSMD[i]>0.444*TSMDmax) {b[i]=1} else{
      b[i]=0.2+(0.6)*(TSMDmax-TSMD[i])/(TSMDmax-0.444*TSMDmax)
    }}
  
  # RothC correction factor for soil cover
  c <- ifelse(CG==1,0.6,1)
  
  a <- rep(a,10)
  b <- rep(b,10)
  c <- rep(c,10)
  
  
  # Correction factor for grassland renewal
  if(length(renewal) > 0){
    
    d <- data.table(time = round(seq(1/12,10,by = 1/12),digits = 5))
    time_renewal <- round(renewal - 1 + 2/12,digits = 5)
    d[,d := fifelse(time %in% time_renewal,1,0)]
    
    d <- rep(d[,d],5)
    
  }else{ d <- rep(0,600)}
  
  
  return(data.table(a=a, b=b, c=c, d=d))
}




#' Initialization of the RothC carbon pools 
#' 
#' This function calculates the initial size of the carbon pools of the RothC model
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param history (character) The manure history of the soil, optional (options: default, grass_rn for grassland renewal, manure for intensive manure application and manual)
#' @param a (numeric) Fraction of total carbon in the inert organic matter pool (-)
#' @param b (numeric) Fraction of total carbon in the decomposable plant material pool (-)
#' @param c (numeric) Fraction of total carbon in the resistant plant material pool (-)
#' @param d (numeric) Fraction of total carbon in the microbial biomass pool (-)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#'     
#' @reference Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'          
#' @export
calc_cpools <- function(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, history = "default", A_DEPTH = 0.3, a = 0.0558, b = 0.015, c = 0.125, d = 0.015){
  
  
  # Check inputs
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = 10)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 10)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE)
  
  checkmate::assert_character(history,any.missing = FALSE, len = 10)
  checkmate::assert_subset(history, choices = c('default','grass_rn','manure','manual'), empty.ok = FALSE)
  
  checkmate::assert_numeric(a, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(b, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(c, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(d, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  
  
  # Calculate bulk density and total organic carbon
  BD = calc_bulk_density(A_SOM_LOI = A_SOM_LOI, B_SOILTYPE_AGR =  B_SOILTYPE_AGR, A_CLAY_MI = A_CLAY_MI)
  TOC =   A_SOM_LOI*0.58*(BD)*(A_DEPTH*100*100)/100
  
  if(history == "default"){ 
    IOM0      = 0.049*(TOC^1.139)
    CDPM0     = 0.015*(TOC-IOM0)
    CRPM0     = 0.125*(TOC-IOM0)
    CBIO0     = 0.015*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  if(history == "grass_rn"){
    IOM0      = a*TOC
    CDPM0     = 0.01*(TOC-IOM0)
    CRPM0     = 0.47*(TOC-IOM0)
    CBIO0     = 0.01*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  if(history == "manure"){
    IOM0      = a*TOC
    CDPM0     = 0.01*(TOC-IOM0)
    CRPM0     = 0.2*(TOC-IOM0)
    CBIO0     = 0.015*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  if(history == "manual"){
    IOM0      = a*TOC
    CDPM0     = b*(TOC-IOM0)
    CRPM0     = c*(TOC-IOM0)
    CBIO0     = d*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  
  out <- c(IOM0,CDPM0,CRPM0,CBIO0,CHUM0)
  return(out)
}

