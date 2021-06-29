#' RothC model
#' 
#' This function contains the Rothemsted carbon model that calculates C decomposition over time
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' @param event (data.table) A data.table with the carbon application events as calculated in calc_events_current or calc_events_minimal
#' @param pool_size (numeric) Vector with the initial sizes of the C pools (kg C/ha), order: IOM, CDPM, CRPM, CBIO, CHUM
#' @param cor_factors (numeric) A vector with the correction required for RothC, should be in order: temperature, soil moisture, crop cover, grassland renewal
#' @param dec_rates rates (numeric) A vector of the decomposition rate constants for the C pools (/year), order: DPM, RPM, BIO and HUM pool; if not provided, default values are used, optional
#' 
#' @references Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'     
#' @export
calc_rothc  <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI, A_DEPTH = 0.3, event, pool_size, cor_factors, dec_rates = c(10,0.3,0.66,0.02)){
  
  crops.obic = soils.obic = time = OSm = NULL
  
  
  # Import data of initial pool sizes
  IOM0 <- pool_size[1]
  CDPM0 <- pool_size[2]
  CRPM0 <- pool_size[3]
  CBIO0 <- pool_size[4]
  CHUM0 <- pool_size[5]
  
  # Import RothC correction factors
  a <- cor_factors[1]
  b <- cor_factors[2]
  c <- cor_factors[3]
  d <- cor_factors[4]
  
  # Import decomposition rate constants
  k1 <- dec_rates[1]
  k2 <- dec_rates[2]
  k3 <- dec_rates[3]
  k4 <- dec_rates[4]
  
  # Check inputs
  # Add length parameter
  
  checkmate::assert_numeric(B_SOILTYPE_AGR, any.missing = FALSE, len = 1)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(crops.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE, len = 1)
  
  checkmate::assert_numeric(IOM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CDPM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CRPM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CBIO0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CHUM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  
  checkmate::assert_numeric(a, lower = 0, upper = 1, any.missing = FALSE, len = 1) # Check length
  checkmate::assert_numeric(b, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(c, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(d, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  
  checkmate::assert_numeric(k1, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k2, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k3, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k4, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  
  
  # Check for event
  
  
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
  CF4 = 10*0.5*(A_DEPTH*100*100)*BD/1000
 
  OSm = (out[,2]+out[,3]+out[,4]+out[,5]+IOM0)/CF4
  
  OMdynamic = data.table(time=out[,1],OSm)
  
  return(OMdynamic)
}



#' Calculate the correction factors for the RothC model
#' 
#' This function calculates the correction factors for the RothC model
#' 
#' @param A_TEMP_MEAN (numeric) Mean monthly temperature (dC), should be a vector of 12 elements, optional
#' @param A_PREC_MEAN (numeric) Mean monthly precipitation (mm), should be a vector of 12 elements, optional
#' @param A_ET_MEAN (numeric) Mean actual evapo-transpiration (mm), should be a vector of 12 elements, optional
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' @param crop_cover (numeric) A vector with crop cover of the soil per month (options: 1 or 0)
#' @param mcf (numeric) A vector with Makkink correction factor for evapo-transpiration per month (between 0 and 2)
#' @param renewal (numeric) A vector with the years in which grassland renewal takes place, optional
#' 
#' @references Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'     
#' @export
calc_cor_factors <- function(A_TEMP_MEAN, A_PREC_MEAN, A_ET_MEAN, A_CLAY_MI, A_DEPTH = 0.3, crop_cover, mcf, renewal = NULL){
  
  time = NULL
  
  # Check inputs
  checkmate::assert_numeric(A_TEMP_MEAN, lower = -30, upper = 50, any.missing = FALSE, len = 12)
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
  a         = 47.9/(1+exp(106/(A_TEMP_MEAN+18.3)))
  
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
  
  
  return(c(a,b,c,d))
}



#' Initialization of the RothC carbon pools 
#' 
#' This function calculates the initial size of the carbon pools of the RothC model
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' @param history (character) The manure history of the soil, optional (options: default, grass_rn for grassland renewal, manure for intensive manure application and manual)
#' @param c_fractions (numeric) A vector of the fractions of total carbon in the IOM, DPM, RPM and BIO pool (-), the fraction of the HUM pool is derived form these values; if not provided, default values are used, optional
#'     
#' @references Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'          
#' @export
calc_cpools <- function(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH = 0.3, history = "default", c_fraction){
  
  soils.obic = NULL
  
  # Load C fraction values
  IOM_fr <- c_fractions[1]
  DPM_fr <- c_fractions[2]
  RPM_fr <- c_fractions[3]
  BIO_fr <- c_fractions[4]
  
    
  # Check inputs
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = 1)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE, len = 1)
  
  checkmate::assert_character(history,any.missing = FALSE, len = 1)
  checkmate::assert_subset(history, choices = c('default','grass_rn','manure','manual'), empty.ok = FALSE)
  
  checkmate::assert_numeric(IOM_fr, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(DPM_fr, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(RPM_fr, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  checkmate::assert_numeric(BIO_fr, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, max.len = 1)
  
  
  # Calculate bulk density and total organic carbon
  BD = calc_bulk_density(A_SOM_LOI = A_SOM_LOI, B_SOILTYPE_AGR =  B_SOILTYPE_AGR, A_CLAY_MI = A_CLAY_MI)
  TOC =   A_SOM_LOI*0.5*(BD)*(A_DEPTH*100*100)/100
  
  # Default distribution
  if(history == "default"){ 
    IOM0      = 0.049*(TOC^1.139)
    CDPM0     = 0.015*(TOC-IOM0)
    CRPM0     = 0.125*(TOC-IOM0)
    CBIO0     = 0.015*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  # Distribution after grassland renewal
  if(history == "grass_rn"){
    IOM0      = IOM_fr*TOC
    CDPM0     = 0.01*(TOC-IOM0)
    CRPM0     = 0.47*(TOC-IOM0)
    CBIO0     = 0.01*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  # Distribution after intense manure application
  if(history == "manure"){
    IOM0      = IOM_fr*TOC
    CDPM0     = 0.01*(TOC-IOM0)
    CRPM0     = 0.2*(TOC-IOM0)
    CBIO0     = 0.015*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  # Distribution based on manual input
  if(history == "manual"){
    IOM0      = IOM_fr*TOC
    CDPM0     = DPM_fr*(TOC-IOM0)
    CRPM0     = RPM_fr*(TOC-IOM0)
    CBIO0     = BIO_fr*(TOC-IOM0)
    CHUM0     = TOC-IOM0-CDPM0-CRPM0-CBIO0
  }
  
  
  out <- c(IOM0,CDPM0,CRPM0,CBIO0,CHUM0)
  return(out)
}

