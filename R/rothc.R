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
#' @details The input parameter event should be a data.table containing the columns time, var, value and method. See ?deSolve::events for the exact format options.
#' 
#' @references Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'     
#' @export
calc_rothc  <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI, A_DEPTH = 0.3, event, pool_size, cor_factors, dec_rates = c(10,0.3,0.66,0.02)){
  
  crops.obic = soils.obic = time = OSm = NULL
  
  
  # Import data of initial pool sizes
  IOM0  <- pool_size[1]
  CDPM0 <- pool_size[2]
  CRPM0 <- pool_size[3]
  CBIO0 <- pool_size[4]
  CHUM0 <- pool_size[5]
  
  # Import RothC correction factors
  checkmate::assertDataTable(cor_factors, any.missing = FALSE, ncols = 4, nrows = 600)
  a <- cor_factors[,a]
  b <- cor_factors[,b]
  c <- cor_factors[,c]
  d <- cor_factors[,d]
  
  # Import decomposition rate constants
  k1 <- dec_rates[1]
  k2 <- dec_rates[2]
  k3 <- dec_rates[3]
  k4 <- dec_rates[4]
  
  # Check inputs
  # Additional check event and correction factors?
  
  # Check Soil parameters
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = 1)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(OBIC::soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE, len = 1)
  
  checkmate::assertDataTable(event, ncols = 4)
  checkmate::assert_subset(colnames(event), choices = c('time','var','value','method'), empty.ok = FALSE)
  
  # Check pool sizes
  checkmate::assert_vector(pool_size, any.missing = FALSE, len = 5)
  checkmate::assert_numeric(IOM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CDPM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CRPM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CBIO0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(CHUM0, lower = 0, upper = 15E6, any.missing = FALSE, len = 1)
  
  # Check correction factors
  checkmate::assert_numeric(a, lower = 0, upper = 5, any.missing = FALSE, len = 600)
  checkmate::assert_numeric(b, lower = 0.2, upper = 1, any.missing = FALSE, len = 600)
  checkmate::assert_numeric(c, lower = 0.6, upper = 1, any.missing = FALSE, len = 600)
  checkmate::assert_subset(c, choices = c(0.6,1), empty.ok = FALSE)
  checkmate::assert_numeric(d, lower = 0, upper = 1, any.missing = FALSE, len = 600)
  checkmate::assert_subset(d, choices = c(0,1), empty.ok = FALSE)
  
  # Check decomposition rates
  checkmate::assert_vector(dec_rates, any.missing = FALSE, len = 4)
  checkmate::assert_numeric(k1, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k2, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k3, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(k4, lower = 0.001, upper = 50, any.missing = FALSE, len = 1)
  
  
  # Calculate interpolation for correction factors
  abc <- approxfun (x=seq(1:length(a))/12,y=a*b*c,method="linear",rule=2)
  d <- approxfun(x=seq(1:length(a))/12,y=d,method="constant",f=1,rule=2)
  
  # Roth C model  (time in years)
  rothC <- function (time, y, parms){with(as.list(c(y,parms)),{
    dCDPM   <- abc(time)*-k1*CDPM
    dCRPM   <- abc(time)*-k2*CRPM + (d(time) * CHUM * 0.33)
    dCBIO   <- abc(time)*-k3*CBIO - (abc(time)*-k3*CBIO*R1*0.46)-(dCDPM*R1*0.46)-(dCRPM*R1*0.46)-(abc(time)*-k4*CHUM*R1*0.46)
    dCHUM   <- abc(time)*-k4*CHUM - (abc(time)*-k4*CHUM*R1*0.54)-(dCDPM*R1*0.54)-(dCRPM*R1*0.54)-(abc(time)*-k3*CBIO*R1*0.54) - (d(time) * CHUM * 0.33)
    list(c(dCDPM,dCRPM,dCBIO,dCHUM))})}
  
  # Calculate correction factor for soil structure
  R1 = 1/((1.67*(1.85+1.6*exp(-0.0786*A_CLAY_MI)))+1)
  
  # Load model parameters
  parms = c(k1=k1,k2=k2,k3=k3,k4=k4,R1=R1)
  times=seq(0,50,1/12)
  y  = c(CDPM=CDPM0, CRPM=CRPM0, CBIO=CBIO0, CHUM=CHUM0)
  
  # Add intermediate timesteps
  event.times <- unique(event$time)
  times <- unique(c(times,event.times))
  times <- sort(times)
  
  # Run the model
  out = deSolve::ode(y,times,rothC,parms,events=list(data=event))
  
  # Convert kg C/ha to % OM
  BD = calc_bulk_density(A_SOM_LOI = A_SOM_LOI, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_CLAY_MI = A_CLAY_MI)  
  CF = 10*0.5*(A_DEPTH*100*100)*BD/1000
  
  # Calculate total carbon content
  OM = (out[,2]+out[,3]+out[,4]+out[,5]+IOM0)/CF
  
  # Format output
  OMdynamic = data.table(time=out[,1],OM)
  
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
  arg.length <- max(length(crop_cover),length(mcf))
  years <- 1:(arg.length/12)
  
  ## Check weather and soil parameters
  checkmate::assert_numeric(A_TEMP_MEAN, lower = -30, upper = 50, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_PREC_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_ET_MEAN, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE, len = 1)
  
  ## Check crop cover and Makkink correction factor
  checkmate::assert_numeric(crop_cover, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(crop_cover, choices = c(0,1), empty.ok = FALSE)
  checkmate::assert_numeric(mcf, lower = 0, upper = 2, any.missing = FALSE, len = arg.length)
  
  ## Check grassland renewal
  if(!is.null(renewal)){ checkmate::assert_numeric(renewal, any.missing = FALSE, min.len = 1, max.len = arg.length/12) 
                         checkmate::assert_subset(renewal, choices = years, empty.ok = FALSE) }
  
  # Insert some kind of year check -> usefull for extension
  
  # RothC correction factors for temperature
  a         = 47.9/(1+exp(106/(A_TEMP_MEAN+18.3)))
  
  # RothC correction factors for soil moisture
  # Calculate actual evapo-transpiration
  ET_ACT <- A_ET_MEAN * mcf
  
  # Calculate soil moisture deficit
  SMD       = A_PREC_MEAN-ET_ACT
  CC        = crop_cover
  
  # Calculate maximal top soil moisture deficit (TSMD) and bare soil moisture deficit
  TSMDmax   = -(20+1.3*A_CLAY_MI-0.01*(A_CLAY_MI^2))*A_DEPTH/0.23
  BareSMD   = TSMDmax/1.8
  
  # RothC correction factor for soil moisture
  TSMD = matrix(); b=matrix(); TSMD[1]=0; b[1]=1; TSMDmaxc=matrix()
  
  for (i in 2:arg.length){
    if (CC[i]==1){ TSMD[i]=pmax(TSMDmax, pmin(0,(TSMD[i-1] + SMD[i])))
    } else if (TSMD[i-1]<BareSMD){ TSMD[i] = pmin(0,(TSMD[i-1] + pmax(0,SMD[i])))
    } else { TSMD[i]=pmax(BareSMD, pmin(0, TSMD[i-1] + SMD[i]))
    }
    
    TSMDmaxc[i]=(TSMDmax*CC[i]+BareSMD*ifelse(CC[i]==0,1,0))
    
    if (TSMD[i]>0.444*TSMDmax){ b[i]=1
    }else{
      b[i]=0.2+(0.6)*(TSMDmax-TSMD[i])/(TSMDmax-0.444*TSMDmax)
      }
    }
  
  # RothC correction factor for soil cover
  c <- ifelse(CC==1,0.6,1)
  
  # Extend b and c to 50 years
  extend <- ceiling(50/(arg.length/12))
  b <- rep(b,extend, length.out = 600)
  c <- rep(c,extend, length.out = 600)
  
  
  # Correction factor for grassland renewal
  if(!is.null(renewal)){
    
    # Make data.table with all months of crop rotation plan
    d <- data.table(time = round(seq(1/12,arg.length/12,by = 1/12),digits = 5))
    
    # Select February for the years in which grassland renewal is applied 
    time_renewal <- round(renewal - 1 + 2/12,digits = 5)
    d[,d := fifelse(time %in% time_renewal,1,0)]
    
    d <- rep(d[,d],extend, length.out = 600)
    
  }else{ d <- rep(0,600)}
  
  # Format output
  cor_factors <- data.table(a=a, b=b, c=c, d=d)
  return(cor_factors)
}



#' Initialization of the RothC carbon pools 
#' 
#' This function calculates the initial size of the carbon pools of the RothC model
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_DEPTH (numeric) Depth of the soil layer (m)
#' @param history (character) The manure history of the soil, optional (options: default, grass_renewal for grassland renewal, manure for intensive manure application and manual)
#' @param c_fractions (numeric) A vector of the fractions of total carbon in the IOM, DPM, RPM and BIO pool (-), the fraction of the HUM pool is derived form these values; if not provided, default values are used, optional
#'     
#' @references Coleman & Jenkinson (1996) RothC - A model for the turnover of carbon in soil
#'          
#' @export
calc_cpools <- function(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_DEPTH = 0.3, history = "default", c_fractions){
  
  soils.obic = NULL
  
  # Load C fraction values
  IOM_fr <- c_fractions[1]
  DPM_fr <- c_fractions[2]
  RPM_fr <- c_fractions[3]
  BIO_fr <- c_fractions[4]
  
    
  # Check inputs
  ## Check soil parameters
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = 1)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(OBIC::soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 2, any.missing = FALSE, len = 1)
  
  ## Check manure history
  checkmate::assert_character(history,any.missing = FALSE, len = 1)
  checkmate::assert_subset(history, choices = c('default','grass_renewal','manure','manual'), empty.ok = FALSE)
  
  ## Check C distribution fractions
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
  if(history == "grass_renewal"){
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

