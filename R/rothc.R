#' RothC model
#' 
#' This function contains the Rothemsted carbon model that calcualtes C decomposition over time
#' 
#' @param B_LU_BRP (numeric) a series with crop codes given the crop rotation plan (source: the BRP)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param IOM0 (numeric) Initial size of the inert matter pool (kg C/ha)
#' @param CDPM0 (numeric) Initial size of the decomposable plant material pool (kg C/ha)
#' @param CRPM0 (numeric) Initial size of the resistant plant material pool (kg C/ha)
#' @param CBIO0 (numeric) Initial size of the microbial biomass pool (kg C/ha)
#' @param CHUM0 (numeric) Initial size of the humifide organic matter pool (kg C/ha)
#' @param event (numeric) The carbon application events as calculated in calc_events_current, calc_events_minimal
#' @param cor_factors (numeric) Correction factors for temperature (a), soil moisture (b), crop cover (c) and grassland renewal (d)
#' @param k1 (numeric) Decomposition rate constant for the CPM pool (/year)
#' @param k2 (numeric) Decomposition rate constant for the RPM pool (/year)
#' @param k3 (numeric) Decomposition rate constant for the BIO pool (/year)
#' @param k4 (numeric) Decomposition rate constant for the HUM pool (/year)
#' @param depth (numeric) Depth of the soil layer (m)
#'     
#' @export
calc_rothc  <- function(B_LU_BRP,A_SOM_LOI,A_CLAY_MI,IOM0,CDPM0,CRPM0,CBIO0,CHUM0,event,cor_factors, 
                        k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02, depth = 0.3){
  
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
  
  BD = calc_bulk_density(A_SOM_LOI = A_SOM_LOI, B_LU_BRP = B_LU_BRP, A_CLAY_MI = A_CLAY_MI)  
  CF4 = 10*0.58*(depth*100*100)*BD/1000
 
  OSm = (out[,2]+out[,3]+out[,4]+out[,5]+IOM0)/CF4
  
  OMdynamic = data.table(time=out[,1],OSm)
  
  return(OMdynamic)
}



#' Calculate the correction factors for the RothC model
#' 
#' This function calculates the correction factors for the RothC model
#' 
#' @param Temp (numeric) Mean monthly temperature (dC), should be a vector of 12 elements
#' @param Prec (numeric) Mean monthly precipitation (mm), should be a vector of 12 elements
#' @param ETact (numeric) Mean actual evapo-transpiration (mm), should be a vector of 12 elements
#' @param crop_cover (numeric) Crop cover of the soil (fraction)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param renewal (numeric) The frequency of grassland renewal (optional,every x years)
#' @param depth (numeric) Depth of the soil layer (m)
#'     
#' @export
calc_cor_factors <- function(Temp, Prec, ETact, crop_cover, A_CLAY_MI, renewal = 0, depth = 0.3){
  
  # RothC correction factors for temperature
  a         = 47.9/(1+exp(106/(Temp+18.3)))
  
  PWL       = Prec-ETact
  CG        = crop_cover
  
  TSMDmax   = -(20+1.3*A_CLAY_MI-0.01*(A_CLAY_MI^2))*depth/0.23
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
  c = ifelse(CG==1,0.6,1)
  
  a <- rep(a,10)
  b <- rep(b,10)
  c <- rep(c,10)
  
  if(renewal > 0){
    # Correction factor for grassland renewal
    d <- data.table(time = round(seq(1/12,10,by = 1/12),digits = 5))
    time_renewal <- round(seq(renewal,10,by = renewal) - 1 + 2/12,digits = 5)
    d[,d := fifelse(time %in% time_renewal,1,0)]
    
    d <- rep(d[,d],5)
    
  }else{ d <- rep(0,600)}
  
  
  return(data.table(a=a, b=b, c=c, d=d))
}


