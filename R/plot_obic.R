#' Visualize the evaluation of a OBI soil function
#' 
#' This function visualizes the evaluation of soil functions as used in the OBIC package.
#' 
#' @param D_SF (numeric) The value of a soil index
#' @param ind_fun (character) The name of the OBIC function that is visualised
#' 
#' @import data.table
#' @import ggplot2
#' 
#' @export
plot_eval <- function(D_SF,ind_fun) {
  
  # Check inputs
  checkmate::assert_numeric(D_SF, lower = 0 , upper = 10000, any.missing = FALSE)
  
  # data.table to select input variables to make evaluation plots
  dt.fun <- data.table(findex = c('ind_zinc','ind_copper','ind_potassium','ind_pmn','ind_sulpher','ind_winderodibility',
                                  'ind_magnesium','ind_waterstressindex','ind_waterretention','ind_cec'),
                       xlabel = c('zinc content crop','cu content crop','K-availability based on K-CEC and K-CaCl2','PMN normalized',
                                  'S-index based on SLV','stuifgevoeligheid','Mg-index','reduction in yield due to waterstress',
                                  'plant available water (mm)','cation buffering via CEC'),
                       ptitle = c('zinc availability','copper availability','K-index','PMN','S-supply','stuifgevoeligheid',
                                  'Mg-index','water availability','plant available water','cation buffering'),
                       luse = c(FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE),
                       soil = c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE),
                       os = c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                       aer = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE))
  
  # select the relevant OBIC index
  dt.fun <- dt.fun[findex==ind_fun]
  
  # make data.table for given input, predicted index and 
  # required covariables land use, soiltype, economic region, os-content (5 and 15%)
  dt <- CJ(D_SF = D_SF,
           I_SF = NA,
           luse = 265,#c(265,2014),
           soil = c('zeeklei','loess','veen','loess','dekzand'),
           os   = c(5,15),
           aer  = 'Rivierengebied')
 
  # which covariables are (not) required
  covs <- c('luse','soil','os','aer')
  cols <- covs[which(dt.fun[,..covs]==FALSE)]
 
  # remove input options from data.table that are not required 
  dt <- unique(dt[,(cols) := NULL])

  # predict value of the OBIC indicator
  if(dt.fun$luse & dt.fun$soil & dt.fun$os){
    dt[,I_SF := get(ind_fun)(D_SF,B_LU_BRP = luse, B_BT_AK = soil,A_OS_GV = os)]
    dt[,luse := ifelse(luse==265,'grasland','bouwland')]
    dt[,os := ifelse(os==5,'os-arm','os-rijk')]
    dt[,group := paste(os,luse,'op',soil,sep=' ')]
    dt[luse=='grasland' | soil != 'zeeklei',group := paste(luse,'op',soil,sep=' ')]
    dt <- unique(dt[,c(covs[!covs %in% cols]):=NULL])
  } else if (dt.fun$luse & dt.fun$soil & dt.fun$aer){
    dt[,I_SF := get(ind_fun)(D_SF,B_LU_BRP = luse, B_BT_AK = soil,B_LG_CBS = aer)]
    dt[,luse := ifelse(luse==265,'grasland','bouwland')]
    dt[,group := paste(luse,'op',soil,sep=' ')]
    dt[luse=='grasland',group := 'grasland',]
  } else if (dt.fun$luse & dt.fun$soil){
    dt[,I_SF := get(ind_fun)(D_SF,B_LU_BRP = luse, B_BT_AK = soil)]
    dt[,luse := ifelse(luse==265,'grasland','bouwland')]
    dt[,group := paste(luse,'op',soil,sep=' ')]
  } else if (dt.fun$luse){
    dt[,I_SF := get(ind_fun)(D_SF,B_LU_BRP = luse)]
    dt[,group := ifelse(luse==265,'grasland','bouwland')]
  } else if (dt.fun$soil){
    dt[,I_SF := get(ind_fun)(D_SF,B_BT_AK = soil)]
    dt[,group := soil]
  } else {
    dt[,I_SF := get(ind_fun)(D_SF)]
    dt[,group:= 'elke bodem en teelt']
  }
  
  # make a subset of the data.table to plot distinct points along the evaluation line
  ncat <- length(unique(dt$group))
  nsel <- dt$D_SF[c(seq(1,0.5*nrow(dt),length.out = 4),seq(0.6*nrow(dt),nrow(dt),length.out = 2))]
  
  dt.sub <- dt[D_SF %in% nsel,]
  
  # classification colouring
  df.class <- data.table(
    xmin = rep(min(dt$D_SF),5),
    xmax = rep(max(dt$D_SF),5),
    ymin = c(0,0.25,0.5,0.75,1.00),
    ymax = c(0.25,0.5,0.75,1.0,1.25),
    classUK = c("very low", "low", "moderate", "high", "very high"),
    classNL = c('zeer laag','laag','voldoende','ruim voldoende','hoog')
  )
  
  # set class as factor
  df.class[, class := factor(classUK, levels = c("very high", "high", "moderate", "low", "very low"))]
  
  # plot the function
  eplot = list()
  
  if(length(unique(dt.sub$group))<=5){
  eplot[[1]] = ggplot2::ggplot(data = dt, aes(x = D_SF)) + 
                ggplot2::geom_rect(data = df.class, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class), alpha = 0.6) +
                ggplot2::scale_fill_brewer(palette = "Spectral",direction = -1) +
                ggplot2::geom_line(data = dt, aes(y = I_SF,group = group), 
                                   color = "gray25", size = 0.8) +
                ggplot2::geom_point(data = dt.sub, aes(y = I_SF,group = group,shape = group),
                                        col='gray25',size=3) +
                ylim(c(0, 1.25)) +
                labs(x = dt.fun$xlabel,y = "evaluation soil index",
                    title = paste0("Evaluation of soil index: ",dt.fun$ptitle)) +
                theme_bw()
  } else {
  
    # make subsets for plotting
    dt.arable <- dt[grepl('bouwland',group)]
    dt.sub.arable <- dt.sub[grepl('bouwland',group)]
    dt.grassland <- dt[grepl('grasland',group)]
    dt.sub.grassland <- dt.sub[grepl('grasland',group)]
    
    eplot[[1]] = ggplot2::ggplot(data = dt.arable, aes(x = D_SF)) + 
      ggplot2::geom_rect(data = df.class, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class), alpha = 0.6) +
      ggplot2::scale_fill_brewer(palette = "Spectral",direction = -1) +
      ggplot2::geom_line(data = dt.arable, aes(y = I_SF,group = group), 
                         color = "gray25", size = 0.8) +
      ggplot2::geom_point(data = dt.sub.arable, aes(y = I_SF,group = group,shape = group),
                          col='gray25',size=3) +
      ylim(c(0, 1.25)) +
      labs(x = dt.fun$xlabel,y = "evaluation soil index",
           title = paste0("Evaluation of soil index (arable): ",dt.fun$ptitle)) +
      theme_bw()
    
    eplot[[2]] = ggplot2::ggplot(data = dt.grassland, aes(x = D_SF)) + 
      ggplot2::geom_rect(data = df.class, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class), alpha = 0.6) +
      ggplot2::scale_fill_brewer(palette = "Spectral",direction = -1) +
      ggplot2::geom_line(data = dt.grassland, aes(y = I_SF,group = group), 
                         color = "gray25", size = 0.8) +
      ggplot2::geom_point(data = dt.sub.grassland, aes(y = I_SF,group = group,shape = group),
                          col='gray25',size=3) +
      ylim(c(0, 1.25)) +
      labs(x = dt.fun$xlabel,y = "evaluation soil index",
           title = paste0("Evaluation of soil index (grassland): ",dt.fun$ptitle)) +
      theme_bw()
    
                }
  
  if(ind_fun=='ind_cec'){
  
    eplot[[3]] <- recordPlot()  
    par(mar=rep(0.2, 4))
    TernaryPlot(alab = 'Ca-occupation (%)', blab = 'Mg-occupation (%)', clab = 'K-occupation (%)')
    
    FunctionToContour <- function(a,b,c){  sqrt((a*100 - 80)^2 + (b*100 - 8.5)^2 + (c*100 - 14.5)^2)/125}
    
    values <- TernaryPointValues(FunctionToContour, resolution=20L)
    ColourTernary(values,direction = 3,spectrum = rev(brewer.pal(10,'Spectral')))
    TernaryContour(FunctionToContour, resolution=36L)
  }
  
  # return output
  return(eplot)
}



