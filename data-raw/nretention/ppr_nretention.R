library(data.table)

#-----------------
# function to interpolate N leaching fractions
#-----------------
# The original table (STONE model output) is provided by Piet Groenendijk in nov. 2019.
# Upper and lower groundwaterlevel is filled by Gerard Ros. 
# gwlc classes added by Brent Riechelman

interpolate_nleach_table <- function(wb, gt1man, val2u){
  # Input
  # wb (character): water type (grondwater "gw" or surfacewater "ow")
  # gt1man (character): whether values of GT1 are manually filled ("Y") or not ("N")
  # val2u (character): which interpolated values are to be used: original + predicted ('filled'), of predicated only ('pred')
  
  # read csv file
  if(wb == "gw"){
    d1 <- fread('data-raw/nretention/nretention_groenendijk_gw.csv', na.strings = '')
  }
  if(wb == "ow"){
    d1 <- fread('data-raw/nretention/nretention_groenendijk_ow.csv', na.strings = 'NA')
  }
  
  d1 <- as.data.table(d1)
  
  # extract the depth of groundwater table
  g1 <- d1[1:2,c(1,3:13)]
  g1 <- melt(g1,id.vars=c('Gewas'))
  g1 <- dcast(g1,variable~Gewas,value.var = 'value')
  g1[,ghg := as.numeric(ghg)]
  g1[,glg := as.numeric(glg)]
  
  # add additional B_GWL_CLASS
  gwlc <- data.table(variable = 
                       factor(c(
                         "Ia", "Ic",
                         "IIa", "IIc",
                         "IIIa",
                         "IVu", "IVc",
                         "Va", "Vao", "Vad", "Vbo", "Vbd", "sV", "sVb",
                         "VIo", "VId",
                         "VIIo", "VIId",
                         "VIIIo", "VIIId")),
                     ghg =       # GLG and GHG from https://edepot.wur.nl/465988
                       c(10, 25, # interpret <25 as 10 conform GT1
                         20, 60, # interpret <25 as 20 conform GT2
                         20,     # interpret <25 as 20 conform GT2
                         60, 100,# average range 40-80 conform GT4, add 20 to interpret >80
                         20, 20, 20, 32.5, 32.5, 32.5, 32.5, # interpret <25 as 20 conform GT2, average range 25-40 conform GT2b
                         60, 60, # average range 40-80 conform GT4
                         110, 110,# average range 80-140, seemingly different from GT7
                         140, 140),# interpret >140 as 140 conform GTVIIb (GT8)
                     glg = 
                       c(50, 50, # interpret <50 as 50 conform GT1
                         65, 65, # interpret range 50-80 as average conform GT2b
                         100, # interpret range 80-120 as average conform GT3b
                         100, 100,# interpret range 80-120 as average conform GT3b
                         120, 150, 180, 150, 180, 120, 120, # interpret >120 as 120 conform GT5, range 120-180 as average, consider sV and sVb as GT5 and GT5b, prefixes means 'schijnspiegel' in 1988 bodemkaart (https://edepot.wur.nl/21850 tabel 1)
                         150, 180,
                         150, 180,
                         150, 180
                       )
            )
  gwlc2 <- CJ(Gewas = c('akkerbouw', 'gras', 'mais'),
            bodem = c('Klei', 'Zand', 'Veen'),
            variable = gwlc$variable)
  gwlc2 <- merge(gwlc2, gwlc, by = 'variable')
  
  # reshape database so that nloss is one column
  d2 <- melt(d1,id.vars=c('Gewas','bodem'))
  d3 <- d2[bodem!='all',]
  d3 <- merge(d3,g1,by='variable')
  
  # add additional classes
  d3 <- rbindlist(list(d3, gwlc2), fill = TRUE, use.names = TRUE)
  
  setnames(d3,c('gt','gewas','bodem','nloss','ghg','glg'))
  
  d3[,nloss :=as.numeric(nloss)]
  d3[, nloss2 := nloss]
  
  # Manually assign values for GTI
  if (gt1man == "Y"){
    if(wb == "gw"){
      # fill values for GT1 manually
      d3[gt == "GT1" & gewas == "akkerbouw", nloss2 := 0.02]
      d3[gt == "GT1" & gewas == "gras", nloss2 := 0.01]
      d3[gt == "GT1" & gewas == "mais", nloss2 := 0.03]
    }
    if(wb == "ow"){
      d3[gt == "GT1" & is.na(nloss), nloss2 := 0.1]
    }
  }
  
  # adapt column type and log transform response variable
  # this in order to ensure low predicted values in the low range (shallow depth groundwater)
  d3[,nloss2 :=as.numeric(nloss2)]
  d3[,ghg := as.numeric(ghg)]
  d3[,lognloss := log(10*nloss2)]
  m1 = lm(lognloss~ghg*glg+gewas+bodem,data=d3)
  d3[,pred := predict(m1,newdata = d3)]
  d3[,pred := exp(pred)*0.1]
  d3[,pred := pmax(0.01,round(pred,2))]
  
  # add a column in which only NA are filled with prediction
  d3[, filled := pred]
  d3[!is.na(nloss2), filled := nloss2]
  
  # add a column to distinguish whether it's original value or filled value
  d3[, orifill := "model"]
  d3[is.na(nloss), orifill := "gevuld"]
  if (gt1man == "Y"){
    d3[gt == "GT1" & is.na(nloss), orifill := "voorgeschreven"]
  }
  
  ## change naming of grondwatertrap
  dtcon <- data.table(gt = c( "GT1", "GT2", "GT2b","GT3", "GT3b","GT4", "GT5","GT5b", "GT6", "GT7", "GT7b"),
                      B_GWL_CLASS = c("I", "II", "IIb","III", "IIIb","IV", "V", "Vb", "VI", "VII", "VIII"))
  d3 <- merge(d3, dtcon, by = "gt", all.x = TRUE)
  d3[is.na(B_GWL_CLASS), B_GWL_CLASS := gt]
  
  # Assign a set of value for the fraction of N retention (nf)
  d3[, nf:= get(val2u)]
  
  
  return(d3)
}

#-----------------
# make tables with interpolated data, and save as RData
#-----------------
library(data.table)
## Groundwater
nleach_gw_table <- interpolate_nleach_table("gw", "Y", 'filled') # with prescribed values for GT1

# remove unnecessary columns
nleach_gw_table[, c('filled', 'pred', 'nloss', 'nloss2', 'orifill', 'gt', 'lognloss') := NULL]
# save as RData
# usethis::use_data(nleach_gw_table, version = 3, overwrite = TRUE, compress = 'xz')

## surface water
nleach_ow_table <- interpolate_nleach_table("ow", "N", 'filled') # without prescribed values for GT1
#nleach_ow_table <- interpolate_nleach_table("ow", "Y", 'filled') # with prescribed values for GT1

# remove unnecessary columns
nleach_ow_table[, c('filled', 'pred', 'nloss', 'nloss2', 'orifill', 'gt', 'lognloss') := NULL]
# save as RData

# Combine both leaching tables
nleach_gw_table[, leaching_to_set := "gw"]
nleach_ow_table[, leaching_to_set := "ow"]
nleach_table <- rbindlist(list(nleach_gw_table, nleach_ow_table))
nleach_table[, gewas := as.character(gewas)]
nleach_table[, bodem := tolower(as.character(bodem))]
usethis::use_data(nleach_table, version = 3, overwrite = TRUE, compress = 'xz')

fwrite(nleach_table, 'data-raw/nretention/nleach_table.csv', encoding = 'UTF-8')

#------------------
# info output table
#------------------
# {gt}{grondwatertrap}
# {gewas}{crop type}
# {bodem}{soil type}
# {nloss}{Original values of N leaching fraction to groundwater (mgNO3/L per kgN overschot/ha/yr)}
# {ghg}{Lower value for groundwater table}
# {glg}{Upper value for groundwater table}
# {nloss2}{Original values + manually assigned values (for GT1) of N leaching fraction to groundwater}
# {pred}{Predicted value of N leaching fraction to groundwater}
# {filled}{Original values + predicted values (for NA cells) of N leaching fraction to groundwater}
# {orifill}{whether the value is the original ("model"), nf ("gevuld"), or manually assigned ("voorgeschreven"))}

#-----------------
# temp: draw scatter plot
#-----------------
require(ggplot2)
# set default theme of ggplot
theme_set(theme_light() + theme(legend.position = "right"))

d3 <- nleach_gw_table

# Scatter plot of grondwater gevoeligheid vs grondwatertrap
# Filled circle as actual (modeled with STONE) values, open circle as interpolated values
# and crossed open circle as prescribed values
ggplot(d3, aes(x = as.factor(gt), y = filled, col = gewas)) +
  geom_point(size = 4, aes(pch = orifill)) +
  scale_shape_manual(values=c(1, 16, 13), name = "") +
  xlab('Grondwatertrap') + ylab(ylabnm) +
  #ylim(0, 1.2) +
  facet_grid(.~ bodem) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x = element_text(size = 12))

# all predicted values
ggplot(d3, aes(x = as.factor(gt), y = pred, col = gewas)) +
  geom_point(size = 4, pch = 16, alpha = 0.5) +
  xlab('Grondwatertrap') + ylab(ylabnm) +
  #ylim(0, 1.2) +
  facet_grid(.~ bodem) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text.x = element_text(size = 12))

#-----------------
# temp: show final (dcasted) tables on screen
#-----------------
# d3 <- nleach_gw_table
# # all predicted values
# dcast(d3,gewas + bodem~gt,value.var = 'pred')
# # Only missing values are interpolated
# dcast(d3,gewas + bodem~gt,value.var = 'filled')