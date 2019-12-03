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
  
  # set variables as NULL
  soiltype = soiltype.n = crop_waterstress = crop_maatregel = crop_code = ID = NULL
  OBICvariable = maatregel_nr = Dremp_S = app = app1 = app2 = sector = grondsoort = indicator = NULL
  Ef_M_v = tresshold = score.m = weight = grp = score.mp = Prio_M = m.effect = FS = TH = NULL
  
  # make local copy of dt.score
  dt.score <- copy(dt.score)
  
  # add local databases for joining properties -----
  
    # wegingsfactoren voor integratie per bodemfunctie type
    w <- as.data.table(OBIC::weight.obic)
    
    # measures database
    maatregel.obic <- as.data.table(OBIC::maatregel.obic)
    
    # crop categories
    crops.obic <- as.data.table(OBIC::crops.obic)
    
    # soil categories
    soils.obic <- as.data.table(OBIC::soils.obic)

  # adapt local databases before joining -----
    
    # Make an additional category for loess
    soils.obic[soiltype == 'loess', soiltype.n := 'loess'] 
    # merge with dt.score
    dt.score <- merge(dt.score, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
    
    # add crop_maatregel to crops.obic
    crops.obic[grepl('grasland|natuur', crop_waterstress), crop_maatregel := "melkveehouderij"]
    crops.obic[grepl('granen|suikerbiet|aardappel|mais|overig', crop_waterstress), crop_maatregel := "akkerbouw"]
    crops.obic[grepl('groenten|bloembollen', crop_waterstress), crop_maatregel := "groente"]
    crops.obic[grepl('boomteelt|fruit', crop_waterstress), crop_maatregel := "boomteelt"]
    # merge with dt.score
    dt.score <- merge(dt.score, crops.obic[, list(crop_code, crop_maatregel)], by.x = "B_LU_BRP", by.y = "crop_code")
    setkey(dt.score, ID)
    
  # redesign measures db to make it suitable for joining -----
    
    # drop indicators from measure db that are not present in dt.score
    mdb1 <- maatregel.obic[OBICvariable %in% colnames(dt.score)]
    
    # replace two NA values in database (reason why missing is unknown)
    cols <- colnames(mdb1)[grepl('melk',colnames(mdb1))]
    mdb1[,(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = cols]
    
    # drop descriptive columns that are not needed
    cols <- colnames(mdb1)[grepl('omschr|Bodemfunct',colnames(mdb1))]
    mdb1[,c(cols) := NULL]
    
    # check if there is missing data for any indicator
    cols <- colnames(dt.score)[grepl('^I_C_|^I_B|^I_P_',colnames(dt.score))]
    cols <- cols[!cols %in% unique(mdb1$OBICvariable)]
    
    # make temporary data.table with for each unknown indicator a zero impact measure
    ind.miss <- CJ(maatregel_nr = 1:max(mdb1$maatregel_nr), OBICvariable = cols)
    ind.add  <- merge(unique(mdb1[,c(1:2,4)]),ind.miss,by='maatregel_nr')
    
    # add missing columns with value 0, and a tresshold value of 0.5
    cols <- colnames(mdb1)[grepl('melk|groente|akker|boom|klei|veen|zand|loss|Ef',colnames(mdb1))]
    ind.add[,c(cols) := 0]
    ind.add[,Dremp_S := 0.5]
    
    # rbind these new values with the original measures table
    mdb2 <- rbind(mdb1,ind.add,fill = T)
    
    # reshape mdb: agricultural sector from columns to rows
    cols_m <- colnames(mdb2)[grepl('melkvee|akker|groente|boom',colnames(mdb2))]
    cols_i <- colnames(mdb2)[!colnames(mdb2) %in% cols_m]
    mdb3 <- melt(mdb2,id.vars = cols_i, measure.vars = cols_m, variable.name = 'sector',
                 value.name = 'app1',variable.factor = FALSE)
    
    # reshape mdb: soil type from columns to rows
    cols_m <- colnames(mdb3)[grepl('klei|veen|zand|loss',colnames(mdb3))]
    cols_i <- colnames(mdb3)[!colnames(mdb3) %in% cols_m]
    mdb3 <- melt(mdb3,id.vars = cols_i, measure.vars = cols_m, variable.name = 'grondsoort',
                 value.name = 'app2',variable.factor = FALSE)
    
    # take combined applicability for sector and soil type 
    mdb3[,app := pmin(app1,app2)]
    mdb3[,c('app1','app2') := NULL]
    
    # remove one option for melkveehouderij (is not used) and rename sector to options in crop_maatregel
    mdb3 <- mdb3[sector != 'melkveehouderij_incl_mais_naast_gras']
    mdb3[grepl('melkv',sector),sector := 'melkveehouderij']
    
    # adjust soil type loss
    mdb3[grondsoort == 'loss', grondsoort := 'loess']
    
    # reset names and set key
    setnames(mdb3,'OBICvariable','indicator')
    setkey(mdb3,indicator,sector,grondsoort)
    
    # remove temporary databases
    rm(mdb1,mdb2,cols,cols_m,cols_i,ind.add,ind.miss,maatregel.obic,soils.obic,crops.obic)
    
  # Make a new datatable to store the effects of measures -----
    
    # add local copy of the input
    dt.recom <- copy(dt.score) 
    
    # melt the database
    cols_m <- colnames(dt.recom)[grepl('^I_C_|^I_P_|^I_B_',colnames(dt.recom))]
    cols_i <- colnames(dt.recom)[grepl('^B_|^ID$|soil|crop',colnames(dt.recom))]
    dt.recom <- melt(dt.recom,id.vars = cols_i,measure.vars = cols_m,variable.name = 'indicator', value.name='score')
    
    # join with weighing factor
    w[,indicator := gsub('W_','I_',var)]
    dt.recom <- merge(dt.recom,w[,c('indicator','weight')],by='indicator',all.x = TRUE)
    
    # reset names and set key
    setnames(dt.recom,c('crop_maatregel','soiltype.n'),c('sector','grondsoort'))
    setkey(dt.recom,indicator,sector,grondsoort)
    
  # join measures and calcualte score ------
    
    # join each parcel with possible measures (join by sector, soil type and indicator)
    dt.recom2 <- mdb3[dt.recom,allow.cartesian=TRUE]
    
    # filter only those measures where the score is below the thresshold value
    dt.recom2[, tresshold := fifelse(score <= Dremp_S & score >= 0,1,0)]
    
    # calculate the score of the measure for each indicator
    dt.recom2[,score.m := weight * app * (pmax(0,Ef_M_v) * tresshold + pmin(0,Ef_M_v))]
    
    # add three groups of soil functions
    dt.recom2[grepl('^I_C_',indicator), grp := 'M_S_C']
    dt.recom2[grepl('^I_B_',indicator), grp := 'M_S_B']
    dt.recom2[grepl('^I_P_',indicator), grp := 'M_S_P']
    
    # add priority to the score, just before calculating score per group
    dt.recom2[,score.mp := Prio_M * score.m]
    
    # extract relevant columns and dcast effect of measures on indices per parcel
    cols <- c('ID','maatregel_nr','indicator','score.m')
    dt.meas.ind <- dt.recom2[,mget(cols)]
    dt.meas.ind[, m.effect := gsub('I_','M_',indicator)]
    dt.meas.ind[, score.m := round(score.m,2)]
    dt.meas.ind <- dcast(dt.meas.ind,ID + maatregel_nr ~ m.effect, value.var = 'score.m')
    
    # calculate the total score for each measure, and count the number of indices exceeding thresshold
    dt.meas.tot <- dt.recom2[,lapply(.SD,sum),.SDcols = c('score.mp','tresshold'),by=.(ID,maatregel_nr,grp)]
    setnames(dt.meas.tot,c('ID','maatregel_nr','grp','FS','TH'))
    dt.meas.tot <- dcast(dt.meas.tot,ID + maatregel_nr ~ grp, value.var = c('FS','TH'))
    
    # combine total score and individual scores per measure and parcel
    dt.final <- merge(dt.meas.ind,dt.meas.tot,by=c('ID','maatregel_nr'))
    
    # terurn final db
    return(dt.final)
}


#' Recommend measurements for better soil managment
#' 
#' This function gives recommendations better soil managament based on the OBI score
#' 
#' @param dt.recom (data.table) 
#' @param extensive (boolean) whether the output table includes evaluation scores of each  measures (TRUE) or only names of top 3 measures
#' 
#' @import data.table
#' 
#' @export
obic_recommendations <- function(dt.recom, extensive = FALSE) {
  
 
  
  return(dt.recom)
  
}