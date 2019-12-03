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
  soiltype = soiltype.m = crop_measure = crop_code = ID = m_sector = m_soiltyp = NULL
  indicator = NULL
  m_effect = threshold = score.m = weight = grp = score.mp = m_prio = m.effect = FS = TH = NULL
  
  # make local copy of dt.score
  dt.score <- copy(dt.score)
  
  # add local databases for joining properties -----
  
    # load weighing factors needed for integration
    w <- as.data.table(OBIC::weight.obic)
    
    # load db for measures, crop and soil categories
    m.obic <- as.data.table(OBIC::recom.obic)
    crops.obic <- as.data.table(OBIC::crops.obic)
    soils.obic <- as.data.table(OBIC::soils.obic)
    
    # merge dt.score with soils.obic and crops.obic
    dt.score <- merge(dt.score, soils.obic[, list(soiltype, soiltype.m)], by.x = "B_BT_AK", by.y = "soiltype")
    dt.score <- merge(dt.score, crops.obic[, list(crop_code, crop_measure)], by.x = "B_LU_BRP", by.y = "crop_code")
    
  # redesign measures db to make it suitable for joining -----
  
    # drop indicators from measure db that are not present in dt.score
    mdb1 <- m.obic[indicator %in% colnames(dt.score)]
    
    # drop descriptive columns that are not needed
    mdb1[,c('m_description','m_soilfunction') := NULL]
    
    # check if there is missing data for any indicator
    cols <- colnames(dt.score)[grepl('^I_C_|^I_B_|^I_P_',colnames(dt.score))]
    cols <- cols[!cols %in% unique(mdb1$indicator)]
    checkmate::assert_character(cols, len = 0)
    
    # setkeys
    setkey(mdb1,indicator,m_sector,m_soiltype)
    
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
    setnames(dt.recom,c('crop_measure','soiltype.m'),c('m_sector','m_soiltype'))
    setkey(dt.recom,indicator,m_sector,m_soiltype)
    
  # join measures and dt.score ------
  
    # join each parcel with possible measures (join by sector, soil type and indicator)
    dt.recom2 <- mdb1[dt.recom,allow.cartesian=TRUE]
    
    # add parameter whether score is below threshold yes or no
    dt.recom2[, threshold := fifelse(score <= m_threshold & score >= 0,1,0)]
    
    # calculate the score of the measure for each indicator
    dt.recom2[, score.m := weight * m_applicability * (pmax(0,m_effect) * threshold + pmin(0,m_effect))]
    
    # add three groups of soil functions
    dt.recom2[grepl('^I_C_',indicator), grp := 'M_S_C']
    dt.recom2[grepl('^I_B_',indicator), grp := 'M_S_B']
    dt.recom2[grepl('^I_P_',indicator), grp := 'M_S_P']
    
    # add priority to the score, just before calculating score per group
    dt.recom2[,score.mp := m_prio * score.m]
    
    # extract relevant columns and dcast effect of measures on indices per parcel
    cols <- c('ID','m_nr','indicator','score.m')
    dt.meas.ind <- dt.recom2[,mget(cols)]
    dt.meas.ind[, m.effect := gsub('I_','M_',indicator)]
    dt.meas.ind[, score.m := round(score.m,2)]
    dt.meas.ind <- dcast(dt.meas.ind,ID + m_nr ~ m.effect, value.var = 'score.m')
    
    # calculate the total score for each measure, and count the number of indices exceeding thresshold
    dt.meas.tot <- dt.recom2[,lapply(.SD,sum),.SDcols = c('score.mp','threshold'),by=c('ID','m_nr','grp')]
    setnames(dt.meas.tot,c('ID','m_nr','grp','FS','TH'))
    dt.meas.tot <- dcast(dt.meas.tot,ID + m_nr ~ grp, value.var = c('FS','TH'))
    
    # combine total score and individual scores per measure and parcel
    dt.final <- merge(dt.meas.ind,dt.meas.tot,by=c('ID','m_nr'))
    
    # add priority for those situations that measures have equal score
    dt.final <- merge(dt.final,unique(mdb1[,c('m_nr','m_order')]),by='m_nr')
    
    # terurn final db
    return(dt.final)
}


#' Recommend measurements for better soil managment
#' 
#' This function gives recommendations better soil management based on the OBI score
#' 
#' @param dt.recom (data.table) 
#' @param extensive (boolean) whether the output table includes evaluation scores of each  measures (TRUE) or only names of top 3 measures
#' 
#' @import data.table
#' 
#' @export
obic_recommendations <- function(dt.recom, extensive = FALSE) {
  
  # Check inputs
  checkmate::assert_data_table(dt.recom)
  
  # set variables as NULL
  Order = maatregel_nr = ID = sid = m.adv = NULL
  TH_M_S_C = TH_M_S_P = TH_M_S_B = NULL
  FS_M_S_C = FS_M_S_P = FS_M_S_B = NULL
  
  # make local copy of input data.table
  dt.final <- copy(dt.recom)
  
  # sort all measures in dt.final for each parcel for chemical score (FS_M_S_C)
  dt.chem <- dt.final[order(-FS_M_S_C,m_order),list(m_nr,FS_M_S_C,TH_M_S_C,m_order),by='ID']
  # add an id for all measures
  dt.chem[,sid := seq_len(.N),by='ID']
  # select only the top-3 measures with the highest score
  dt.chem <- dt.chem[sid<4]
  # add a character for the recommended measure
  dt.chem[,m.adv := paste0('M',m_nr)]
  # when the score of selected measures is <=0, discard the advice 
  dt.chem[FS_M_S_C==0, m.adv := 'no suitable advice']
  # when no indicator is below the threshold level, give no advice.
  dt.chem[TH_M_S_C==0, m.adv := 'no advice needed']
  # dcast the recommended measures for each parcel
  dt.chem <- dcast(dt.chem, ID ~ sid, value.var = 'm.adv')
  # rename the columns with recommeded measures
  setnames(dt.chem,c('ID',paste0('RM_C_',1:3)))   
  
  # similar evaluation for physical properties
  dt.phys <- dt.final[order(-FS_M_S_P,m_order),list(m_nr,FS_M_S_P,TH_M_S_P,m_order),by='ID']
  dt.phys[,sid := seq_len(.N),by='ID']
  dt.phys <- dt.phys[sid<4]
  dt.phys[,m.adv := paste0('M',m_nr)]
  dt.phys[FS_M_S_P==0, m.adv := 'no suitable advice']
  dt.phys[TH_M_S_P==0, m.adv := 'no advice needed']
  dt.phys <- dcast(dt.phys, ID ~ sid, value.var = 'm.adv')
  setnames(dt.phys,c('ID',paste0('RM_P_',1:3)))   
  
  # similar evaluation for biological properties
  dt.biol <- dt.final[order(-FS_M_S_B,m_order),list(m_nr,FS_M_S_B,TH_M_S_B,m_order),by='ID']
  dt.biol[,sid := seq_len(.N),by='ID']
  dt.biol <- dt.biol[sid<4]
  dt.biol[,m.adv := paste0('M',m_nr)]
  dt.biol[FS_M_S_B==0, m.adv := 'no suitable advice']
  dt.biol[TH_M_S_B==0, m.adv := 'no advice needed']
  dt.biol <- dcast(dt.biol, ID ~ sid, value.var = 'm.adv')
  setnames(dt.biol,c('ID',paste0('RM_B_',1:3)))   
  
  # join the different recommendations
  out <- merge(dt.chem,dt.phys,by='ID')
  out <- merge(out,dt.biol,by='ID')
  
  if(extensive == FALSE){
    # add effect of each measure on all indicators
    out <- merge(dt.final,out,by='ID')
  }
  
  return(out)
  
}

#' applicability range of measures including literature based estimates of effect on soil indicators
#' 
#' This table defines the effects of 11 measures on soil indicators. 
#' This table is used internally in \code{\link{obic_evalmeasure}}
#' 
"maatregel.obic"