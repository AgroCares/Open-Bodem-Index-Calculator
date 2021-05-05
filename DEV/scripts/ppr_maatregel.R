# make recom_obic.Rdata

  # load library
  library(data.table)

  # load maatregel csv
  maatregel.obic  <- fread("dev/data/maatregel_effect.csv")

  # --- maatregel tabel preprocessed by Sven in november 2019 ---

    # filter
    maatregel.obic  <- maatregel.obic[filter == 1]
    
    # check if all combinations of measures x indicators are included
    mat <- data.frame(table(maatregel.obic$maatregel_nr, maatregel.obic$OBICvariable))
    if(length(mat$Freq[mat$Freq != 1]) > 0){
      print(paste('There are missing values or duplicates in the effect of measures!')) # give a warning message
    }
    
    # convert '=' and '-' to points
    maatregel.obic[Ef_M == "+++", Ef_M_v := 3]
    maatregel.obic[Ef_M == "++", Ef_M_v := 2]
    maatregel.obic[Ef_M == "+", Ef_M_v := 1]
    maatregel.obic[Ef_M == "0", Ef_M_v := 0]
    maatregel.obic[Ef_M == "nr", Ef_M_v := 0]
    maatregel.obic[Ef_M == "-", Ef_M_v := -1]
    
    # remove unnecessary columns
    maatregel.obic[, c('filter', 'Ef_M_pre') := NULL]

  # maatregel tabel preprocessed by Gerard in november 2019 ---
  
    # remove all indicators that are not present
    m.obic <- maatregel.obic[!is.na(OBICvariable)]
    m.obic <- m.obic[!OBICvariable %in% c('I_C_CU, I_C_ZN','I_B_X')]
    m.obic <- m.obic[!is.na(Dremp_S)]
    
    # replace two NA values in database (reason why missing is unknown)
    cols <- colnames(m.obic)[grepl('melk',colnames(m.obic))]
    m.obic[,(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = cols]
    
    # check if there is missing data for any indicator
    load('dev/column_description_obic.RData')
    cols <- column_description.obic[grepl('^I_C_|^I_P_|^I_B_',column),column]
    
    # make temporary data.table with for each unknown indicator a zero impact measure
    cols <- cols[!cols %in% unique(m.obic$OBICvariable)]
    ind.miss <- CJ(maatregel_nr = 1:max(m.obic$maatregel_nr), OBICvariable = cols)
    ind.add  <- merge(unique(m.obic[,c(1:3,5:6)]),ind.miss,by='maatregel_nr')
    
    # add missing columns with value 0, and a tresshold value of 0.5
    cols <- colnames(m.obic)[grepl('melk|groente|akker|boom|klei|veen|zand|loss|Ef',colnames(m.obic))]
    ind.add[,c(cols) := 0]
    ind.add[,Dremp_S := 0.5]
    
    # rbind these new values with the original measures table
    m.obic <- rbind(m.obic,ind.add,fill = T)
    
    # reshape m.obic: agricultural sector from columns to rows
    cols_m <- colnames(m.obic)[grepl('melkvee|akker|groente|boom',colnames(m.obic))]
    cols_i <- colnames(m.obic)[!colnames(m.obic) %in% cols_m]
    m.obic <- melt(m.obic,id.vars = cols_i, measure.vars = cols_m, variable.name = 'sector',
                 value.name = 'app1',variable.factor = FALSE)
    
    # reshape m.obic: soil type from columns to rows
    cols_m <- colnames(m.obic)[grepl('klei|veen|zand|loss',colnames(m.obic))]
    cols_i <- colnames(m.obic)[!colnames(m.obic) %in% cols_m]
    m.obic <- melt(m.obic,id.vars = cols_i, measure.vars = cols_m, variable.name = 'grondsoort',
                 value.name = 'app2',variable.factor = FALSE)
    
    # take combined applicability for sector and soil type 
    m.obic[,app := pmin(app1,app2)]
    m.obic[,c('app1','app2') := NULL]
    
    # remove one option for melkveehouderij (is not used) and rename sector to options in crop_maatregel
    m.obic <- m.obic[sector != 'melkveehouderij_incl_mais_naast_gras']
    m.obic[grepl('melkv',sector),sector := 'veeteelt']
    
    # adjust soil type loss
    m.obic[grondsoort == 'loss', grondsoort := 'loess']
    
    # delete categorial effect
    m.obic[,Ef_M := NULL]
    
    # adapt names
    setnames(m.obic,c('m_nr','m_description','m_prio','m_threshold','m_order','m_soilfunction','indicator','m_effect','m_sector','m_soiltype','m_applicability'))
    
    # adapt descriptions
    m.obic[m_sector =='boomteelt',m_sector := 'boomteelt']
    m.obic[m_sector =='akkerbouw',m_sector := 'akkerbouw']
    m.obic[m_sector =='groente',m_sector := 'groenteteelt']
    
    # setkey
    setkey(m.obic,indicator,m_sector,m_soiltype)
  
# update april 2021
  
  # I_P_MS is splitted into droughtstress and wetness stress separately
  m1.copy <- m.obic[indicator=='I_P_MS'][,indicator := 'I_P_DS']
  m2.copy <- m.obic[indicator=='I_P_MS'][,indicator := 'I_P_WS']
  
  # add to m.obic
  m.obic <- rbind(m.obic[!indicator=='I_P_MS'],m1.copy,m2.copy)
  
  # setkey
  setkey(m.obic,indicator,m_sector,m_soiltype)
  
  # rename to recom.obic
  recom.obic <- m.obic
  
save(recom.obic, file = "data/recom_obic.RData")
