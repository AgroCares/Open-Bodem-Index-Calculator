# make recom_obic_bkp.Rdata

  # load library
  library(data.table);library(usethis)
  
  # load maatregel csv
  maatregel.obic  <- fread("DEV/data/maatregel_effect.csv")
  maatregel.bkp  <- fread("DEV/data/maatregel_effect_bkp.csv")
  
  # filter and format
  maatregel.bkp  <- maatregel.bkp[filter == 1]
  maatregel.obic <- maatregel.obic[filter == 1 & !is.na(OBICvariable)][,Ef_M_pre := NULL]
  
  setnames(maatregel.obic,'Bodemfunctie / indicator','m_soilfunction')
  maatregel.obic[OBICvariable == 'I_P_WRI', m_soilfunction := 'waterregulatie']
  
  
  # combine maatregel.obic and maatrgel.bkp
  maatregel.combi <- rbind(maatregel.obic,maatregel.bkp,fill = TRUE)
  
  # make template to make all combinations of measrue x indicators
  mat <- data.frame(table(maatregel_nr = maatregel.combi$maatregel_nr, OBICvariable = maatregel.combi$OBICvariable))
  setDT(mat)
  
  # filter out combinationof measure x indicators do not occur in maatregel.combi
  maatregel.missing <- mat[Freq == 0][, Freq := NULL]
  maatregel.missing[,maatregel_nr := as.numeric(maatregel_nr)]
  
  # Fill in missing columns
  maatregel.missing <- merge(maatregel.missing,unique(maatregel.combi[,list(maatregel_nr,omschrijving,Prio_M,Order)]),by = 'maatregel_nr', all.x = TRUE)
  maatregel.missing <- merge(maatregel.missing,unique(maatregel.combi[,list(OBICvariable,m_soilfunction)]),by = 'OBICvariable', all.x = TRUE)
  
  maatregel.missing[,c('Dremp_S','filter') := list(0.6,1)]
  
  cols <- colnames(maatregel.combi)[!(colnames(maatregel.combi) %in% colnames(maatregel.missing))]
  maatregel.missing[,c(cols) := 0]
  
  # Make total table
  maatregel.total <- rbind(maatregel.combi,maatregel.missing)
  
  # check if all combinations of measures x indicators are included
  mat <- data.frame(table(maatregel_nr = maatregel.total$maatregel_nr, OBICvariable = maatregel.total$OBICvariable))
  if(length(mat$Freq[mat$Freq != 1]) > 0){
    print(paste('There are missing values or duplicates in the effect of measures!')) # give a warning message
  }
  
  # convert '=' and '-' to points
  maatregel.total[Ef_M == "+++", Ef_M_v := 3]
  maatregel.total[Ef_M == "++", Ef_M_v := 2]
  maatregel.total[Ef_M == "+", Ef_M_v := 1]
  maatregel.total[Ef_M == "0", Ef_M_v := 0]
  maatregel.total[Ef_M == "nr", Ef_M_v := 0]
  maatregel.total[Ef_M == "-", Ef_M_v := -1]
  maatregel.total[Ef_M == "--", Ef_M_v := -2]
  
  # remove unnecessary columns
  maatregel.total[, c('filter') := NULL]
  
  # remove all indicators that are not present
  m.maatregel <- maatregel.total[!is.na(OBICvariable)]
  
  # check if there is missing data for any indicator
  column_description.obic <- readRDS('dev/data/column_description_obic.rds')
  cols <- column_description.obic[grepl('^I_C_|^I_P_|^I_B_|^I_W',column),column]
  
  # make temporary data.table with for each unknown indicator a zero impact measure
  cols <- cols[!cols %in% unique(m.maatregel$OBICvariable)]
  ind.miss <- CJ(maatregel_nr = 1:max(m.maatregel$maatregel_nr), OBICvariable = cols)
  ind.miss <- merge(ind.miss,
                    column_description.obic[,.(OBICvariable = column,m_soilfunction = description_nl)],
                    by = 'OBICvariable')
  ind.add  <- merge(ind.miss,unique(m.maatregel[,.(maatregel_nr,omschrijving,Prio_M,Order)]),
                    by='maatregel_nr')
  
  # add missing columns with value 0, and a tresshold value of 0.5
  cols <- colnames(m.maatregel)[grepl('melk|groente|akker|boom|klei|veen|zand|loess|Ef',colnames(m.maatregel))]
  ind.add[,c(cols) := 0]
  ind.add[,Dremp_S := 0.5]
  
  # rbind these new values with the original measures table
  m.maatregel <- rbind(m.maatregel,ind.add,fill = T)
  
  # reshape m.maatregel: agricultural sector from columns to rows
  cols_m <- colnames(m.maatregel)[grepl('melkvee|akker|groente|boom',colnames(m.maatregel))]
  cols_i <- colnames(m.maatregel)[!colnames(m.maatregel) %in% cols_m]
  m.maatregel <- melt(m.maatregel,id.vars = cols_i, measure.vars = cols_m, variable.name = 'sector',
                      value.name = 'app1',variable.factor = FALSE)
  
  # reshape m.maatregel: soil type from columns to rows
  cols_m <- colnames(m.maatregel)[grepl('klei|veen|zand|loess',colnames(m.maatregel))]
  cols_i <- colnames(m.maatregel)[!colnames(m.maatregel) %in% cols_m]
  m.maatregel <- melt(m.maatregel,id.vars = cols_i, measure.vars = cols_m, variable.name = 'grondsoort',
                      value.name = 'app2',variable.factor = FALSE)
  
  # take combined applicability for sector and soil type 
  m.maatregel[,app := pmin(app1,app2)]
  m.maatregel[,c('app1','app2') := NULL]
  
  # remove one option for melkveehouderij (is not used) and rename sector to options in crop_maatregel
  m.maatregel <- m.maatregel[sector != 'melkveehouderij_incl_mais_naast_gras']
  m.maatregel[grepl('melkv',sector),sector := 'veeteelt']
  
  # adjust soil type loss
  m.maatregel[grondsoort == 'loss', grondsoort := 'loess']
  
  # delete categorial effect
  m.maatregel[,Ef_M := NULL]
  
  # adapt names
  setnames(m.maatregel,c('m_nr','m_description','m_prio','m_threshold','m_order','m_soilfunction','indicator','m_effect','m_sector','m_soiltype','m_applicability'))
  
  # adapt descriptions
  m.maatregel[m_sector =='groente',m_sector := 'groenteteelt']
  
  # setkey
  setkey(m.maatregel,indicator,m_sector,m_soiltype)
  
  # update march 2022
  
  # there where three OBIC indicators added, now with default "no impact
  # these include: I_B_NEM, I_P_DS, I_P_WS and I_P_WO
  
  # drought stress and wetness stress: all measures for original moisture stress are applicable
  # and moisture stress can be deleted
  m1.copy <- m.maatregel[indicator=='I_P_MS'][,indicator := 'I_P_DS']
  m2.copy <- m.maatregel[indicator=='I_P_MS'][,indicator := 'I_P_WS']
  
  # add to m.maatregel
  m.maatregel <- m.maatregel[!indicator %in% c('I_P_DS','I_P_WS','I_P_MS')]
  m.maatregel <- rbind(m.maatregel,m1.copy,m2.copy)
  
  # add workability, assuming similar impacts on workability as on wetness stress
  m1.copy <- m.maatregel[indicator=='I_P_WS'][,indicator := 'I_P_WO']
  m.maatregel <- rbind(m.maatregel[!indicator == 'I_P_WO'],m1.copy)
  
  # setkey
  setkey(m.maatregel,indicator,m_sector,m_soiltype)
  
  # test
  if(length(m.maatregel[,.N,by=.(indicator)][,unique(N)]) != 1){
    print("warning, there are differences in the number of indicators. that should not be possible. please check")
  }
  
  # Update recommendationd descriptions
  new_descriptions <- fread('DEV/data/maatregel_description_bkp.csv')
  m.maatregel <- merge(m.maatregel,new_descriptions, by = 'm_description')
  
  m.maatregel[,m_description := new_description][,new_description := NULL]
  
  # rename to recom.maatregel
  recom.obic_bkp <- m.maatregel

usethis::use_data(recom.obic_bkp,overwrite = TRUE)
devtools::document()
  
