test_that('All OBIC tables with B_GWL_CLASS have the same values in them.', {
  ## find tables with B_GWL_CLASS column ====
  # dynamic file location
  tloc <- if(length(list.files("../testdata/"))>0){"../../data/"} else {'data/'}
  
  tbls <- gsub('\\.rda', '',list.files(path = tloc))
  
  gwl_tabs <- c() # empty vector to store tables
  
  # go through all OBIC objects, check if they are tables and have the right column
  for(tab in tbls){
    # load object
    load(paste0(tloc, tab, '.rda'))
    
    if(is.data.frame(get(tab)) &! tab %in% c('binnenveld')){ # binnenveld can be skipped as it is not referenced by functions
      # get tab colnames
      tab_cnames <- names(get(tab))
      
      if('B_GWL_CLASS' %in% tab_cnames){
        gwl_tabs <- c(gwl_tabs, tab)
      }
    }
  }
  
  expect_true(all(unique(nleach_table$B_GWL_CLASS) %in% waterstress.obic$B_GWL_CLASS),
              label = glue::glue('Not all nleach_table$B_GWL_CLASS are in waterstress.obic$B_GWL_CLASS.
                                 nleach_table$B_GWL_CLASS has additional elements:\n
                                 {list(unique(nleach_table$B_GWL_CLASS[!nleach_table$B_GWL_CLASS %in% waterstress.obic$B_GWL_CLASS]))}'))
  expect_true(all(unique(waterstress.obic$B_GWL_CLASS) %in% nleach_table$B_GWL_CLASS),
              label = glue::glue('Not all waterstress.obic$B_GWL_CLASS are in nleach_table$B_GWL_CLASS.
                                 waterstress.obic$B_GWL_CLASS has additional elements:\n
                                 {list(unique(waterstress.obic$B_GWL_CLASS[!waterstress.obic$B_GWL_CLASS %in% nleach_table$B_GWL_CLASS]))}'))
  
})