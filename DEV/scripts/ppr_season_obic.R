# Script to make season.obic table
 
  # require package
  library(data.table)

  # Table is based on Tabel 2 in Huinink (2018) and links to crops.obic$crop_season
  # Not all landuses in the original table appear in brp and some of the brp crops have been assigned to "overig" and could be reclassified later.
  season.obic <- fread('dev/data/season_obic.csv')
  
  # save table
  save(season.obic,file = 'data/season.obic.RData')
