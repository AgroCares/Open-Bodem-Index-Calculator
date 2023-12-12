# prepare ppr_waterstress table

# load package
library(data.table)
require(XML)
library(httr)

# load in soil database
soils <- fread('data/internal_data/raw/bodemopties_helptabel.csv')

# load crop_id
crops <- data.table(gewasnr = sort(rep(1:14,12)))

# groundwater table, input definition
gt <- data.table(ghg <- c(20,20,20,20,35,35,60,60,60,100,100,150),
                 glg <- c(45,60,100,150,100,150,100,150,200,150,200,200),
                 gt <- c('GtI','GtII','GtIII','GtV','GtIII','GtV','GtIV','GtVI','GtVI','GtVII','GtVII','GtVIII'),
                 gto <-c('GtI','GtIIa','GtIIIa','GtVa','GtIIIb','GtVo','GtIVu','GtVIo','GtVId','GtVIIo','GtVIId','GtVIIId'))

# make database
db <- cbind(crops,gt)
db <- cbind(soil = rep(soils$bodemcode_ref,nrow(db)),db)
setnames(db,c('soil','crop','ghg','glg','gtnew','gtold'))

# add url
db[,url := paste0("http://help200x.alterra.nl/queryajax.aspx?crop=",crop,"&soil=",soil,"&ghg=",ghg,"&glg=",glg)]

# add estimated water related yield depression
db[,natschade := NA_real_]
db[,droogteschade := NA_real_]
db[,combischade := NA_real_]

# extract rsults from Alterra api
for(i in 1:nrow(db)){
  req <- GET(db$url[i], timeout(60000))
  if (req$status_code == 200) {
    body <- content(req)
    r1 = htmlTreeParse(body)
    r2 = suppressWarnings(xmlValue(r1$children$html))
    a = strsplit(r2,'selectie')[[1]][2]
    b = strsplit(a,':|%')[[1]][c(3,5,7)]
    b[is.na(b)] = -999
    b = as.numeric(b)
    set(db,i,'natschade',b[1])
    set(db,i,'droogteschade',b[2])
    set(db,i,'combischade',b[3])
  } else {
    req <- GET(db$url[i], timeout(10000))
    if (req$status_code == 200) {
      body <- content(req)
      r1 = htmlTreeParse(body)
      r2 = xmlValue(r1$children$html)
      a = strsplit(r2,'selectie')[[1]][2]
      b = strsplit(a,':|%')[[1]][c(3,5,7)]
      b[is.na(b)] = -999
      b = as.numeric(b)
      set(db,i,'natschade',b[1])
      set(db,i,'droogteschade',b[2])
      set(db,i,'combischade',b[3])
    }
  }
  print(i)
}

# remove the url from the database
db[,c('url','gtold') := NULL]

# reset column names
setnames(db,c('soilunit','croptype','ghg','glg','droughtstress','wetnessstress','waterstress'))

# crop definition table
crops <- data.table(gewas = c('aardappel','bladgroenten','bloembollen','boomteelt','overig boomteelt',
                              'granen','grasland met herinzaai','grasland zonder herinzaai',
                              'groot fruit','klein fruit','grove zomergroenten',
                              'wintergroenten','snijmais','suikerbieten'),
                    # gewasnaam in obic package
                    cropname = c('aardappel','bladgroenten','bloembollen','boomteelt','overig boomteelt',
                                 'granen','grasland met herinzaai','grasland zonder herinzaai',
                                 'groot fruit','klein fruit','zomergroenten',
                                 'wintergroenten','snijmais','suikerbieten'),
                    gewasnr = c(3,8,10,13,14,5,2,1,11,12,6,7,9,4))
# merge
db <- merge(db,crops,by.x = 'croptype',by.y = 'gewasnr')

# remove two columns
db[,gewas:=NULL]
db[,croptype:=NULL]
db[,c('ghg','glg'):=NULL]

# replace missing values with NA
db[droughtstress == -999, droughtstress :=NA]
db[wetnessstress == -999, wetnessstress  :=NA]
db[waterstress == -999, waterstress:=NA]

# setname
setnames(db,'gtnew','gt')

# setorder
setcolorder(db,c('cropname','soilunit','gt'))

# calculate mean per soil unit, gt and crop type
cols <- c('droughtstress','wetnessstress','waterstress')
db[,lapply(.SD,mean,na.rm=TRUE),by=.(cropname,soilunit,gt), .SDcols=cols]
db[,(cols) := lapply(.SD,function(x) fifelse(!is.finite(x),NA,x)),.SDcols=cols]

# make new table
waterstress.obic <- db

# save the file
usethis::use_data(waterstress.obic, version = 3, overwrite = TRUE, compress = 'xz')




