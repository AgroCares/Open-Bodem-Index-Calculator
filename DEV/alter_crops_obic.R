# Script to modify crops.obic
library(data.table)

# load the original OBIC csv file from version 0.11.0
cr <- fread('dev/crops_obic_start.csv')
cr <- cr[,V1 := NULL]

# --- update crops for 2020 ---------------

  # update done at 10-april 2021

  # update crop codes to 2020
  crops2020 <- as.data.table(fread('dev/brp_crops_2020.csv'))
  
  # Identify missing crop codes
  miss_codes <- unique(crops2020[!crop_code %in% cr$crop_code])
  setnames(miss_codes, c('crop_name', 'crop_code'))
  miss_codes[,crop_code := as.numeric(crop_code)]
  miss_codes <- miss_codes[!is.na(crop_code)]
  
  # update list with new crop codes from 2020
  cr <- rbindlist(list(cr, miss_codes), fill = TRUE)
  
  # convert to lower case
  org_names <- cr[,.(crop_name, crop_code)]
  cr[,crop_name := tolower(crop_name)]
  
  # Add crop groups for crop_crumblebility (only done for new crop codes)
  
    # group 1. graslandvernieuwing, graszaad
    cr[grepl('roodzwenk',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 1L]
  
    # group 2. granen, mais, natuur
    
    # group 3. (poot)aardappelen
    
    # group 4. Fabrieksaardappelen, witlof, selderij, uien, prei,spruiten, koolsoorten
    cr[grepl('venkel|uien',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 4L]
    
    # group 5. cons.aardappelen, peen, schorseneren, teelten onder glas
    # group 6. suikerbieten
    # group 7. erwten,bonen, koolsoorten
    # group 8. asperge
    # group 9. bladgewassen
  
    # group 10. dahliaâ€™s, hyacinten
    cr[grepl('hyacint',crop_name) & !grepl('droogbloem',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 10L]
    
    # group 11. bloembollen (najaarsplanting)
    # group 12. groot en klein fruit
   
    # group 13. bos- en haagplantsoen
    cr[grepl('^bomen|^boom|^bos|^griend|bosje$|^struweel|^windhaag|^knotboom|^houtwal|elzensingel',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 13L]
    
    # group 14. sierteelten
    cr[grepl('chrysant|iris|droogbloem|zonnekroon',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 14L]
    
    # group 15. laanbomen en onderstammen
    cr[grepl('laan',crop_name) & is.na(crop_crumbleability), crop_crumbleability := 15L]
    
    # group 16. overig
    cr[is.na(crop_crumbleability), crop_crumbleability := 16L]
    
  
  # Add crop_category (used for groups given soil analysis fertilizer recommendation manual)
  # these include: akkerbouw, mais, grasland, natuur
  
    # group akkerbouw
    cr[grepl('chrysant|boomgaard|droogbloem|zaden|magi|komkommer|klaver|venkel',crop_name) & is.na(crop_category), crop_category := 'akkerbouw']
    cr[grepl('kuifhyacint|palmen|seradelle|sierui|spurrie|^uien|voedselbos|vrouwenmantel',crop_name) & is.na(crop_category), crop_category := 'akkerbouw']
    cr[grepl('marjolein|rijst|zonnekroon',crop_name) & is.na(crop_category), crop_category := 'akkerbouw']
    
    # group mais
    
    # group grasland
    cr[grepl('zwenkgras',crop_name) & is.na(crop_category), crop_category := 'grasland']
    
    # group natuur
    cr[grepl('bossingel|bomenrij|azolla|boomgroepen|bosje|knotboom|landschap|laan|lisdodde',crop_name) & is.na(crop_category), crop_category := 'natuur']
    cr[grepl('natuur|^riet|singel|^dracht|^houtwal|scheerheg|wandelpad|water|struweel|windhaag|^niger',crop_name) & is.na(crop_category), crop_category := 'natuur']
    cr[grepl('griendje|schurveling',crop_name) & is.na(crop_category), crop_category := 'natuur']
    
  
  # Add crop waterstress
    
    # group overig boomteelt
    # group bloembollen
    cr[grepl('^iris|^krokus|^lelie',crop_name) & is.na(crop_waterstress), crop_waterstress := 'bloembollen']
    
    # group groot fruit
    # group granen
    # group zomergroenten
    cr[grepl('^komkommer|^knolvenkel|^uien',crop_name) & is.na(crop_waterstress), crop_waterstress := 'zomergroenten']
    
    # group suikerbiet
    # group snijmais
    # group grasland met herinzaai
    # group natuur
    cr[grepl('bossingel|bomenrij|azolla|boomgroepen|bosje|knotboom|landschap|laan|lisdodde',crop_name) & is.na(crop_waterstress), crop_waterstress := 'natuur']
    cr[grepl('natuur|^riet|singel|^dracht|^houtwal|scheerheg|wandelpad|water|struweel|windhaag|^niger',crop_name) & is.na(crop_waterstress), crop_waterstress := 'natuur']
    cr[grepl('griendje|schurveling|vrouwen',crop_name) & is.na(crop_waterstress), crop_waterstress := 'natuur']
    
    # group boomteelt
    cr[grepl('^hoogstamboom|^palmen|voedselbos',crop_name) & is.na(crop_waterstress), crop_waterstress := 'boomteelt']
    
    # group overig boomteelt
    cr[grepl('^chrysant|^kuifhyacint|maggi|^sierui|marjolein|rijst|zante',crop_name) & is.na(crop_waterstress), crop_waterstress := 'overig boomteelt']
    
    # group aardappel
    # group klein fruit
    # group bladgroenten
    # group grasland zonder herinzaai
    cr[grepl('roodzwenkgras',crop_name) & is.na(crop_waterstress), crop_waterstress := 'grasland zonder herinzaai']
    
    # group wintergroenten
    # group overig
    cr[grepl('azolla|^bomenrij|klaver|niger|seradelle|spurrie|zonnekroon',crop_name) & is.na(crop_waterstress), crop_waterstress := 'overig']
  
  # Add crop intensity: options overig, rust, rooivrucht
    
    # group rust
    cr[grepl('roodzwenkgras|klaver|spurrie|niger|zonnekroon|rijst',crop_name) & is.na(crop_intensity), crop_intensity := 'rust']
    
    # group rooivrucht
    # group overig
    cr[is.na(crop_intensity), crop_intensity := 'overig']
    
  # Add crop intensity: 
  # options "other","cereal","sugarbeet","alfalfa","mais","grass","nature","catchcrop", "clover","starch","potato","rapeseed"
    
    # group nature
    cr[grepl('bossingel|bomenrij|azolla|boomgroepen|bosje|knotboom|landschap|laan|lisdodde',crop_name) & is.na(crop_intensity), crop_intensity := 'natuur']
    cr[grepl('natuur|^riet|singel|^dracht|^houtwal|scheerheg|wandelpad|water|struweel|windhaag|^niger',crop_name) & is.na(crop_intensity), crop_intensity := 'natuur']
    cr[grepl('griendje|schurveling|vrouwen',crop_name) & is.na(crop_intensity), crop_intensity := 'natuur']
    
    # group clover
    cr[grepl('klaver',crop_name) & is.na(crop_intensity), crop_intensity := 'clover']
  
    # group grass
    cr[grepl('roodzwenk',crop_name) & is.na(crop_intensity), crop_intensity := 'grass']
    
    # group other
    cr[is.na(crop_intensity), crop_intensity := 'other']
    
    
  # add group for phosphate soil class: arable, mais, gras, nature
    
    # group arable
    cr[grepl('droogbloem|chrysant|klaver|knolvenk|komkom|bloembol|maggi|^palmen|^uien',crop_name) & is.na(crop_phosphate), crop_phosphate := 'arable']
    cr[grepl('spurrie|voedselbos|marjolein|zonnekroon|rijst',crop_name) & is.na(crop_phosphate), crop_phosphate := 'arable']
    
    # group mais
    
    # group gras
    cr[grepl('roodzwenk',crop_name) & is.na(crop_phosphate), crop_phosphate := 'gras']
    
    # group nature
    cr[is.na(crop_phosphate), crop_phosphate := 'nature']
    
  
  # Add crop sealing
    
    # group gras
    cr[grepl('roodzwenk',crop_name) & is.na(crop_sealing), crop_sealing := 'gras']
    
    # group overig
    cr[is.na(crop_sealing), crop_sealing := 'overig']
    
  
  # Add crop n
    
    # group gras
    cr[grepl('roodzwenk',crop_name) & is.na(crop_n), crop_n := 'gras']
    
    # group akkerbouw
    cr[is.na(crop_n), crop_n := 'akkerbouw']
  
  # Add crop k
  
    # group gras
    cr[grepl('roodzwenk',crop_name) & is.na(crop_k), crop_k := 'gras']
    
    # group mais
    cr[is.na(crop_k), crop_k := 'mais']
  
  # Add crop measure
    
    # group akkerbouw
    cr[grepl('klaver|maggi|roodzwenk|seradelle|spurrie',crop_name) & is.na(crop_measure), crop_measure := 'akkerbouw']
    
    # group boomteelt
    cr[grepl('^palmen|voedselbos',crop_name) & is.na(crop_measure), crop_measure := 'boomteelt']
    
    # group groenteteelt
    cr[grepl('droogbloemen|komkommer|knolvenkel|bloembol|^uien|marjolein|rijst',crop_name) & is.na(crop_measure), crop_measure := 'groenteteelt']
    
    # group veeteelt
    cr[is.na(crop_measure), crop_measure := 'veeteelt']
    
  
  
  # Add EOS to non nature crops
    
    # identical to boomgaarden
    cr[grepl('voedelbos|^hoogstam',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(1175,0)]
    
    # als Dahlia (zelfde familie/onderfamilie)
    cr[grepl('^chrysant',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(750,0)]
  
    # als iris, bloembollen en knollen
    cr[grepl('^iris',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(400,0)]
    
    # als andere klavers (behalve rolklaver)
    cr[grepl('^klaver',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(1200,0)]
    
    # misschien als peen??
    cr[grepl('^knolvenkel',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(700,0)]
    
    # als Augurk, zaden en opkweekmateriaal
    cr[grepl('^komkommer',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(250,0)]
    
    # als Krokus, bloembollen en - knollen
    cr[grepl('^krokus',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(150,0)]
    
    # als Hyacint of lelie of sierui
    cr[grepl('^kuifhyacint',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(350,0)]
    cr[grepl('^lelie',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(450,0)]
    cr[grepl('^sierui',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(500,0)]
    cr[grepl('^uien',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(300,0)]  
  
    # als pot- en container planten
    cr[grepl('^palmen',crop_name) & is.na(crop_eos),c('crop_eos','crop_eos_residue') := list(0,0)]
    
    # unknown: maggi, niger, roodzwenk, rijst, spurrie, seradelle, zonnekroon
   
  # Add nitrogen use normes (stikstofgebruiksnormen)
    nfcols <- c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat')
  
    cr[grepl('^azolla|^lisdodde|^schurveling|wandelpad|^bomen|^boom|^bos|^griend|bosje$|^struweel|^windhaag|knotboom|^houtwal|elzensingel|landschapselement|oever|^poel|^riet|wandelpad,|water|scheerheg|^laan$', crop_name)&is.na(nf_clay),
       c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(0, 0, 0, 0,0)]
    cr[grepl('^chrysant|^iris|^kroku|^kuifhyacint|^lelie|^sierui', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(150, 150, 150, 150,150)]
    cr[grepl('^drachtplanten', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(150, 140, 112, 112,145)]
    cr[grepl('^hoogstamboomgaard', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(135, 105, 105, 105,105)]
    cr[grepl('^klaver', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(30, 25, 25, 25,30)]
    cr[grepl('^^knolvenkel', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(180, 165, 132, 132, 170)]
    cr[grepl('^komkommer', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(190, 175, 140, 140,180)]
    cr[grepl('^kuifhyacint.*bloembollen|^sierui', crop_name), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(165, 155, 155, 155,155)]
    cr[grepl('maggi.*zaden', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(100, 90, 72, 72,92)]
    cr[grepl('^niger|^seradelle', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(200, 185, 148, 148, 190)] # stengel/knol/wortegewassen, overig
    cr[grepl('^palmen', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(80, 80, 80, 80,80)]
    cr[grepl('^roodzwenkgras', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(85, 75, 60, 60,80)] #uncertain
    cr[grepl('^uien', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(120, 120, 120, 120,120)]
    cr[grepl('^spurrie', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(60, 50, 50, 50,50)]
    cr[grepl('^vrouwenmantel', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(150, 140, 112, 112,145)] # aangenomen als Kruiden, bladgewas, eenmalige oogst
    cr[grepl('^wilde marjolein', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(100, 90, 72, 72,95)]
    cr[grepl('^wilde rijst', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(150, 150, 150, 150,150)] # buitenbloemen overig
    cr[grepl('^zantedeschia', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(120, 120, 120, 120,120)]
    cr[grepl('^zonnekroon', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(185, 140, 112, 112, 150)] # als mais zonder derogatie
    cr[grepl('^voedselbos', crop_name)&is.na(nf_clay), c('nf_clay', 'nf_sand.other', 'nf_sand.south', 'nf_loess', 'nf_peat') := list(110, 110, 110, 110, 110)]
# --- add scientific name of crop species ---------------
    
  # update done at 10-april-21
    
  # load data
  # load('data/crops_obic.RData')
  # cr <- as.data.table(crops.obic)
    
  # Add scientific name of crop species
  cr[grepl('ardappel', crop_name),crop_name_scientific:= 'solanum tuberosum']
  cr[grepl('uinbouwzaden|bloemzaden|kwekerijgewassen|^bloembollen en - knollen$|fruit|oomkwekerij|rand', crop_name),crop_name_scientific:= 'overig']
  cr[grepl('tarwe', crop_name),crop_name_scientific:= 'triticum aestivum']
  cr[grepl('gerst', crop_name),crop_name_scientific:= 'horderum vulgare']
  cr[grepl('rogge', crop_name),crop_name_scientific:= 'secale cereale']
  cr[grepl('haver', crop_name),crop_name_scientific:= 'avena sativa']
  cr[grepl('erwten|schokkers|eulen', crop_name),crop_name_scientific:= 'pisum sativum']
  cr[grepl('bonen', crop_name),crop_name_scientific:= 'phaseolus vulgaris']
  cr[grepl('pronkbonen', crop_name),crop_name_scientific:= 'phaseolus coccineus'] #overwrites bonen for pronkbonen
  cr[grepl('sojabonen', crop_name),crop_name_scientific:= 'glycine max'] #overwrite bonen for sojabonen
  cr[grepl('karwij|kummel', crop_name),crop_name_scientific:= 'carum carvi']
  cr[grepl('maanzaad', crop_name),crop_name_scientific:= 'papaver somniferum']
  cr[grepl('vlas', crop_name),crop_name_scientific:= 'linum usitatissimum']
  cr[grepl('bieten', crop_name),crop_name_scientific:= 'beta vulgaris']
  cr[grepl('luzerne', crop_name),crop_name_scientific:= 'medicago sativa']
  cr[grepl('mais', crop_name),crop_name_scientific:= 'zea mays']
  cr[grepl('^uien|ierui', crop_name),crop_name_scientific:= 'allium cepa']
  cr[grepl('grasland|graszaad|blijvend gras$|tijdelijk gras$|raszoden', crop_name),crop_name_scientific:= 'gras overig']
  cr[grepl('engels raai', crop_name),crop_name_scientific:= 'lolium perenne']
  cr[grepl('italiaans raai|esterwolds|, italiaans', crop_name),crop_name_scientific:= 'lolium multiflorum']
  cr[grepl('sorghum|sorgho', crop_name),crop_name_scientific:= 'sorghum bicolor']
  cr[grepl('rietzwenk|rietzwenk', crop_name),crop_name_scientific:= 'Festuca arundinacea']
  cr[grepl('miscanthus', crop_name),crop_name_scientific:= 'miscanthus']
  cr[grepl('roodzwenk|roodzwenk', crop_name),crop_name_scientific:= 'festuca rubra']
  cr[grepl('eldbeemdgras|veldbeemd', crop_name),crop_name_scientific:= 'poa pratensis']
  cr[grepl('triticale', crop_name),crop_name_scientific:= 'triticale']
  cr[grepl('atuur|ufferstrook|sloot|overige', crop_name)&is.na(crop_name_scientific),crop_name_scientific:= 'overig']
  cr[grepl('argetes|frikaantje', crop_name),crop_name_scientific:= 'targetes']
  cr[grepl('hop', crop_name),crop_name_scientific:= 'humulus lupus']
  cr[grepl('teff', crop_name),crop_name_scientific:= 'eragrostis tef']
  cr[grepl('spelt', crop_name),crop_name_scientific:= 'triticum spelta']
  cr[grepl('gele mosterd', crop_name),crop_name_scientific:= 'sinapis alba']
  cr[grepl('zwarte mos', crop_name),crop_name_scientific:= 'brassica nigra']
  cr[grepl('ethiopische mos', crop_name),crop_name_scientific:= 'brassica carinata']
  cr[grepl('sarepta mosterd', crop_name),crop_name_scientific:= 'brassica juncea']
  cr[grepl('cichorei|lof', crop_name),crop_name_scientific:= 'chichorium intybus']
  cr[grepl('ndijvie', crop_name),crop_name_scientific:= 'chichorium endivia']
  cr[grepl('anariezaad', crop_name),crop_name_scientific:= 'phalaris canariensis']
  cr[grepl('onnebloem', crop_name),crop_name_scientific:= 'helianthus anuus']
  cr[grepl('saffloer', crop_name),crop_name_scientific:= 'carthamus tinctorius']
  cr[grepl('meekrap', crop_name),crop_name_scientific:= 'rubia tinctorum']
  cr[grepl('teunisbloem', crop_name),crop_name_scientific:= 'oenothera']
  cr[grepl('brandnetel', crop_name),crop_name_scientific:= 'urtica urens']
  cr[grepl('boekweit', crop_name),crop_name_scientific:= 'fagopyrum esculentum']
  cr[grepl('gierst', crop_name),crop_name_scientific:= 'panicum miliaceum']
  cr[grepl('bos|lijnzaad|oenten|bomen|ilgen|klaverzaad|heester|planten', crop_name),crop_name_scientific:= 'overig']
  cr[grepl('peen', crop_name),crop_name_scientific:= 'daucus carota'] # overwrites bos for bospeen
  cr[grepl('lupinen', crop_name),crop_name_scientific:= 'lupinus']
  cr[grepl('aapzaad', crop_name),crop_name_scientific:= 'brassica rapa']
  cr[grepl('zwaardherik', crop_name),crop_name_scientific:= 'Eruca sativa']
  cr[grepl('raketblad', crop_name),crop_name_scientific:= 'solanum sisymbriifolium']
  cr[grepl('klaver, rode', crop_name),crop_name_scientific:= 'trifolium pratense']
  cr[grepl('klaver, witte', crop_name),crop_name_scientific:= 'trifolium repens']
  cr[grepl('olklaver', crop_name),crop_name_scientific:= 'lotus']
  cr[grepl('esparcette', crop_name),crop_name_scientific:= 'onobrychis viciifolia']
  cr[grepl('wikke, voeder', crop_name),crop_name_scientific:= 'vicia sativa']
  cr[grepl('wikke, bonte', crop_name),crop_name_scientific:= 'vicia villosa']
  cr[grepl('ennep', crop_name),crop_name_scientific:= 'cannabis sativa']
  cr[grepl('ahlia', crop_name),crop_name_scientific:= 'dahlia']
  cr[grepl('ladiool', crop_name),crop_name_scientific:= 'gladiolus communis']
  cr[grepl('hyacint', crop_name),crop_name_scientific:= 'hyacinthus orientalis']
  cr[grepl('kuifhyacint', crop_name),crop_name_scientific:= 'leopoldia comosa']
  cr[grepl('iris|iris', crop_name),crop_name_scientific:= 'iris germanica']
  cr[grepl('rokus', crop_name),crop_name_scientific:= 'crocus']
  cr[grepl('lelie', crop_name),crop_name_scientific:= 'lilium']
  cr[grepl('arcis', crop_name),crop_name_scientific:= 'narcissus']
  cr[grepl('tulp', crop_name),crop_name_scientific:= 'tulipa']
  cr[grepl('antedeschia', crop_name),crop_name_scientific:= 'zantedeschia aethiopica']
  cr[grepl('maryllis', crop_name),crop_name_scientific:= 'amaryllis belladonna']
  cr[grepl('lauw druifje', crop_name),crop_name_scientific:= 'muscari botryoides']
  cr[grepl('aleriaan', crop_name),crop_name_scientific:= 'valeriana officinalis']
  cr[grepl('knoflook', crop_name),crop_name_scientific:= 'allium sativum']
  cr[grepl('quinoa', crop_name),crop_name_scientific:= 'chenopodium quinoa']
  cr[grepl('pastinaak', crop_name),crop_name_scientific:= 'pastinaca sativa']
  cr[grepl('pioenroos', crop_name),crop_name_scientific:= 'paeonia']
  cr[grepl('lavas', crop_name),crop_name_scientific:= 'levisticum officinale']
  cr[grepl('oregano', crop_name),crop_name_scientific:= 'origanum vulgare']
  cr[grepl('goudsbloem', crop_name),crop_name_scientific:= 'calendula officinalis']
  cr[grepl('igniscum candy', crop_name),crop_name_scientific:= 'polygonaceae']
  cr[grepl('aaldaar', crop_name),crop_name_scientific:= 'setaria']
  cr[grepl('eterselie', crop_name),crop_name_scientific:= 'petroselinum crispum']
  cr[grepl('rysant', crop_name),crop_name_scientific:= 'chrysanthemum']
  cr[grepl('elica', crop_name),crop_name_scientific:= 'angelica']
  cr[grepl('apaver', crop_name),crop_name_scientific:= 'papaver']
  cr[grepl('donis', crop_name),crop_name_scientific:= 'adonis']
  cr[grepl('nietje', crop_name),crop_name_scientific:= 'myosotis']
  cr[grepl('ranberry', crop_name),crop_name_scientific:= 'vaccinium macrocarpon']
  cr[grepl('zonnehoed', crop_name),crop_name_scientific:= 'echinacea']
  cr[grepl('eeuwenbek', crop_name),crop_name_scientific:= 'antirrhinum']
  cr[grepl('uxus', crop_name),crop_name_scientific:= 'buxus']
  cr[grepl('ricaceae', crop_name),crop_name_scientific:= 'ericaceae']
  cr[grepl('rozen', crop_name),crop_name_scientific:= 'rosa']
  cr[grepl('coniferen', crop_name),crop_name_scientific:= 'coniferales']
  cr[grepl('ppelen\\.', crop_name),crop_name_scientific:= 'malus']
  cr[grepl('eren\\.', crop_name),crop_name_scientific:= 'pyrus']
  cr[grepl('ijndruiven', crop_name),crop_name_scientific:= 'vitis vinifera']
  cr[grepl('cultuurgrond|aunaranden|fruit|nijgroen|opgegeven|dummy|sbl|beteelde|bemester|eide|element|ruiden|enrij', crop_name),crop_name_scientific:= 'overig']
  cr[grepl('azelno', crop_name),crop_name_scientific:= 'corylus avellana']
  cr[grepl('alnoten', crop_name),crop_name_scientific:= 'juglans regia']
  cr[grepl('essen, blauwe', crop_name),crop_name_scientific:= 'vaccinium corymbosum']
  cr[grepl('ruimen|ersen', crop_name),crop_name_scientific:= 'prunus']
  cr[grepl('essen, zwarte', crop_name),crop_name_scientific:= 'ribes nigrum']
  cr[grepl('oolzaad', crop_name),crop_name_scientific:= 'brassica napus']
  cr[grepl('agetes', crop_name),crop_name_scientific:= 'tagetes']
  cr[grepl('ardperen$', crop_name),crop_name_scientific:= 'helianthus tuberosus']
  cr[grepl('vlinderbloemige', crop_name),crop_name_scientific:= '']
  cr[grepl('essen, rode', crop_name),crop_name_scientific:= 'ribes rubrum']
  cr[grepl('rambozen', crop_name),crop_name_scientific:= 'rubus idaeus']
  cr[grepl('ramen', crop_name),crop_name_scientific:= 'rubus']
  cr[grepl('ardbeien', crop_name),crop_name_scientific:= 'fragaria x ananassa']
  cr[grepl('sperges', crop_name),crop_name_scientific:= 'asparagus officinalis']
  cr[grepl('kool|rocolli|roccoli|rabi', crop_name),crop_name_scientific:= 'brassica oleracea']
  cr[grepl('raap|inese kool|aksoi|aapstelen|oppelknollen', crop_name),crop_name_scientific:= 'brassica rapa']
  cr[grepl('ourgette', crop_name),crop_name_scientific:= 'cucurbita pepo']
  cr[grepl('elderij', crop_name),crop_name_scientific:= 'apium graveolens']
  cr[grepl('enkel', crop_name),crop_name_scientific:= 'foeniculum vulgare']
  cr[grepl('omkommer|gurk', crop_name),crop_name_scientific:= 'cucumis sativus']
  cr[grepl('eloen|ompoen', crop_name),crop_name_scientific:= 'cucurbita'] # soortnieveau van meloenen en pompoen wordt niet bijgehouden in brp
  cr[grepl('abarber', crop_name),crop_name_scientific:= 'rheum rhabarbarum']
  cr[grepl('prei', crop_name),crop_name_scientific:= 'allium ampeloprasum']
  cr[grepl('adijs', crop_name),crop_name_scientific:= 'raphanus sativus']
  cr[grepl('chorseneren', crop_name),crop_name_scientific:= 'scorzonera hispanica']
  cr[grepl('^sla', crop_name),crop_name_scientific:= 'lactuca sativa']
  cr[grepl('inazie', crop_name),crop_name_scientific:= 'espinaca oleracea']
  cr[grepl('lexandrijnse', crop_name),crop_name_scientific:= 'trifolium alexandrinum']
  cr[grepl('eemdlangbloem', crop_name),crop_name_scientific:= 'festuca pratensis']
  cr[grepl('rammenas', crop_name),crop_name_scientific:= 'raphanus sativus']
  cr[grepl('deder', crop_name),crop_name_scientific:= 'camelina sativa']
  cr[grepl('acelia', crop_name),crop_name_scientific:= 'phacelia']
  cr[grepl('ranse boekweit', crop_name),crop_name_scientific:= 'fagopyrum tataricum']
  cr[grepl('incarnaat', crop_name),crop_name_scientific:= 'trifolium incarnatum']
  cr[grepl('imothee', crop_name),crop_name_scientific:= 'phleum pratense']
  cr[grepl('zolla', crop_name),crop_name_scientific:= 'azolla']
  cr[grepl('knotboom|historisch|houtsingel|oogstamboomgaar|bosje|oomgroepen|singel|riendje|scheerheg|laan|almen, |rietzoom|^riet$|zandwallen|struweel|oedselbos|boerenland|water', crop_name),crop_name_scientific:= 'overig']
  cr[grepl('erzische', crop_name),crop_name_scientific:= 'trifolium resupinatum']
  cr[grepl('isdodde', crop_name),crop_name_scientific:= 'typhaceae']
  cr[grepl('niger$', crop_name),crop_name_scientific:= 'guizotia abyssinica']
  cr[grepl('eradelle', crop_name),crop_name_scientific:= 'ornithopus sativus']
  cr[grepl('spurrie', crop_name),crop_name_scientific:= 'spergula']
  cr[grepl('rouwenmantel', crop_name),crop_name_scientific:= 'alchemila']
  cr[grepl('ilde rijst$', crop_name),crop_name_scientific:= 'zizania']
  cr[grepl('onnekroon', crop_name),crop_name_scientific:= 'silphium perfoliatum']
  cr[grepl('festulolium', crop_name),crop_name_scientific:= 'festulolium']

  # update the csv and Rdata file
  # crops.obic <- copy(cr)
  # fwrite(crops.obic, 'dev/crops_obic.csv')
  # save(crops.obic, file = 'data/crops_obic.RData')
  
  
# add crop_season for workability -----
  
  # update done at 10-april-21
  
  # load data
  # load('data/crops_obic.RData')
  # cr <- as.data.table(crops.obic)

  # add categories
  cr[grepl('fruit|peren|appel|steenvruchten|ruimen|ersen|noten', crop_name), crop_season := 'groot fruit']
  cr[grepl('kleinfruit|aardbeien|boos|essen|ramen|rambozen|ijndruiven', crop_name), crop_season := 'klein fruit']
  cr[grepl('^aardappelen', crop_name), crop_season := 'aardappelen']
  cr[grepl('^aardappelen.*zetmeel', crop_name), crop_season := 'fabrieksaardappelen']
  cr[grepl('^aardappelen.*poot|ardappelras', crop_name), crop_season := 'pootaardappelen'] # voor pootaardeappelen met (loofver na 15-08) moet misschien een andere datum worden aangehouden
  cr[grepl('ieten.*suiker|ieten.*voeder|ardperen|kroten', crop_name), crop_season := 'suikerbieten'] # Seizoen voor voederbieten komt enigzins overeen medio maart/begin april - begin november
  cr[grepl('^tarwe.*winter|inter.*arwe|^gerst.*winter', crop_name), crop_season := 'wintertarwe']
  cr[grepl('^gerst.*zomer|erst.*omer|arwe.*zomer|haver|rogge|spelt|teff|triticale|ierst|oekweit', crop_name), crop_season := 'zomergerst']
  cr[grepl('ais|ojabonen', crop_name), crop_season := 'snijmais'] # snijmais en sojabonen hebben enigzins vergelijkbare zaai en oogst data in NL
  cr[grepl('aanbo.*grond|stammen.*grond', crop_name), crop_season := 'laanbomen_onderstammen']
  cr[grepl('oom|omen', crop_name)&is.na(crop_season), crop_season := 'overige boomteelt']
  cr[grepl('kerstb', crop_name), crop_season := 'naaldbos']
  cr[grepl('graszaad', crop_name), crop_season := 'graszaad']
  cr[grepl('sperge', crop_name), crop_season := 'asperge']
  cr[grepl('chorseneren', crop_name), crop_season := 'schorseneren']
  cr[grepl('prei|pruit|winterpeen', crop_name)|grepl('brassica rapa|brassica oleracea', crop_name_scientific), crop_season := 'prei, spruiten, koolsoorten']
  cr[grepl('rwten|onen|chokkers|eulen', crop_name) & is.na(crop_season), crop_season := 'erwten, bonen'] 
  cr[grepl('lof|elderij|knoflook|chorei', crop_name)|crop_name_scientific == 'allium cepa', crop_season := 'witlof, selderij, uien'] #assumed cichorei has similair dates to witlof
  cr[grepl('aspeen|ospeen', crop_name), crop_season := 'was bospeen']
  cr[grepl('ulp|arcis|hyacint', crop_name), crop_season := 'tulpen, narcis, hyacint']
  cr[grepl('ahlia', crop_name), crop_season := 'dahlia']
  cr[grepl('^sla|pinazie|dijvie', crop_name), crop_season := 'bladgroenten']
  cr[grepl('grasland.*blijvend|grasland.*tijdelijk|raaigras$|^veldbeemdgras$|grasland.*begraasd|grasland.*landbouw', crop_name), crop_season := 'beweid bemaaid gras']
  cr[grepl('open.*grond', crop_name)&is.na(crop_season), crop_season := 'overige boomteelt']
  # Assumed these crops need short seasons
  cr[crop_waterstress== 'zomergroenten' & is.na(crop_season), crop_season := 'witlof, selderij, uien'] 
  cr[grepl('uifhyacint|bloem|rysant|roos', crop_name)&!grepl('ahlia|bemester',crop_name)|crop_waterstress == 'bloembollen'&!grepl('ahlia',crop_name),
     crop_season := 'tulpen, narcis, hyacint']
  cr[grepl('bos[ ,-js]|bos', crop_name), crop_season := 'loofbos']
  cr[is.na(crop_season), crop_season := 'overig']

  # Correction on Spelt waterstress
  cr[crop_name == 'spelt', crop_waterstress := 'granen']

  # update the csv and Rdata file
  # replace crop names with original capitalised names
  cr[,crop_name := NULL]
  cr <- merge.data.table(org_names, cr, by='crop_code')
  crops.obic <- copy(cr)
  fwrite(crops.obic, 'dev/crops_obic.csv')
  save(crops.obic, file = 'data/crops_obic.RData')
  
  