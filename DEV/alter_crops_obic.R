# Script to modify crops.obic
library(data.table)

cr <- fread('dev/crops_obic_start.csv')
cr <- cr[,V1 := NULL]

# update crop codes to 2020
crops2020 <- as.data.table(fread('DEV/brp_crops_2020.csv'))

# Identify missing crop codes
miss_codes <- unique(crops2020[!crop_code %in% cr$crop_code])
setnames(miss_codes, c('crop_name', 'crop_code'))
miss_codes$crop_code <- as.numeric(miss_codes$crop_code)
miss_codes <- miss_codes[!is.na(crop_code)]

cr <- rbindlist(list(cr, miss_codes), fill = TRUE)
rm(crops2020)
rm(miss_codes)

# Add crop groups from eval.crumbeability to crops.obic$crop_crumblebility
cr[crop_name %in% c(), crop_crumbleability := 1L] # 1 graslandverniuwing, graszaad
cr[crop_name %in% c('Roodzwenkgras'), crop_crumbleability := 2L] # 2 granen, mais, natuur
cr[crop_name %in% c(), crop_crumbleability := 3L] # 3 (poot)aardappelen
cr[crop_name %in% c('Knolvenkel/venkel, zaden en opkweekmateriaal', 'Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars'), crop_crumbleability := 4L] # 4 Fabrieksaardappelen, witlof, selderij, uien, prei,spruiten, koolsoorten
cr[crop_name %in% c(), crop_crumbleability := 5L] # 5 cons.aardappelen, peen, schorseneren, teelten onder glas
cr[crop_name %in% c(), crop_crumbleability := 6L] # 6 suikerbieten
cr[crop_name %in% c(), crop_crumbleability := 7L] # 7 erwten,bonen, koolsoorten
cr[crop_name %in% c(), crop_crumbleability := 8L] # 8 asperge
cr[crop_name %in% c(), crop_crumbleability := 9L] # 9 bladgewassen
cr[crop_name %in% c('Kuifhyacint, bloembollen en -knollen'), crop_crumbleability := 10L] # 10 dahliaâ€™s, hyacinten
cr[crop_name %in% c(), crop_crumbleability := 11L] # 11 bloembollen (najaarsplanting)
cr[crop_name %in% c(), crop_crumbleability := 12L] # 12 groot en klein fruit
cr[crop_name %in% c('Bomenrij (anders dan knotboom)','Boomgroepen in het veld', 'Bosje', 'Bosssingel ','Bosssingel',
                            'Elzensingel', 'Geisoleerde boom (anders dan knotboom)', 'Griendje', 'Hakhoutbosje',
                            'Houtwal en houtsingel', 'Knotboom, bomen in rij', 'Knotboom, geisloleerde boom',
                            'Laan', 'Struweelhaag', 'Struweelrand', 'Voedselbos', 'Windhaag , in een perceel fruitteelt'
), crop_crumbleability := 13L] # 13 bos- en haagplantsoen
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering', 'Iris, droogbloemen', 
                            'Krokus, droogbloemen', 'Kuifhyacint, droogbloemen', 'Lelie, droogbloemen',
                            'Sierui, droogbloemen', 'Zantedeschia, droogbloemen', 'Zonnekroon'), crop_crumbleability := 14L] # 14 sierteelten
cr[crop_name %in% c(), crop_crumbleability := 15L] # 15 laanbomen en onderstammen
cr[crop_name %in% c('Azolla', 'Drachtplanten', 'Hoogstamboomgaard', 'Klaver, Perzische', 'Knip- of scheerheg',
                            'Komkommer, zaden en opkweekmateriaal', 'Landschapselement, overig', 'Lavas (Maggiplant), zaden en opkweekmateriaal',
                            'Lisdodde', 'Natuurvriendelijke oever','Niger', 'Palmen, pot- en containervelden', 'Poel en klein historisch water',
                            'Riet', 'Rietzoom en klein rietperceel', 'Schurvelingen en zandwallen', 'Seradelle', 'Spurrie', 'Vrouwenmantel',
                            'Wandelpad over boerenland', 'Water, overig', 
                            'Wilde marjolein (Oregano), zaden en opkweekmateriaal', 'Wilde rijst'), crop_crumbleability := 16L] # 16 overig

# Add crop_category
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering', 'Hoogstamboomgaard', 'Iris, droogbloemen',
                            'Klaver, Perzische', 'Knolvenkel/venkel, zaden en opkweekmateriaal', 'Komkommer, zaden en opkweekmateriaal',
                            'Krokus, droogbloemen',  'Kuifhyacint, bloembollen en -knollen', 'Kuifhyacint, droogbloemen',
                            'Lavas (Maggiplant), zaden en opkweekmateriaal', 'Lelie, droogbloemen', 'Palmen, pot- en containervelden',
                            'Seradelle', 'Sierui, droogbloemen', 'Spurrie', 'Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars', 'Voedselbos', 'Vrouwenmantel',
                            'Wilde marjolein (Oregano), zaden en opkweekmateriaal', 'Wilde rijst', 'Zantedeschia, droogbloemen',
                            'Zonnekroon'), crop_category := 'akkerbouw'] #  akkerbouw
cr[crop_name %in% c(), crop_category := 'mais'] #  mais
cr[crop_name %in% c('Roodzwenkgras'), crop_category := 'grasland'] #  grasland
cr[crop_name %in% c('Azolla', 'Bomenrij (anders dan knotboom)', 'Boomgroepen in het veld', 'Bosje',
                            'Bosssingel ','Bosssingel','Drachtplanten', 'Elzensingel', 'Geisoleerde boom (anders dan knotboom)',
                            'Griendje', 'Hakhoutbosje', 'Houtwal en houtsingel','Knip- of scheerheg', 'Knotboom, bomen in rij',
                            'Knotboom, geisloleerde boom', 'Laan', 'Landschapselement, overig', 'Lisdodde', 'Natuurvriendelijke oever',
                            'Niger', 'Poel en klein historisch water','Riet', 'Rietzoom en klein rietperceel',
                            'Schurvelingen en zandwallen', 'Struweelhaag', 'Struweelrand', 'Wandelpad over boerenland',
                            'Water, overig', 'Windhaag , in een perceel fruitteelt'), crop_category := 'natuur'] #  natuur

# Add crop waterstress
cr[crop_name %in% c('Azolla', 'Bomenrij (anders dan knotboom)' ,'Klaver, Perzische', 'Niger', 'Seradelle',
                            'Spurrie', 'Zonnekroon'), crop_waterstress := 'overig']
cr[crop_name %in% c('Bomenrij (anders dan knotboom)', 'Boomgroepen in het veld', 'Bosje', 'Bosssingel ','Bosssingel', 'Drachtplanten',
                            'Elzensingel', 'Geisoleerde boom (anders dan knotboom)', 'Griendje','Hakhoutbosje',
                            'Houtwal en houtsingel', 'Knip- of scheerheg', 'Knotboom, bomen in rij', 'Knotboom, geisloleerde boom',
                            'Laan', 'Landschapselement, overig', 'Lisdodde', 'Natuurvriendelijke oever', 'Poel en klein historisch water', 'Riet',
                            'Rietzoom en klein rietperceel','Schurvelingen en zandwallen', 'Struweelhaag', 'Struweelrand',
                            'Vrouwenmantel', 'Wandelpad over boerenland', 'Water, overig', 'Windhaag , in een perceel fruitteelt'
), crop_waterstress := 'natuur']
cr[crop_name %in% c('Iris, droogbloemen', 'Krokus, droogbloemen','Lelie, droogbloemen'), crop_waterstress := 'bloembollen']
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering','Kuifhyacint, bloembollen en -knollen', 'Kuifhyacint, droogbloemen',
                            'Lavas (Maggiplant), zaden en opkweekmateriaal','Sierui, droogbloemen', 
                            'Wilde marjolein (Oregano), zaden en opkweekmateriaal', 'Wilde rijst', 'Zantedeschia, droogbloemen'), crop_waterstress := 'overig boomteelt']
cr[crop_name %in% c('Hoogstamboomgaard', 'Palmen, pot- en containervelden', 'Voedselbos'), crop_waterstress := 'boomteelt']
cr[crop_name %in% c('Knolvenkel/venkel, zaden en opkweekmateriaal', 'Komkommer, zaden en opkweekmateriaal',
                            'Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars'), crop_waterstress := 'zomergroenten']
cr[crop_name %in% c('Roodzwenkgras'), crop_waterstress := 'grasland zonder herinzaai']

# Add crop intensity
cr[crop_name %in% c('Klaver, Perzische', 'Roodzwenkgras', 'Spurrie', 'Niger', 'Zonnekroon', 'Wilde rijst'), crop_intensity := 'rust']
cr[is.na(cr$crop_intensity), crop_intensity := 'overig']

# Add crop_rotation
cr[crop_name %in% c('Bomenrij (anders dan knotboom)', 'Boomgroepen in het veld', 'Bosje', 'Bosssingel ', 'Bosssingel', 'Drachtplanten',
                            'Elzensingel', 'Geisoleerde boom (anders dan knotboom)', 'Griendje','Hakhoutbosje',
                            'Houtwal en houtsingel', 'Knip- of scheerheg', 'Knotboom, bomen in rij', 'Knotboom, geisloleerde boom',
                            'Laan', 'Landschapselement, overig', 'Lisdodde', 'Natuurvriendelijke oever', 'Poel en klein historisch water', 'Riet',
                            'Rietzoom en klein rietperceel','Schurvelingen en zandwallen', 'Struweelhaag', 'Struweelrand',
                            'Vrouwenmantel', 'Wandelpad over boerenland', 'Water, overig',
                            'Windhaag , in een perceel fruitteelt'), crop_intensity := 'nature']
cr[crop_name %in% c('Klaver, Perzische'), crop_intensity := 'clover']
cr[crop_name %in% c('Roodzwenkgras'), crop_intensity := 'grass']
cr[is.na(cr$crop_rotation), crop_rotation := 'other']

# Add crop phosphate
cr[crop_name %in% c('Roodzwenkgras'), crop_phosphate := 'gras']
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering', 'Iris, droogbloemen', 'Klaver, Perzische',
                            'Knolvenkel/venkel, zaden en opkweekmateriaal', 'Komkommer, zaden en opkweekmateriaal', 'Krokus, droogbloemen',
                            'Kuifhyacint, bloembollen en -knollen', 'Kuifhyacint, droogbloemen', 'Lavas (Maggiplant), zaden en opkweekmateriaal',
                            'Lelie, droogbloemen', 'Palmen, pot- en containervelden', 'Seradelle', 'Sierui, droogbloemen', 'Spurrie', 
                            'Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars', 'Voedselbos', 'Wilde marjolein (Oregano), zaden en opkweekmateriaal', 'Wilde rijst',
                            'Zonnekroon'), crop_phosphate := 'arable']
cr[is.na(crop_phosphate), crop_phosphate := 'nature'] # Everything that not grass, mais, or other production = nature

# Add crop sealing
cr[crop_name %in% c('Roodzwenkgras'), crop_sealing := 'gras']
cr[is.na(cr$crop_sealing), crop_sealing := 'overig']

# Add crop n
cr[crop_name %in% c('Roodzwenkgras'), crop_n := 'gras']
cr[is.na(cr$crop_n), crop_n := 'akkerbouw']

# Add crop k
cr[crop_name %in% c('Roodzwenkgras'), crop_k := 'gras']
cr[is.na(cr$crop_k), crop_k := 'mais']

# Add crop measure
cr[crop_name %in% c('Klaver, Perzische', 'Lavas (Maggiplant), zaden en opkweekmateriaal','Roodzwenkgras', 'Seradelle',
                            'Spurrie'), crop_measure := 'akkerbouw']
cr[crop_name %in% c('Palmen, pot- en containervelden', 'Voedselbos'), crop_measure := 'boomteelt']
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering', 'Iris, droogbloemen', 
                            'Knolvenkel/venkel, zaden en opkweekmateriaal', 'Komkommer, zaden en opkweekmateriaal','Krokus, droogbloemen',
                            'Kuifhyacint, bloembollen en -knollen', "Kuifhyacint, droogbloemen", 'Lelie, droogbloemen',
                            'Sierui, droogbloemen', 'Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars','Wilde marjolein (Oregano), zaden en opkweekmateriaal',
                            'Wilde rijst'), crop_measure := 'groenteteelt']
cr[is.na(cr$crop_measure), crop_measure := 'veeteelt']

# Add EOS to non nature crops
cr[crop_name %in% c('Voedselbos', 'Hoogstamboomgaard'), crop_eos := 1175] # net als boomgaarden
cr[crop_name %in% c('Voedselbos', 'Hoogstamboomgaard'), crop_eos_residue := 0]

cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering'), crop_eos := 750] # als Dahlia (zelfde familie/onderfamilie)
cr[crop_name %in% c('Chrysant, droogbloemen', 'Chrysant, vermeerdering'), crop_eos_residue := 0]

cr[crop_name %in% c('Iris, droogbloemen'), crop_eos := 400] # als iris, bloembollen en knollen
cr[crop_name %in% c('Iris, droogbloemen'), crop_eos_residue := 0]

cr[crop_name %in% c('Klaver, Perzische'), crop_eos := 1200] # als andere klavers (behalve rolklaver)
cr[crop_name %in% c('Klaver, Perzische'), crop_eos_residue := 0]

cr[crop_name %in% c('Knolvenkel/venkel, zaden en opkweekmateriaal'), crop_eos := NA] # MISSCHIEN als peen? 700
cr[crop_name %in% c('Knolvenkel/venkel, zaden en opkweekmateriaal'), crop_eos_residue := NA]

cr[crop_name %in% c('Komkommer, zaden en opkweekmateriaal'), crop_eos := 250] # als Augurk, zaden en opkweekmateriaal
cr[crop_name %in% c('Komkommer, zaden en opkweekmateriaal'), crop_eos_residue := 0]

cr[crop_name %in% c('Krokus, droogbloemen'), crop_eos := 150] # als Krokus, bloembollen en - knollen
cr[crop_name %in% c('Krokus, bloembollen en - knollen'), crop_eos_residue := 0]

cr[crop_name %in% c('Kuifhyacint, bloembollen en -knollen', 'Kuifhyacint, droogbloemen'), crop_eos := 350] # als Hyacint
cr[crop_name %in% c('Kuifhyacint, bloembollen en -knollen', 'Kuifhyacint, droogbloemen'), crop_eos_residue := 0]

# cr[crop_name %in% c('Lavas (Maggiplant), zaden en opkweekmateriaal'), crop_eos := NA] # als ??? (schermbloemige)
# cr[crop_name %in% c('Lavas (Maggiplant), zaden en opkweekmateriaal'), crop_eos_residue := NA]

cr[crop_name %in% c('Lelie, droogbloemen'), crop_eos := 450] # als andere Lelie
cr[crop_name %in% c('Lelie, droogbloemen'), crop_eos_residue := 0]

# cr[crop_name %in% c('Niger'), crop_eos := NA] # als ???
# cr[crop_name %in% c('Niger'), crop_eos_residue := NA]

cr[crop_name %in% c('Palmen, pot- en containervelden'), crop_eos := 0] # als pot- en container planten
cr[crop_name %in% c('Palmen, pot- en containervelden'), crop_eos_residue := 0]

# cr[crop_name %in% c('Roodzwenkgras', 'Wilde rijst'), crop_eos := NA] # als andere grassen maar welke? 2300 is voor meerjarig gras en graszaad maar dit is siergras
# cr[crop_name %in% c('Roodzwenkgras', 'Wilde rijst'), crop_eos_residue := NA]

cr[crop_name %in% c('Sierui, droogbloemen'), crop_eos := 500] # als sierui bollen en knollen
cr[crop_name %in% c('Sierui, droogbloemen'), crop_eos_residue := 0]

# cr[crop_name %in% c('Spurrie', 'Seradelle', 'Zonnekroon'), crop_eos := NA] # als ????
# cr[crop_name %in% c('Spurrie', 'Seradelle', 'Zonnekroon'), crop_eos_residue := NA]

cr[crop_name %in% c('Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars'), crop_eos := 300] # als Uien, poot en plant (incl. sjalotten)
cr[crop_name %in% c('Uien, poot en plant, 1e jaars ', 'Uien, poot en plant, 1e jaars'), crop_eos_residue := 0]

# cr[crop_name %in% c('Wilde marjolein (Oregano), zaden en opkweekmateriaal'), crop_eos := NA] # als andere klavers (behalve rolklaver)
# cr[crop_name %in% c('Wilde marjolein (Oregano), zaden en opkweekmateriaal'), crop_eos_residue := NA]

# cr[crop_name %in% c('Zantedeschia, droogbloemen'), crop_eos := NA] # als ??? Zuid-Afrikaanse sierplant
# cr[crop_name %in% c('Zantedeschia, droogbloemen'), crop_eos_residue := NA]


# Add scientific name of crop scecies =================================================================================================================
cr[grepl('ardappel', cr$crop_name),crop_name_scientific:= 'solanum tuberosum']
cr[grepl('uinbouwzaden|Bloemzaden open|kwekerijgewassen \\(inclusief bloemzaden\\)|^Bloembollen en - knollen$|Fruit|oomkwekerij|Rand', cr$crop_name),crop_name_scientific:= 'overig']
cr[grepl('Tarwe|tarwe', cr$crop_name),crop_name_scientific:= 'triticum aestivum']
cr[grepl('Gerst|gerst', cr$crop_name),crop_name_scientific:= 'horderum vulgare']
cr[grepl('Rogge|rogge', cr$crop_name),crop_name_scientific:= 'secale cereale']
cr[grepl('Haver|haver', cr$crop_name),crop_name_scientific:= 'avena sativa']
cr[grepl('erwten|Erwten|Schokkers|eulen', cr$crop_name),crop_name_scientific:= 'pisum sativum']
cr[grepl('Bonen|bonen', cr$crop_name),crop_name_scientific:= 'phaseolus vulgaris']
cr[grepl('Pronkbonen', cr$crop_name),crop_name_scientific:= 'phaseolus coccineus'] #overwrites bonen for pronkbonen
cr[grepl('Sojabonen', cr$crop_name),crop_name_scientific:= 'glycine max'] #overwrite bonen for sojabonen
cr[grepl('Karwij|karwij|kummel', cr$crop_name),crop_name_scientific:= 'carum carvi']
cr[grepl('maanzaad', cr$crop_name),crop_name_scientific:= 'papaver somniferum']
cr[grepl('Vlas|vlas', cr$crop_name),crop_name_scientific:= 'linum usitatissimum']
cr[grepl('Bieten|bieten', cr$crop_name),crop_name_scientific:= 'beta vulgaris']
cr[grepl('Luzerne', cr$crop_name),crop_name_scientific:= 'medicago sativa']
cr[grepl('Mais|mais', cr$crop_name),crop_name_scientific:= 'zea mays']
cr[grepl('Uien|uien|ierui', cr$crop_name),crop_name_scientific:= 'allium cepa']
cr[grepl('Grasland|grasland|^Graszaad$|blijvend gras$|Graszaad, overig|tijdelijk gras$|raszoden|Graszaad \\(inclusief|estulolium', cr$crop_name),crop_name_scientific:= 'gras overig']
cr[grepl('Engels raai', cr$crop_name),crop_name_scientific:= 'lolium perenne']
cr[grepl('Italiaans raai|esterwolds|, Italiaans', cr$crop_name),crop_name_scientific:= 'lolium multiflorum']
cr[grepl('Sorghum|sorgho', cr$crop_name),crop_name_scientific:= 'sorghum bicolor']
cr[grepl('Rietzwenk|rietzwenk', cr$crop_name),crop_name_scientific:= 'Festuca arundinacea']
cr[grepl('Miscanthus', cr$crop_name),crop_name_scientific:= 'miscanthus']
cr[grepl('Roodzwenk|roodzwenk', cr$crop_name),crop_name_scientific:= 'festuca rubra']
cr[grepl('eldbeemdgras|veldbeemd', cr$crop_name),crop_name_scientific:= 'poa pratensis']
cr[grepl('Triticale', cr$crop_name),crop_name_scientific:= 'triticale']
cr[grepl('atuur|ufferstrook|Sloot|Overige', cr$crop_name)&is.na(crop_name_scientific),crop_name_scientific:= 'overig']
cr[grepl('argetes|frikaantje', cr$crop_name),crop_name_scientific:= 'targetes']
cr[grepl('Hop', cr$crop_name),crop_name_scientific:= 'humulus lupus']
cr[grepl('Teff', cr$crop_name),crop_name_scientific:= 'eragrostis tef']
cr[grepl('Spelt', cr$crop_name),crop_name_scientific:= 'triticum spelta']
cr[grepl('Gele mosterd', cr$crop_name),crop_name_scientific:= 'sinapis alba']
cr[grepl('Zwarte mos', cr$crop_name),crop_name_scientific:= 'brassica nigra']
cr[grepl('Ethiopische mos', cr$crop_name),crop_name_scientific:= 'brassica carinata']
cr[grepl('Sarepta mosterd', cr$crop_name),crop_name_scientific:= 'brassica juncea']
cr[grepl('Cichorei|lof', cr$crop_name),crop_name_scientific:= 'chichorium intybus']
cr[grepl('ndijvie', cr$crop_name),crop_name_scientific:= 'chichorium endivia']
cr[grepl('anariezaad', cr$crop_name),crop_name_scientific:= 'phalaris canariensis']
cr[grepl('onnebloem', cr$crop_name),crop_name_scientific:= 'helianthus anuus']
cr[grepl('Saffloer', cr$crop_name),crop_name_scientific:= 'carthamus tinctorius']
cr[grepl('Meekrap', cr$crop_name),crop_name_scientific:= 'rubia tinctorum']
cr[grepl('Teunisbloem', cr$crop_name),crop_name_scientific:= 'oenothera']
cr[grepl('Brandnetel', cr$crop_name),crop_name_scientific:= 'urtica urens']
cr[grepl('Boekweit', cr$crop_name),crop_name_scientific:= 'fagopyrum esculentum']
cr[grepl('Gierst', cr$crop_name),crop_name_scientific:= 'panicum miliaceum']
cr[grepl('Bos|Lijnzaad|oenten|bomen|ilgen|Klaverzaad|heester|planten', cr$crop_name),crop_name_scientific:= 'overig']
cr[grepl('peen', cr$crop_name),crop_name_scientific:= 'daucus carota'] # overwrites bos for bospeen
cr[grepl('Lupinen', cr$crop_name),crop_name_scientific:= 'lupinus']
cr[grepl('aapzaad', cr$crop_name),crop_name_scientific:= 'brassica rapa']
cr[grepl('Zwaardherik', cr$crop_name),crop_name_scientific:= 'Eruca sativa']
cr[grepl('Raketblad', cr$crop_name),crop_name_scientific:= 'solanum sisymbriifolium']
cr[grepl('Klaver, rode', cr$crop_name),crop_name_scientific:= 'trifolium pratense']
cr[grepl('Klaver, witte', cr$crop_name),crop_name_scientific:= 'trifolium repens']
cr[grepl('olklaver', cr$crop_name),crop_name_scientific:= 'lotus']
cr[grepl('Esparcette', cr$crop_name),crop_name_scientific:= 'onobrychis viciifolia']
cr[grepl('Wikke, voeder', cr$crop_name),crop_name_scientific:= 'vicia sativa']
cr[grepl('Wikke, bonte', cr$crop_name),crop_name_scientific:= 'vicia villosa']
cr[grepl('ennep', cr$crop_name),crop_name_scientific:= 'cannabis sativa']
cr[grepl('ahlia', cr$crop_name),crop_name_scientific:= 'dahlia']
cr[grepl('ladiool', cr$crop_name),crop_name_scientific:= 'gladiolus communis']
cr[grepl('Hyacint', cr$crop_name),crop_name_scientific:= 'hyacinthus orientalis']
cr[grepl('Kuifhyacint', cr$crop_name),crop_name_scientific:= 'leopoldia comosa']
cr[grepl('Iris|iris', cr$crop_name),crop_name_scientific:= 'iris germanica']
cr[grepl('rokus', cr$crop_name),crop_name_scientific:= 'crocus']
cr[grepl('Lelie', cr$crop_name),crop_name_scientific:= 'lilium']
cr[grepl('arcis', cr$crop_name),crop_name_scientific:= 'narcissus']
cr[grepl('Tulp|tulp', cr$crop_name),crop_name_scientific:= 'tulipa']
cr[grepl('antedeschia', cr$crop_name),crop_name_scientific:= 'zantedeschia aethiopica']
cr[grepl('maryllis', cr$crop_name),crop_name_scientific:= 'amaryllis belladonna']
cr[grepl('lauw druifje', cr$crop_name),crop_name_scientific:= 'muscari botryoides']
cr[grepl('aleriaan', cr$crop_name),crop_name_scientific:= 'valeriana officinalis']
cr[grepl('Knoflook', cr$crop_name),crop_name_scientific:= 'allium sativum']
cr[grepl('Quinoa', cr$crop_name),crop_name_scientific:= 'chenopodium quinoa']
cr[grepl('Pastinaak', cr$crop_name),crop_name_scientific:= 'pastinaca sativa']
cr[grepl('Pioenroos', cr$crop_name),crop_name_scientific:= 'paeonia']
cr[grepl('Lavas', cr$crop_name),crop_name_scientific:= 'levisticum officinale']
cr[grepl('Oregano', cr$crop_name),crop_name_scientific:= 'origanum vulgare']
cr[grepl('Goudsbloem', cr$crop_name),crop_name_scientific:= 'calendula officinalis']
cr[grepl('Igniscum Candy', cr$crop_name),crop_name_scientific:= 'polygonaceae']
cr[grepl('aaldaar', cr$crop_name),crop_name_scientific:= 'setaria']
cr[grepl('eterselie', cr$crop_name),crop_name_scientific:= 'petroselinum crispum']
cr[grepl('rysant', cr$crop_name),crop_name_scientific:= 'chrysanthemum']
cr[grepl('elica', cr$crop_name),crop_name_scientific:= 'angelica']
cr[grepl('apaver', cr$crop_name),crop_name_scientific:= 'papaver']
cr[grepl('donis', cr$crop_name),crop_name_scientific:= 'adonis']
cr[grepl('nietje', cr$crop_name),crop_name_scientific:= 'myosotis']
cr[grepl('ranberry', cr$crop_name),crop_name_scientific:= 'vaccinium macrocarpon']
cr[grepl('zonnehoed', cr$crop_name),crop_name_scientific:= 'echinacea']
cr[grepl('eeuwenbek', cr$crop_name),crop_name_scientific:= 'antirrhinum']
cr[grepl('uxus', cr$crop_name),crop_name_scientific:= 'buxus']
cr[grepl('ricaceae', cr$crop_name),crop_name_scientific:= 'ericaceae']
cr[grepl('Rozen', cr$crop_name),crop_name_scientific:= 'rosa']
cr[grepl('coniferen', cr$crop_name),crop_name_scientific:= 'coniferales']
cr[grepl('ppelen\\.', cr$crop_name),crop_name_scientific:= 'malus']
cr[grepl('eren\\.', cr$crop_name),crop_name_scientific:= 'pyrus']
cr[grepl('ijndruiven', cr$crop_name),crop_name_scientific:= 'vitis vinifera']
cr[grepl('cultuurgrond|aunaranden|fruit|nijgroen|opgegeven|dummy|SBL|beteelde|bemester|eide|element|ruiden|enrij', cr$crop_name),crop_name_scientific:= 'overig']
cr[grepl('azelno', cr$crop_name),crop_name_scientific:= 'corylus avellana']
cr[grepl('alnoten', cr$crop_name),crop_name_scientific:= 'juglans regia']
cr[grepl('essen, blauwe', cr$crop_name),crop_name_scientific:= 'vaccinium corymbosum']
cr[grepl('ruimen|ersen', cr$crop_name),crop_name_scientific:= 'prunus']
cr[grepl('essen, zwarte', cr$crop_name),crop_name_scientific:= 'ribes nigrum']
cr[grepl('oolzaad', cr$crop_name),crop_name_scientific:= 'brassica napus']
cr[grepl('agetes', cr$crop_name),crop_name_scientific:= 'tagetes']
cr[grepl('ardperen$', cr$crop_name),crop_name_scientific:= 'helianthus tuberosus']
cr[grepl('vlinderbloemige', cr$crop_name),crop_name_scientific:= '']
cr[grepl('essen, rode', cr$crop_name),crop_name_scientific:= 'ribes rubrum']
cr[grepl('rambozen', cr$crop_name),crop_name_scientific:= 'rubus idaeus']
cr[grepl('ramen', cr$crop_name),crop_name_scientific:= 'rubus']
cr[grepl('ardbeien', cr$crop_name),crop_name_scientific:= 'fragaria x ananassa']
cr[grepl('sperges', cr$crop_name),crop_name_scientific:= 'asparagus officinalis']
cr[grepl('kool|rocolli|roccoli|rabi', cr$crop_name),crop_name_scientific:= 'brassica oleracea']
cr[grepl('raap|inese kool|aksoi|aapstelen|oppelknollen', cr$crop_name),crop_name_scientific:= 'brassica rapa']
cr[grepl('ourgette', cr$crop_name),crop_name_scientific:= 'cucurbita pepo']
cr[grepl('elderij', cr$crop_name),crop_name_scientific:= 'apium graveolens']
cr[grepl('enkel', cr$crop_name),crop_name_scientific:= 'foeniculum vulgare']
cr[grepl('omkommer|gurk', cr$crop_name),crop_name_scientific:= 'cucumis sativus']
cr[grepl('eloen|ompoen', cr$crop_name),crop_name_scientific:= 'cucurbita'] # soortnieveau van meloenen en pompoen wordt niet bijgehouden in brp
cr[grepl('abarber', cr$crop_name),crop_name_scientific:= 'rheum rhabarbarum']
cr[grepl('Prei', cr$crop_name),crop_name_scientific:= 'allium ampeloprasum']
cr[grepl('adijs', cr$crop_name),crop_name_scientific:= 'raphanus sativus']
cr[grepl('chorseneren', cr$crop_name),crop_name_scientific:= 'scorzonera hispanica']
cr[grepl('Sla', cr$crop_name),crop_name_scientific:= 'lactuca sativa']
cr[grepl('inazie', cr$crop_name),crop_name_scientific:= 'espinaca oleracea']
cr[grepl('lexandrijnse', cr$crop_name),crop_name_scientific:= 'trifolium alexandrinum']
cr[grepl('eemdlangbloem', cr$crop_name),crop_name_scientific:= 'festuca pratensis']
cr[grepl('rammenas', cr$crop_name),crop_name_scientific:= 'raphanus sativus']
cr[grepl('Deder', cr$crop_name),crop_name_scientific:= 'camelina sativa']
cr[grepl('acelia', cr$crop_name),crop_name_scientific:= 'phacelia']
cr[grepl('ranse boekweit', cr$crop_name),crop_name_scientific:= 'fagopyrum tataricum']
cr[grepl('incarnaat', cr$crop_name),crop_name_scientific:= 'trifolium incarnatum']
cr[grepl('imothee', cr$crop_name),crop_name_scientific:= 'phleum pratense']
cr[grepl('zolla', cr$crop_name),crop_name_scientific:= 'azolla']
cr[grepl('knotboom|Knotboom|historisch|houtsingel|oogstamboomgaar|bosje|oomgroepen|singel|riendje|scheerheg|Laan|almen, |Rietzoom|^Riet$|zandwallen|Struweel|oedselbos|boerenland|Water,', cr$crop_name),crop_name_scientific:= 'overig']
cr[grepl('erzische', cr$crop_name),crop_name_scientific:= 'trifolium resupinatum']
cr[grepl('isdodde', cr$crop_name),crop_name_scientific:= 'typhaceae']
cr[grepl('Niger$', cr$crop_name),crop_name_scientific:= 'guizotia abyssinica']
cr[grepl('eradelle', cr$crop_name),crop_name_scientific:= 'ornithopus sativus']
cr[grepl('Spurrie', cr$crop_name),crop_name_scientific:= 'spergula']
cr[grepl('rouwenmantel', cr$crop_name),crop_name_scientific:= 'alchemila']
cr[grepl('ilde rijst$', cr$crop_name),crop_name_scientific:= 'zizania']
cr[grepl('onnekroon', cr$crop_name),crop_name_scientific:= 'silphium perfoliatum']

# head(cr[is.na(crop_name_scientific),crop_name],1)
# 
# unique(cr[crop_name_scientific == 'carum carvi',.(crop_name, crop_name_scientific)])
# cr[grepl('lanten', cr$crop_name), .(crop_name, crop_name_scientific)]
# cr[grepl('Engels|engels', cr$crop_name), .(crop_name, crop_name_scientific)]
# unique(cr[is.na(crop_name_scientific),.(crop_name, crop_name_scientific)])
# sum(is.na(cr$crop_name_scientific))

crops.obic <- cr

# Save new crops.obic
fwrite(crops.obic, 'DEV/crops_obic.csv')
save(crops.obic, file = 'data/crops_obic.RData')
