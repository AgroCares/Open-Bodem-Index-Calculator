library(data.table)

test_that("ind_nematodes works", {
  expect_equal(
    ind_nematodes(
      data.table(species = c("Ditylenchus spp."             ,"Ditylenchus dipsaci"          ,"Ditylenchus destructor"       ,"Xiphinema spp."           ,   
                                "Longidorus spp."              ,"(Para)Trichodoridae spp."     ,"Trichodorus similis"          ,"Trichodorus primitivus"   ,   
                                "Trichodorus viruliferus"      ,"Trichodorus sparsus"          ,"Trichodorus cylindricus"      ,"Trichodorus hooperi"      ,   
                                "Paratrichodorus teres"        ,"Paratrichodorus pachydermus"  ,"Paratrichodorus anemones"     ,"Paratrichodorus nanus"    ,   
                                "Aphelenchoides spp."          ,"Aphelenchoides fragariae"     ,"Aphelenchoides ritzemabosi"   ,"Aphelenchoides subtenuis" ,   
                                "Criconematidae spp."          ,"Subanguina spp."              ,"Rotylenchus spp."             ,"Paratylenchus spp."       ,   
                                "Paratylenchus bukowinensis"   ,"Meloidogyne spp."             ,"Meloidogyne chitwoodi/fallax" ,"Meloidogyne chitwoodi"    ,   
                                "Meloidogyne fallax"           ,"Meloidogyne minor"            ,"Meloidogyne incognita"        ,"Meloidogyne javanica"     ,   
                                "Meloidogyne artiellia"        ,"Meloidogyne arenaria"         ,"Meloidogyne ardenensis"       ,"Meloidogyne naasi"        ,   
                                "Meloidogyne hapla"            ,"Cysteaaltjes"                 ,"Hemicycliophora spp."         ,"Pratylenchus spp."        ,   
                                "Pratylenchus penetrans"       ,"Pratylenchus crenatus"        ,"Tylenchorhynchus spp."        ,"Helicotylenchus spp."     ,   
                                "Pratylenchus neglectus"       ,"Pratylenchus pratensis"       ,"Pratylenchus thornei"         ,"Pratylenchus flakkensis"  ,   
                                "Pratylenchus fallax"          ,"Pratylenchus pinguicaudatus"  ,"Pratylenchus pseudopratensis" ,"Pratylenchus vulnus"      ,   
                                "Pratylenchus dunensis"        ,"Pratylenchus zeae"),
                    count = rep(0,54))
      ),
    expected = 1,
    tolecance = 0.01
  )
  expect_less_than(
    ind_nematodes(
      data.table(species = c("Ditylenchus spp."             ,"Ditylenchus dipsaci"          ,"Ditylenchus destructor"       ,"Xiphinema spp."           ,   
                             "Longidorus spp."              ,"(Para)Trichodoridae spp."     ,"Trichodorus similis"          ,"Trichodorus primitivus"   ,   
                             "Trichodorus viruliferus"      ,"Trichodorus sparsus"          ,"Trichodorus cylindricus"      ,"Trichodorus hooperi"      ,   
                             "Paratrichodorus teres"        ,"Paratrichodorus pachydermus"  ,"Paratrichodorus anemones"     ,"Paratrichodorus nanus"    ,   
                             "Aphelenchoides spp."          ,"Aphelenchoides fragariae"     ,"Aphelenchoides ritzemabosi"   ,"Aphelenchoides subtenuis" ,   
                             "Criconematidae spp."          ,"Subanguina spp."              ,"Rotylenchus spp."             ,"Paratylenchus spp."       ,   
                             "Paratylenchus bukowinensis"   ,"Meloidogyne spp."             ,"Meloidogyne chitwoodi/fallax" ,"Meloidogyne chitwoodi"    ,   
                             "Meloidogyne fallax"           ,"Meloidogyne minor"            ,"Meloidogyne incognita"        ,"Meloidogyne javanica"     ,   
                             "Meloidogyne artiellia"        ,"Meloidogyne arenaria"         ,"Meloidogyne ardenensis"       ,"Meloidogyne naasi"        ,   
                             "Meloidogyne hapla"            ,"Cysteaaltjes"                 ,"Hemicycliophora spp."         ,"Pratylenchus spp."        ,   
                             "Pratylenchus penetrans"       ,"Pratylenchus crenatus"        ,"Tylenchorhynchus spp."        ,"Helicotylenchus spp."     ,   
                             "Pratylenchus neglectus"       ,"Pratylenchus pratensis"       ,"Pratylenchus thornei"         ,"Pratylenchus flakkensis"  ,   
                             "Pratylenchus fallax"          ,"Pratylenchus pinguicaudatus"  ,"Pratylenchus pseudopratensis" ,"Pratylenchus vulnus"      ,   
                             "Pratylenchus dunensis"        ,"Pratylenchus zeae"),
                 count = rep(10000,54))
    ),
    expected = 0.0011
  )
})
