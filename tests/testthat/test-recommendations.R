# Check wrapper function
test_that("obic_recommendations_bkp works", {
  # Load data
  dt <- OBIC::binnenveld[ID <=5]
  B_LU_BRP <- c(233,266,2014,256,308)
  B_SOILTYPE_AGR <- c("dekzand","rivierklei","dekzand","rivierklei","dekzand")
  dt[, B_FERT_NORM_FR := 1]
  
  dt.obi <- obic_farm(dt)
  dt.score <- dt.obi$fields
  
  dt.score <- dt.score[,mget(colnames(dt.score)[!grepl('^RM',colnames(dt.score))])]
  
  
  # Example 1
  out1 <- obic_recommendations_bkp(dt.score = dt.score, B_LU_BRP, B_SOILTYPE_AGR)
  
  # test sample 1
  expect_equal(
    object = out1[c(1,4,12,18,20,24),recommendation],
    expect = c("Herstel de verdichte ondergrond",
               "Bekalk de bodem tot de geadviseerde pH is bereikt",
               "Verbouw (meer) groenbemesters",
               "Pas niet-kerende grondbewerking toe",
               "Voer (extra) compost aan",
               "Voer (extra) compost aan")
  )
  
  # Example 2
  dt.score[,c(colnames(dt.score)) := 0]
  out2 <- obic_recommendations_bkp(dt.score = dt.score[1,], B_LU_BRP = 263, B_SOILTYPE_AGR = B_SOILTYPE_AGR[1])
  
  # test sample 2
  expect_equal(
    object = out2$recommendation,
    expect = c("Bemest confrom het bemestingsadvies",
               "Pas niet-kerende grondbewerking toe",            
               "Bekalk de bodem tot de geadviseerde pH is bereikt",
               "Voer (extra) compost aan",           
               "Verbouw (meer) groenbemesters")
  )
  
  # Example 3
  dt.score[,c(colnames(dt.score)) := 1]
  out3 <- obic_recommendations_bkp(dt.score = dt.score[1,], B_LU_BRP = 263, B_SOILTYPE_AGR = B_SOILTYPE_AGR[1])
  
  # test sample 3
  expect_equal(
    object = out3$recommendation,
    expect = c("Herstel de verdichte ondergrond",
               "Voer (extra) compost aan",
               "Bemest confrom het bemestingsadvies",
               "Verbouw (meer) diepwortelende gewassen",
               "Verbouw (meer) groenbemesters")
  )
  
  
})
