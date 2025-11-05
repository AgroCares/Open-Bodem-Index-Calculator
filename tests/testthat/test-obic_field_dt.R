# make input data table
tdt <- data.table(
  B_SOILTYPE_AGR = 'rivierklei',
  B_GWL_CLASS = "II",
  B_GWL_GLG = 75,
  B_GWL_GHG = 10,
  B_GWL_ZCRIT = 50,
  B_SC_WENR = '2',
  B_HELP_WENR = "MOb72",
  B_AER_CBS = 'LG01',
  B_LU_BRP = c( 1010, 1010,263,263, 263,265,265,265),
  A_SOM_LOI = 3.91,
  A_SAND_MI = 66.3,
  A_SILT_MI = 22.8,
  A_CLAY_MI = 7.8,
  A_PH_CC = 5.4,
  A_N_RT = 1528.33,
  A_CN_FR = 13.02,
  A_S_RT = 321.26,
  A_N_PMN = 63.3,
  A_P_AL = 50.2,
  A_P_CC = 2.9,
  A_P_WA = 50.5,
  A_CEC_CO = 56.9,
  A_CA_CO_PO = 66.87,
  A_MG_CO_PO = 13.97,
  A_K_CO_PO = 3.06,
  A_K_CC = 58.6,
  A_MG_CC = 77.53,
  A_MN_CC = 7586.61,
  A_ZN_CC = 726.2,
  A_CU_CC = 68.8,
  A_C_BCS = 1,
  A_CC_BCS = 1,
  A_GS_BCS = 1,
  A_P_BCS = 1,
  A_RD_BCS = 1,
  A_EW_BCS = 1,
  A_SS_BCS = 1,
  A_RT_BCS = 1,
  A_SC_BCS = 1,
  M_COMPOST = 0,
  M_GREEN = FALSE,
  M_NONBARE =FALSE,
  M_EARLYCROP = FALSE,
  M_SLEEPHOSE = FALSE,
  M_DRAIN = FALSE,
  M_DITCH = FALSE,
  M_UNDERSEED = FALSE,
  M_LIME = FALSE,
  M_MECHWEEDS = FALSE,
  M_NONINVTILL = FALSE,
  M_PESTICIDES_DST = FALSE,
  M_SOLIDMANURE = FALSE,
  M_SSPM = FALSE,
  M_STRAWRESIDUE = FALSE,
  ID = 1,
  key = 'ID'
)

test_that("obic_field_dt works with classic OBI", {
  # test classic obi
  expect_equal(
    obic_field_dt(tdt, useClassicOBI = TRUE),
    expected = data.table(
      ID = 1,
      I_BCS = 0.369,
      I_B_DI = 0.843,
      I_B_SF = 0.965,
      I_C_CEC = 0.569,
      I_C_CU = 1,
      I_C_K = 0.988,
      I_C_MG = 0.991,
      I_C_N = 0.836,
      I_C_P = 0.961,
      I_C_PH = 0.15,
      I_C_S = 0.26,
      I_C_ZN = 0.561,
      I_E_NGW = 0.988,
      I_E_NSW = 0.969,
      I_M = 0.069,
      I_M_BIODIVERSITY = 0.144,
      I_M_CLIMATE = 0.113,
      I_M_SOILFERTILITY = 0.144,
      I_M_WATERQUALITY = 0.126,
      I_P_CEC = 0.417,
      I_P_CO = 0.333,
      I_P_CR = 1,
      I_P_DS = 0.99,
      I_P_DU = 0.652,
      I_P_SE = 0.93,
      I_P_WO = 0.7,
      I_P_WRI = 0.835,
      I_P_WS = 0.11,
      S_B_OBI_A = 0.9,
      S_C_OBI_A = 0.537,
      S_E_OBI_A = 0.979,
      S_M_OBI_A = 0.069,
      S_P_OBI_A = 0.474,
      S_T_OBI_A = 0.592,
      RM_C_1 = "M8",
      RM_C_2 = "M1",
      RM_C_3 = "M2",
      RM_P_1 = "M6",
      RM_P_2 = "M3",
      RM_P_3 = "M1",
      RM_B_1 = "M0",
      RM_B_2 = "M0",
      RM_B_3 = "M0",
      key = "ID"
    ),
    tolerance = 0.01
  )
})

test_that('obic_field_dt works with extended OBI', {
  # test extended obi
  expect_error(obic_field(tdt, useClassicOBI = FALSE)) # B_DRAIN is missing
  
  tdt[, B_DRAIN := FALSE]
  expect_equal(
    obic_field_dt(tdt, useClassicOBI = FALSE),
    expected = data.table(
      ID = 1,
      D_OPI_GW = 0.88,
      I_BCS = 0.369,
      I_B_DI = 0.843,
      I_B_SF = 0.965,
      I_C_CEC = 0.569,
      I_C_CU = 1,
      I_C_K = 0.988,
      I_C_MG = 0.991,
      I_C_N = 0.836,
      I_C_P = 0.961,
      I_C_PH = 0.15,
      I_C_S = 0.26,
      I_C_ZN = 0.561,
      I_E_NGW = 0.988,
      I_E_NSW = 0.969,
      I_H_GWR = 0.626,
      I_H_NGW = 0.99,
      I_H_NSW = 0.992,
      I_H_PEST = 0.023,
      I_M = 0.069,
      I_M_BIODIVERSITY = 0.144,
      I_M_CLIMATE = 0.113,
      I_M_SOILFERTILITY = 0.144,
      I_M_WATERQUALITY = 0.126,
      I_P_CEC = 0.417,
      I_P_CO = 0.333,
      I_P_CR = 1,
      I_P_DS = 0.99,
      I_P_DU = 0.652,
      I_P_SE = 0.93,
      I_P_WO = 0.7,
      I_P_WRI = 0.835,
      I_P_WS = 0.11,
      S_B_OBI_A = 0.9,
      S_C_OBI_A = 0.537,
      S_E_OBI_A = 0.649,
      S_M_OBI_A = 0.069,
      S_P_OBI_A = 0.474,
      S_T_OBI_A = 0.559,
      RM_C_1 = "M8",
      RM_C_2 = "M1",
      RM_C_3 = "M2",
      RM_P_1 = "M6",
      RM_P_2 = "M3",
      RM_P_3 = "M1",
      RM_B_1 = "M0",
      RM_B_2 = "M0",
      RM_B_3 = "M0",
      key = "ID"
    ),
    tolerance = 0.01
  )
  
})

test_that('obic_field_dt() works with just required columns', {
  # get required column names, should be equal to dt.req in obic_field_dt()
  req.cols <- c('B_SOILTYPE_AGR', 'B_GWL_CLASS', 'B_SC_WENR', 'B_HELP_WENR', 'B_AER_CBS', 
                'B_GWL_GLG', 'B_GWL_GHG', 'B_GWL_ZCRIT', 'B_LU_BRP', 'B_LU_BRP', 
                'A_SOM_LOI', 'A_SAND_MI', 'A_SILT_MI', 'A_CLAY_MI', 'A_PH_CC',
                'A_N_RT', 'A_CN_FR', 'A_S_RT', 'A_N_PMN', 'A_P_AL', 'A_P_CC', 'A_P_WA',
                'A_CEC_CO', 'A_CA_CO_PO', 'A_MG_CO_PO', 'A_K_CO_PO',
                'A_K_CC', 'A_MG_CC', 'A_MN_CC', 'A_ZN_CC', 'A_CU_CC', 'ID')
  
  # take subset of tdt
  stdt <- tdt[,..req.cols]
  
  expect_equal(
    obic_field_dt(stdt, output = "scores", useClassicOBI = TRUE),
    expected = 
      data.table(
        ID = 1,
        S_B_OBI_A = 0.900,
        S_C_OBI_A = 0.537,
        S_E_OBI_A = 0.979,
        S_M_OBI_A = 0.287,
        S_P_OBI_A = 0.571,
        S_T_OBI_A = 0.642,
        key = 'ID'
      ),
    tolerance = 0.01
  )
  
  stdt[, B_DRAIN := TRUE]
  expect_equal(
    object = obic_field_dt(stdt, output = "scores", useClassicOBI = FALSE),
    expected = 
      data.table(
        ID = 1,
        S_B_OBI_A = 0.900,
        S_C_OBI_A = 0.537,
        S_E_OBI_A = 0.658,
        S_M_OBI_A = 0.287,
        S_P_OBI_A = 0.571,
        S_T_OBI_A = 0.602,
        key = 'ID'
      ),
    tolerance = 0.01
  )
  
})

test_that('B_FERT_NORM_FR can be changed in obic_field_dt',{
  fnorm1 <- copy(tdt)
  fnorm1[,B_FERT_NORM_FR := 1]
  fnorm05 <- copy(tdt)
  fnorm05[,B_FERT_NORM_FR := 0.5]
  
  expect_false(obic_field_dt(fnorm1) == obic_field_dt(fnorm05))
})
