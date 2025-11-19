# OBIC 5.0.0 2025-11-13
## Added
* Function ind_gw_target() to modify I_H_GWR with a correction factor based on
soiltype and groundwaterclass akin to the groundwaterrecharge score from BBWPC.
* D_OPI_GW to weight.obic

## Changed
* In table weight.obic, peat and non-peat weight for indicator I_H_GWR are now
set to -1 for all landuses. This indicator is replaced in calculating scores by
D_OPI_GW to get the same scores as would be obtained by BBWPC. D_OPI_GW applies
to all soil types.
* Argument useClassicOBI is added to obi_field(), obi_field_dt(), and obi_farm().
This argument defaults to TRUE, when TRUE, only agronomic indicators are calculated
and aggregated. When FALSE, indicators for water quality are calculated and aggregated
in the environmental score and total score.
* When useClassicOBI = FALSE, water indicators (I_H_x) are included in the 
calculation of S_E_OBI_A. I_H_GWR isn't included in this aggregation, this indicator
is replaced by D_OPI_GW.
* B_DRAIN no longer has a default value and must be supplied when useClassicOBI = FALSE
* B_FERT_NORM_FR now must be in the data.table supplied to `obic_field_dt()` or 
`obic_farm()`, failing to do so will return an error. This is a breaking change for
users of these functions.
* M_GREEN is longer set to TRUE for potato and maize by `add_management()`. This
means that M_GREEN is overwritten for such cultivations when using one of the wrapper
functions.
* renamed I_H_NGW to I_H_NGW 
* renamed I_H_NSW to I_E_SW_NLEA 
* renamed I_H_PEST to I_E_PEST 
* renamed I_H_GWR to I_E_GWR 
* renamed I_E_NGW to I_E_GW_NRET
* renamed I_E_NSW to I_E_SW_NRET 

## Fixed
* B_FERT_NORM_FR and B_DRAIN can now be supplied when using `obic_field_dt()` or
`obic_farm()`.

# OBIC 4.1.0 2025-08-04
## Added
* BRP crop codes for 2025: 7137, 7138, 7135, 7134

# OBIC 4.0.0 2025-07-30
## Changed
* the format of groundwater class values (B_GWL_CLASS) that are accepted by OBIC 
functions and recorded in OBIC tables. Acceptable input values for B_GWL_CLASS are now:
"I", "Ia", "Ic", "II", "IIa", "IIb", "IIc", "III", "IIIa", "IIIb", "IV",
"IVc", "IVu", "sV", "sVb", "V", "Va", "Vad", "Vao", "Vb", "Vbd", "Vbo", "VI", 
"VId", "VII", "VIId", "VIII", "VIIId", "VIIIo", "VIIo", "VIo".
* OBIC no longer supports B_GWL_CLASS value "-". For fields with groundwater class "-", the user is advised to use expert judgment on
what the most suitable groundwater class is. Fields with "-" are typically found 
in locations with very variable or very deep groundwater levels such as flood plains
or hills.
* In the table nleach_table, column B_GT is renamed B_GWL_CLASS for consistency. 
The values in this column have also been modified to no longer have the prefix "Gt"
to align with the rest of the package.
* In the table waterstress.obic, column gt is renamed B_GWL_CLASS for consistency.
The values in this column have also been modified to no longer have the prefix "Gt"
to align with the rest of the package.

## Added
* The table waterstress.obic now supports more groundwaterclasses. Classes such as
IIIb are now included in waterstress.obic$B_GWL_CLASS. The new classes with 
prefixes and suffixes have the same values as rows with the same roman numeral. 
So, with the same cropname and soilunit, III and IIIb have the same values for
the different stresses.
* additional B_GWL_CLASS values to `nleach_table` ("Ia", "Ic", "IIa", "IIc", "IIIa",
"IVu", "IVc", "Va", "Vao", "Vad", "Vbo", "Vbd", "sV", "sVb", "VIo", "VId", "VIIo", "VIId", "VIIIo", "VIIId")

## Removed
* function `format_gwt()`. OBIC now supports groundwater classes with prefixes and suffixes.
For fields with groundwater class "-", the user is advised to use expert judgment on
what the most suitable groundwater class is. Fields with "-" are typically found 
in locations with very variable or very deep groundwater levels such as flood plains
or hills.

# OBIC 3.0.4 2025-05-20
## Fixed
* Fixes missing nf_* values in `crops.obic` [OBI-31]

# OBIC 3.0.3 2024-08-12
## Added
* Adds the new cultivations from 2024 [PL-33]

# OBIC 3.0.2 2024-02-09
## Fixed
* Fixes error in validation of B_LU_BRP by enabling calculations with brp codes of 2023 [OBI-28]
* Fixes classification of grasland met/zonder herinzaai in `crops.obic`

## Changed
* `B_LU_WATERSTRESS_OBIC` of 265 (grasland blijvend), 331, and 3718 from 'grasland met herinzaai' to 'grasland zonder herinzaai' in `crops.obic`

## Removed
* Removes urls to bodemconsult.nl due to invalid SSL certificate, which is not accepted by CRAN

# OBIC 3.0.1 2023-11-24
## Fixed
* Fixes calculation of N surplus used for I_H_NGW and I_H_NSW

# OBIC 3.0.0 2023-10-16
## Added
* Add `calc_permeability` for calculating top soil permeability 
* Add `ind_permeability` for calculating the index for top soil permeability
* Add variable `leach_to` to `calc_n_efficiency` and `ind_n_efficiency` to calculate and evaluate leaching to ground water and surface water
* Add vignette `obic_water_functions`, describing the functions that evaluates the effect of soil quality on groundwater quality

## Changed
* Updated N use norms for onions
* Update calc_nleach by soil and land use dependent default n supply, issue #154
* Update calculations in the functions `calc_pesticide_leaching`, `ind_pesticide_leaching`, `calc_psp`, `ind_psp`, `ind_gw_recharge`,`calc_n_efficiency` and `ind_n_efficiency`
* Add soil type and land use dependent NLV estimate in `calc_nleach`
* In the input for `ind_gw_recharge` the variables `B_LU_BRP`, `D_WRI_K`, and `B_GWL_CLASS` are added, and `D_WRI_WHC` is removed
* In `calc_n_efficiency`, `D_NLV` is removed from the input variables

## Fixed
* Fixes plotting of regime curve in workability vignette #149
* Extremely high EOS values for some cultivations fixing #162 & #134
* Updated bibliography of packages in vignettes
* N use norm for 1935 (maiskolfsilage)
* N use norms of 1926 and 1927 (agrarisch natuurmengsel and overige akkerbouwgewassen), increased to match "Akkerbouwgewassen, overig" (RVO, 2022)

# OBIC 2.2.1 2023-10-16
## Added
* CITATION file and additional author data in the package DESCRIPTION

# OBIC 2.2.0 2023-08-29
## Added
* Adds function `obic_recommendations_bkp` for determining management recommendations to improve soil quality

# OBIC 2.1.3 2023-08-29
## Fixed
* Fixes error in formatting of B_AER_CBS for LG01 and LG02 in format_aer [OBI-20]

# OBIC 2.1.2 2023-03-28
## Fixed
* Added missing values in `nf_` columns in `crops.obic`

# OBIC 2.1.1 2023-03-21
## Fixed
* Update `crops.obic` for missing `b_lu_brp` codes

# OBIC 2.1.0 2023-03-03
## Added
* add `obic_farm` to assess soil quality on farm level (plus unit tests), #OBI-8
* total farm score `farm_obi_score` as output reflecting the percentage of fields in the highest class for the OBI score, #OBI-8

## Changed
* output handling in obic_field is updated to have more flexibility and avoid undesired calculations

# OBIC 2.0.5 2023-02-17
## Fixed
* Fixes to generate clearer error message when incorrect B_LU_BRP is supplied.

# OBIC 2.0.4 2023-01-11
## Fixed
* Fixes calculation of `field capacity` at `calc_waterretention`

# OBIC 2.0.3 2023-01-11
## Fixed
* Fixes plotting of regime curve in workabilty vignette #149

# OBIC 2.0.2 2023-01-11
## Changed
* Update the GitHub Actions to the most recent version
* Update `RoxygenNote` to v7.2.3

# OBIC 2.0.1 2022-03-26
## Changed
* The function `ind_sulpher` is renamed to `ind_sulfur` in documentation

## Added
* all functions are extended with `examples` and `return`

## Changed
* standardize output variables of pedotransfer parameters Wosten

## Fixed
* Fixes many spelling errors in the documentation
* Set title in `decription` to title case
* Fix links to external websites
* Fix duplicated indicator in recom.obic (in dev/ppr_maatregel.r), issue #164
* Fix setorder after merge in calc_n_effieciency
* Fix setorder after merge in calc_som_balance

## Removed
* Removes `/docs` as the documentation website is now automatically generated by GitHub Actions

## Deprecated
* The function `ind_sulpher` is deprecated due to renaming

# OBIC 2.0.0 2022-01-11
## Added
* Add `ppr_bouwplan_tables.R` in dev/scripts
* Add `merge_nema_tables.R` in dev/scripts
* Add `ppr_crops_crumbleability.R` in dev/scripts
* Add arguments `B_GWL_GHG`, `B_GWL_GLG` and `B_Z_TWO` to `obic_field` and `obic_field_dt`
* Activate `calc_workability` 
* Add `calc_makkink` to add makkink factors to crop table
* Add `calc_psp` to estimate precipitation surplus
* Add `ind_nematodes` and `ind_nematodes_list` to estimate the index for nematode risks
* Add `ind_gw_recharge` to estimate the groundwater recharge index
* Add `calc_n_efficiency` and `ind_n_efficiency` to estimate N efficiency
* Add `calc_pesticide_leaching` and `ind_pesticide_leaching` to evaluate retention of pesticides in topsoil
* Add check to `ppr_weight_obic` to check if all indicators in `weight.obic` occur in `column_descriptions_obic`
* Add vignette `obic_workability`, describing how workability is calculated and affected by its input variables
* Add vignette `obic_introduction` as a short tutorial describing the OBIC principles
* Add vignette `obic_score_aggregation` to illustrate the aggregation principles used
* Add table `weather.obic` with monthly mean precipitation, temperature and evaporation
* Add table `crops.makkink` with mean Makkink factors per crop type
* Add table `nema.crop.rot.obic` added with crop sensitivity for nematodes
* Add table `nema.obic` with threshold values per species
* Add GitHub Action to run R-CMD-Check for changes on `master` and `development`
* Add coverage of unit tests

## Changed
* Update documentation for workability
* Update `weight.obic` for workability
* Update `column_descriptions_obic.Rdata`
* Update documentation for nematodes
* Update `calc_workability`
* Rename `B_Z_TWO` to `B_GWL_ZCRIT`
* Improve function description of `ind_workability`
* Increase required days post GLG for maize on sand or loess to match October 1
* Increase required days post GLG for maize on other soils to match October 20
* `season.obic` can now be merged on both landuse and soiltype
* Update binnenveld with BodemSchat data (via OBIC-helpers)
* `obic_field` can be used for multiple fields with field-ID

## Removed
* Removes `A_COM_FR` and `A_CACO3_IF` from input as they were not used #145

## Fixed
* Error in `grass_age`: age was incorrectly estimated for multiple fields
* Removed negative values from `season.obic`
* Ensured total_days >= req_days_pre_glg + req_days_post_glg in `season.obic`
* Root depth was incorrectly assigned for maize and nature 
* Fix the format of `NEWS.md` so that it is shown on the package website
* Remove the prefixes and suffixed of `B_GWL_CLASS`

# OBIC 1.0.4 2021-09-27
## Added
* Check on column names in obic_field_dt
* tests on obic_field_dt

## Fixed
* Fixed check on required column names in obic_field_dt

# OBIC 1.0.3 2021-09-16
## Added
* Adds unit test for `obic_field`

## Fixed
* Fixes ascii signs issues

# OBIC 1.0.2 2021-05-20
## Added
* ind_man_ess.R and tests are added for update aggregated management scores, issue #131

## Changed
* obic_field() makes use of ind_man_ess

## Fixed
* check B_GWL_CLASS in calc_waterstressindex(), issue #129

# OBIC 1.0.1 2021-05-05
## Added
* management.obic table added to link management measures to ecosystem services
* calc_man_ess.R and tests are added to estimate grouped impact of management on ecosystem servcices, issue #125

## Changed
* I_M_* and I_*_BCS indicators for management measures and BCS are removed, issue #126
* values -999 are replaced by NA 
* column_descriptions_obic.Rdata is updated
* weight.obic is updated


# OBIC 1.0.0 2021-04-28
## Added
* column crops_season to crops.obic
* column scientific_names to crops.obic
* preparation crops.obic in dev
* function format_gwt to reformat input for groundwater table 
* function format_soilcompaction to reformat input for subsoil compaction
* season.obic added as data.table
* new function and tests for workability indicator
* nema.obic added as data.table
* new function and tests for nematode indicator
* preparation nema.obic in dev
* preparation season.obic in dev
* obic_field is added to run obic score for one field
* ppr_column_description plus column_descriptions_obic.Rdata
* ppr_maatregel plus recom_obic.Rdata
* ppr_weights plus weights.obic
* I_P_DS and I_P_WS added (drought and wetness stress separately)
* add_management added to estimate default values for measures when not given 
* set of eight I_M_XX indicators for measures are added
* set of nine I_X_BCS indicators are added (optional)
* new M_codes added: M_LIME, M_NONINVTILL, M_SSPM, M_SOLIDMANURE,M_STRAWRESIDUE, M_MECHWEEDS, M_PESTICIDES_DST, issue #107
* obic_field_dt to run obic_field() for a data.table input 

## Changed
* calculation PBI updated for grassland
* PBI input is flexible for both grassland and arable
* calculation SLV updated
* update element_names for A_P_CC, A_P_AL in phosphate_availability.R
* update element_names for A_S_RT, A_SOM_LOI, B_AER_CBS, B_SOILTYPE_AGR in sulfur.R
* update element_names for B_GWL_CLASS in wateravailability.R
* update element_names for B_GWL_CLASS and B_AER_CBS in nretention.R
* correct weighing for S_C_A
* all element names are updated to English ones
* weighing for score calculated is changed into a more generic approach
* structure for running obi is changed into one function
* weights.obic is extended and used to distinguish for relevant/non-relevant indicators
* grass_age function is updated: count actual years
* BCS is optional input, and when given it overwrite risk indicators compaction / aggregate stability
* M codes for no advice, issue #62
* directory dev is updated and cleaned, issue #104
* lower and upper limits for soil properties updated
* allow unknown B_HELP_WENR in checkmate water_availability, issue 65
* very low Mg indicator values for maize are changed, plus tests updated
* very low Mg indicator values for grassland on clay and peat, issue 84
* evaluation and calculation for Cu are updated

## Fixed
* setorder in winderodibility


# OBIC 0.11.1 2020-09-02
## Changed
* The water holding capacity is now between 0 and 1 (as theta should be) and uses new evaluation function #67
* Lower range of `D_NLV` is set to -30
* Upper range of `D_MG` is set to 1000
* Range of `D_P_DU` is set between 0 and 1

## Fixed
* Fixed the use of `B_LG_CBS` at sulfur
* `ID` can now also be character and does not have to be numeric #66


# OBIC 0.11.0 2020-01-16
## Added
* Adds two other pedotransfer functions (PTFs) to calculate water retention parameters #53

## Changed
* Optimal pH values of grassland are now the middle value of the class `good` #55
* pF of field capacity is changed from 2.2 to 2.0 #53
* Bulk density in the default PTF is now aggregated into 1 continuous function #53
* Parameters of evaluation for PAW are adjusted #53
* Parameters for the evaluation of sealing are adjusted #57
* Changes the formula to calculate nitrogen leaching and run-off #58
* Parameters for the evaluation of nretention are adjusted #58
* parameters and funs for CEC are updated and split for soil fertility and aggregate stability #59
* Parameters for the evaluation of Cu availability are adjusted #59
* potassium index is multiplied so that the evaluation is equal for grass and maize #60

## Fixed
* Improved the function for crumbleability to match the fact sheet #54 #56
* Fix error in calc_potassium_availability #59

# OBIC 0.10.0 2019-12-13
## Added
* Adds the evaluation of measures #37
* Adds giving recommendations based on the OBI score #37
* A relative score per reference group is added #31
* Adds the leaching of nitrogen to surface water and groundwater #17
* Adds a table with the description of the columns and a vignette about it #46
* Adds scoring for environmental performance #31

## Changed
* Update of crumbeability groups in `crops.obic`
* Maximum value of `A_MN_CC` is increased from 30000 to 250000
* The code for evaluating  in `ind_crumbeability` is simplified
* M_M10 user input changed in `D_CP_RUST` input, plus comments added #29
* new table with weighing factors included 'weight.obic' #32
* weighing factors updated in obic_indicators.R #32
* Improved the zinc indicator function #39
* The evaluation function for sulfur on maize and grass is updated #40
* The management function is improved, especially for maize #38
* The maximum score of `ind_management` is adjusted #38
* The parameter values for sulfur on arable land is adjusted #41
* The optimal pH for grassland depends now on organic matter content and presence of clover #42
* The score aggregation is now 60% average of last 4 years and 40% of older years #31
* The maximum value for `D_CR` is limited to 10 #47
* The parameters for the evaluation for `I_C_K` is set to the same as grassland #51
* The evaluation for `I_C_N` is now split into grassland and arable land #50
* Set lifecycle status to `maturing`
* The evaluation of `I_C_MG` is adjusted according to fact sheet

## Fixed
* Preprocessing, indicators and scores make use of internal copy of the table now rather than referencing to an object outside the scope of the function
* Fixed the drought stress, wetness stress and water stress values for GtI in `waterstress.obic`
* For grasslands on peat pr clay, the `D_MG` is now scaled #51
* Limit `D_SLV` between -30 and 250 

# OBIC 0.9.0 2019-10-22
## Changed
* The upper limit for `D_BCS` is increased from 40 to 50
* Switch on crumbleability
* For `calc_phosphate_availability` the category `arable` is added for the crop categories
* Changed evaluation of sulfur for arable fields #26

## Fixed
* Fixed typo if mais in `ind_managment`
* Fixed test for winderodibility
* Use the correct correction factor in `calc_sealing_risk` #19
* Fix for calculating `I_P_CEC` #24
* Fix for calculating difficult values in `calc_sombalance` #25
* Fix for `calc_sbal_arable` where combinations of soil type and region that do not exist in table 6.2 of Handboek Bodem & Bemesting gave a NA #26
* Fix for `calc_bcs` where a value of 1 is given instead of 0 for `bcs_om` #28

# OBIC 0.8.0 2019-08-02
## Added
* Adds index and evaluation of the BodemConditieScore
* Adds potassium to main functions
* Adds a random recommendation to the output

## Changed
* The upper limit for `A_MN_CC` is increased from 20000 to 30000
* The upper limit for `A_CA_CEC` is increased from 100 to  400

## Fixed
* The indicator of pH had the wrong direction 
* Removed the minus sign before `ind_zinc`
* The function `calc_potassium_availability` contained a wrong column name for `A_PH_CC`
* Some unknown situations at `calc_potassium_availability` have now the same values as comparable situations. This should be looked at later

# OBIC 0.7.0 2019-08-02
## Added
* Adds index and evaluation of waterretention parameters
* Adds index and evaluation of water related yield stress
* Adds index and evaluation of metals (Zn and Cu)
* Adds index and evaluation of microbial activity (PMN)
* Adds index and evaluation of CEC for soil fertility and structure
* Adds waterstress.obic table with information of HELP-tables

## Changed
* Change crops.obic with crop category related to water stress


# OBIC 0.6.0 2019-08-01
## Added
* Adds index and evaluation of K-availability

# OBIC 0.5.0 2019-07-31
## Added
* Adds crop classification (sensitivity to P, K and S supply)
* Adds soil organic matter balance (simple one)
* Adds risk for soil compaction (following risk map Van den Akker, 2013)
* Adds index and evaluation of Mg-availability
* Adds index and evaluation of S-availability (evaluation function need still to be parameterized)
* Adds index and evaluation of winderodibility
* Adds index and evaluation of soil strategic management (following Label Duurzaam Bodembeheer)
* Adds columns `crop_eos` and `crop_eos_residue` in `crops.obic`

# OBIC 0.4.0 2019-07-26
## Added
* The existing functions for pH, P, N, crumbleability, sealing and resistance are integrated into `obic()`

## Changed
* The column name `brp` is changed to `B_LU_BRP`

# OBIC 0.3.0 2019-07-24
## Added
* Adds main function that wraps all the functions needed to calculated the OBI score and recommendations
* Adds documentation website for package using `pkgdown` #3

# OBIC 0.2.0 2019-07-12
## Added
* Adds evaluation of pH #1
* Adds evaluation of nitrogen #2

# OBIC 0.1.0 2019-07-08
First version of OBIC