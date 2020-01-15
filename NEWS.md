# Changelog OBIC

## UNRELEASED
## Added
* Adds two other pedotransfer functions (PTFs) to calculate water retention parameters #53

## Changed
* Improved the function for crumbleability to macht the factsheet #54 #56
* Optimal pH values of grassland are now the middle value of the class `good` #55
* pF of field capacity is changed from 2.2 to 2.0 #53
* Bulk density in the default PTF is now aggregated into 1 continous function #53
* Parameters of evaluation for PAW are adjusted #53
* Parameters for the evaluation of sealing are adjusted #57
* Changes the formula to calculate nitrogen leaching and run-off #58
* Parameters for the evaluation of nretention are adjusted #58
* Paramaters and funs for CEC are updated and split for soil fertility and aggregate stability #59
* Fix error in calc_potassium_availability #59
* Parameters for the evaluation of Cu availability are adjusted #59

## Version 0.10.0 2019-12-13
### Added
* Adds the evaluation of measures #37
* Adds giving recommendations based on the OBI score #37
* A relative score per reference group is added #31
* Adds the leaching of nitrogen to surface water and groundwater #17
* Adds a table with the description of the columns and a vignette about it #46
* Adds scoring for environmental performance #31

### Changed
* Update of crumbeability groups in `crops.obic`
* Maximum value of `A_MN_CC` is increased from 30000 to 250000
* The code for evaluating  in `ind_crumbeability` is simplified
* M_M10 user input changed in `D_CP_RUST` input, plus comments added #29
* new table with weighing factors included 'weight.obic' #32
* weighing factors updated in obic_indicators.R #32
* Improved the zinc indicator function #39
* The evaluation function for sulphur on maize and grass is updated #40
* The management function is improved, especially for maize #38
* The maximum score of `ind_management` is adjusted #38
* The parameter values for sulphur on arable land is adjusted #41
* The optimal pH for grassland depends now on organic matter content and presence of clover #42
* The score aggragation is now 60% average of last 4 years and 40% of older years #31
* The maximum value for `D_CR` is limited to 10 #47
* The parameters for the evaluation for `I_C_K` is set to the same as grassland #51
* The evaluation for `I_C_N` is now split into grassland and arable land #50
* Set lifecyle status to `maturing`
* The evaluation of `I_C_MG` is adjusted according to factsheet

### Fixed
* Preprocessing, indicators and scores make use of internal copy of the table now rather than referencing to an object outside the scope of the function
* Fixed the droughtstress, wetnessstress and waterstress values for GtI in `waterstress.obic`
* For grasslands on peat pr clay, the `D_MG` is now scaled #51
* Limit `D_SLV` between -30 and 250 

## Version 0.9.0 2019-10-22
### Changed
* The uppper limit for `D_BCS` is increased from 40 to 50
* Switch on crumbleability
* For `calc_phosphate_availability` the category `arable` is added for the crop categories
* Changed evaluation of sulphur for arable fields #26

### Fixed
* Fixed typo if mais in `ind_managment`
* Fixed test for winderodibility
* Use the correct correction factor in `calc_sealing_risk` #19
* Fix for calculating `I_P_CEC` #24
* Fix for calculating difficult values in `calc_sombalance` #25
* Fix for `calc_sbal_arable` where combinations of soil type and region that do not exist in table 6.2 of Handboek Bodem & Bemesting gave a NA #26
* Fix for `calc_bcs` where a value of 1 is given instead of 0 for `bcs_om` #28

## Version 0.8.0 2019-08-02
### Added
* Adds index and evaluation of the BodemConditieScore
* Adds potassium to main functions
* Adds a random recommondation to the output

### Changed
* The upper limit for `A_MN_CC` is increased from 20000 to 30000
* The upper limit for `A_CA_CEC` is increased from 100 to  400

### Fixed
* The indicator of pH had the wrong direction 
* Removed the minus sign before `ind_zinc`
* The function `calc_potassium_availability` containted a wrong column name for `A_PH_CC`
* Some unknown situations at `calc_potassium_availability` have now the same values as comparable situations. This should be looked at later

## Version 0.7.0 2019-08-02
### Added
* Adds index and evaluation of waterretention paramaters
* Adds index and evaluation of water related yield stress
* Adds index and evaluation of metals (Zn and Cu)
* Adds index and evaluation of microbial activity (PMN)
* Adds index and evaluation of CEC for soil fertility and structure
* Adds waterstress.obic table with information of HELP-tables

### Changed
* Change crops.obic with crop category related to water stress


## Version 0.6.0 2019-08-01
### Added
* Adds index and evaluation of K-availability

## Version 0.5.0 2019-07-31
### Added
* Adds crop classification (sensitivity to P, K and S supply)
* Adds soil organic matter balance (simple one)
* Adds risk for soil compaction (following risk map Van den Akker, 2013)
* Adds index and evaluation of Mg-availability
* Adds index and evaluation of S-availability (evaluation function need still to be parameterized)
* Adds index and evaluation of winderodibility
* Adds index and evaluation of soil strategic management (following Label Duurzaam Bodembeheer)
* Adds columns `crop_eos` and `crop_eos_residue` in `crops.obic`

## Version 0.4.0 2019-07-26
### Added
* The existing functions for pH, P, N, crumbleability, sealing and resistance are intergrated into `obic()`

### Changed
* The column name `brp` is changed to `B_LU_BRP`

## Version 0.3.0 2019-07-24
### Added
* Adds main function that wraps all the functions needed to calculated the OBI score and recommendations
* Adds documentation website for package using `pkgdown` #3

## Version 0.2.0 2019-07-12
### Added
* Adds evaluation of pH #1
* Adds evaluation of nitrogen #2

## Version 0.1.0 2019-07-08
First version of OBIC