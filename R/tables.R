#' Linking table between crops and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' \describe{
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_name}{The name of the crop, in lower case}
#'   \item{crop_waterstress}{Classification linking for linking crops to waterstress.obic}
#'   \item{crop_intensity}{Whether crop is root/tuber crop, rest crop, or other.}
#'   \item{crop_eos}{Effective soil organic matter produced by the crop in kg/ha}
#'   \item{crop_eos_residue}{Effective soil organic matter from plant residues in kg/ha}
#'   \item{crop_category}{Classification of crop per land use type (arable, maize, grass, nature)}
#'   \item{crop_rotation}{Classification of crop to determine function within crop rotations}
#'   \item{crop_crumbleability}{The category for this crop at crumbleability}
#'   \item{crop_phosphate}{The category for this crop for evaluation phosphate availability}
#'   \item{crop_sealing}{The category for this crop at soil sealing}
#'   \item{crop_n}{The category for this crop for evaluation nitrogen}
#'   \item{crop_k}{The category for this crop for evaluation potassium}
#'   \item{crop_measure}{The category for this crop for evaluating measures}
#'   \item{nf_clay}{Allowed effective N dose on clay soils}
#'   \item{nf_sand.other}{Allowed effective N dose on sandy soils}
#'   \item{nf_sand.south}{Allowed effective N dose on sandy soils sensitive to leaching}
#'   \item{nf_loess}{Allowed effective N dose on loess soils}
#'   \item{nf_peat}{Allowed effective N dose on peat soils}
#'   \item{crop_name_scientific}{All-lower-case scientific name of the crop species. When crop is not species specific the genus of the crop is given}
#'   \item{crop_season}{Crop category for length growing season}
#'   \item{crop_makkink}{Crop category for makkink correction factors}
#' }
#' @aliases crops.obic
"crops.obic"

#' Linking table between soils and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' \describe{
#'   \item{soiltype}{The name of the soil type}
#'   \item{soiltype.ph}{The category for this soil at pH}
#'   \item{soiltype.n}{The category for this soil at nitrogen}
#' }
"soils.obic"

#' Linking table between crops, soils, groundwater tables and water induced stresses in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' \describe{
#'   \item{cropname}{The name of the crop}
#'   \item{soilunit}{The category for this soil, derived from 1:50.000 soil map}
#'   \item{gt}{The class describing mean highest and lowest groundwater table, derived from 1:50.000 soil map}
#'   \item{droughtstress}{The mean yield reduction due to drought (in percentage)}
#'   \item{wetnessstress}{The mean yield reduction due to water surplus (in percentage)}
#'   \item{waterstress}{The mean combined effect water stress (due to deficiency or excess of water)}
#' }
"waterstress.obic"

#' Weight of indicators to calculate integrated scores
#' 
#' This table defines the weighting factors (ranging between 0 and 1) of indicator values to calculate integrated scores.
#' 
#' \describe{
#'   \item{var}{The name of the weight}
#'   \item{weight}{weighing factor}
#' }
"weight.obic"

#' Effects of measures on soil indicators
#' 
#' This table defines the effects of 11 measures on soil indicators 
#' 
#' \describe{
#'   \item{m_nr}{The ID number of measure}
#'   \item{m_description}{The description of measure}
#'   \item{m_prio}{weighing factor for measure. This is not used in the script.}
#'   \item{m_treshold}{Threshold value of the indicator value. This is not used in the script.}
#'   \item{m_order}{Order of measures. When scores are tie, the measure with a smaller number is chosen.}
#'   \item{m_soilfunction}{description of the OBIC indicator variable}
#'   \item{indicator}{Name of OBIC soil indicator variable}
#'   \item{m_effect}{Effect of measure on soil indicator. 3/2/1/0/-1}
#'   \item{m_sector}{type of agricultural sector: dairy/arable/vegetable/tree cultivation (in dutch)}
#'   \item{m_soiltype}{type of soil: sand/clay/peat/loess (in dutch)}
#'   \item{m_applicability}{is the measure applicable for combination of sector and soil (1/0)}
#' }
"recom.obic"

#' Column description for the OBIC
#' 
#' This table defines the columns used in the OBIC and which unit is used
#' 
#' \describe{
#'   \item{column}{The column name used in OBIC}
#'   \item{type}{The type of column}
#'   \item{description_nl}{A description of the column in Dutch}
#'   \item{description_en}{A description of the column in English}
#'   \item{unit}{The unit used for this column}
#'   \item{method}{The method to measure/obtain the values for this column}
#' }
"column_description.obic"

#' Nematode table
#' 
#' This table contains information uses for calculations on nematode species counts
#' 
#' \describe{
#'   \item{geel}{The intermediate infestation severity count}
#'   \item{rood}{The count at which a severe infestation is present}
#'   \item{species}{The species or sometimes genera of the plant parasitic nematode}
#'   \item{standard}{A boolean indicating whether the species should always be used in calculating the indicator score, regardless of the number of nematodes}
#'   \item{b}{Growth rate (b) for the evaluate_logistics function}
#'   \item{v}{v for the evaluate_logistics function, affects the growth rate near the maximum}
#' }
"nema.obic"

#' Desired growing season period for maximum yield
#' 
#' This table gives the required number of days before and after August 15 required for optimal yield or usability and has categories to determine yield loss having a shorter workable growing season based on Tabel 2 and several formulas from Huinink (2018)
#' 
#' \describe{
#'   \item{landuse}{The name of the crop or landuse category, used to link to crops.obic$crop_season}
#'   \item{req_days_pre_glg}{Required number of workable days before August 15 assuming this coincides with GLG, lowest groundwater}
#'   \item{req_days_post_glg}{Required number of workable days after August 15 assuming this coincides with GLG, lowest groundwater}
#'   \item{total_days}{Total number of days required for optimal growth or use}
#'   \item{derving}{Category to determine yield loss due to having a sub-optimal relative growing season length or RLG}
#' }
"season.obic"

#' Example dataset for use in OBIC package
#' 
#' This table contains a series of agricultural fields with soil properties needed for illustration OBIC.
#' 
#' \describe{
#'    \item{ID}{A field id (numeric)}
#'    \item{YEAR}{The year that the crop is grown (integer)}
#'    \item{B_LU_BRP}{A series with crop codes given the crop rotation plan (integer, source: the BRP)}
#'    \item{B_SC_WENR}{The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006) (character).}
#'    \item{B_GWL_CLASS}{The groundwater table class (character)}
#'    \item{B_SOILTYPE_AGR}{The agricultural type of soil (character)}
#'    \item{B_HELP_WENR}{The soil type abbreviation, derived from 1:50.000 soil map (character)}
#'    \item{B_AER_CBS}{The agricultural economic region in the Netherlands (CBS, 2016) (character)}
#'    \item{A_SOM_LOI}{The percentage organic matter in the soil (\%) (numeric)}
#'    \item{A_CLAY_MI}{The clay content of the soil (\%) (numeric)}
#'    \item{A_SAND_MI}{The sand content of the soil (\%) (numeric)}
#'    \item{A_SILT_MI}{The silt content of the soil (\%) (numeric)}
#'    \item{A_PH_CC}{The acidity of the soil, measured in 0.01M CaCl2 (-) (numeric)}
#'    \item{A_CACO3_IF}{The carbonate content of the soil (\%) (numeric)}
#'    \item{A_N_RT}{The organic nitrogen content of the soil in mg N / kg (numeric)}
#'    \item{A_CN_FR}{The carbon to nitrogen ratio (-) (numeric)}
#'    \item{A_COM_FR}{The carbon fraction of soil organic matter (\%) (numeric)}
#'    \item{A_S_RT}{The total Sulfur content of the soil (in mg S per kg) (numeric)}
#'    \item{A_N_PMN}{The potentially mineralizable N pool (mg N / kg soil) (numeric)}
#'    \item{A_P_AL}{The P-AL content of the soil (numeric)}
#'    \item{A_P_CC}{The plant available P content, extracted with 0.01M CaCl2 (mg / kg) (numeric)}
#'    \item{A_P_WA}{The P-content of the soil extracted with water (mg P2O5 / 100 ml soil) (numeric)}
#'    \item{A_CEC_CO}{The cation exchange capacity of the soil (mmol+ / kg), analysed via Cobalt-hexamine extraction (numeric)}
#'    \item{A_CA_CO_PO}{The The occupation of the CEC with Ca (\%) (numeric)}
#'    \item{A_MG_CO_PO}{The The occupation of the CEC with Mg (\%) (numeric)}
#'    \item{A_K_CO_PO}{The occupation of the CEC with K (\%) (numeric)}
#'    \item{A_K_CC}{The plant available K content, extracted with 0.01M CaCl2 (mg / kg) (numeric)}
#'    \item{A_MG_CC}{The plant available Mg content, extracted with 0.01M CaCl2 (ug / kg) (numeric)}
#'    \item{A_MN_CC}{The plant available Mn content, extracted with 0.01M CaCl2 (ug / kg) (numeric)}
#'    \item{A_ZN_CC}{The plant available Zn content, extracted with 0.01M CaCl2 (ug / kg) (numeric)}
#'    \item{A_CU_CC}{The plant available Cu content, extracted with 0.01M CaCl2 (ug / kg) (numeric)}
#'    \item{A_EW_BCS}{The presence of earth worms (optional, score 0-1-2, numeric)}
#'    \item{A_SC_BCS}{The presence of compaction of subsoil (optional, score 0-1-2, numeric)}
#'    \item{A_GS_BCS}{The presence of waterlogged conditions, gley spots (optional, score 0-1-2, numeric)}
#'    \item{A_P_BCS}{The presence / occurrence of water puddles on the land, ponding (optional, score 0-1-2, numeric)}
#'    \item{A_C_BCS}{The presence of visible cracks in the top layer (optional, score 0-1-2, numeric)}
#'    \item{A_RT_BCS}{The presence of visible tracks / rutting or trampling on the land (optional, score 0-1-2, numeric)}
#'    \item{A_RD_BCS}{The rooting depth (optional, score 0-1-2, numeric)}
#'    \item{A_SS_BCS}{The soil structure (optional, score 0-1-2, numeric)}
#'    \item{A_CC_BCS}{he crop cover on the surface (optional, score 0-1-2, numeric)}
#'    \item{M_COMPOST}{The frequency that compost is applied (optional, every x years, numeric)}
#'    \item{M_GREEN}{A soil measure. Are catch crops sown after main crop (optional, option: yes or no, boolean)}
#'    \item{M_NONBARE}{A soil measure. Is parcel for 80 percent of the year cultivated and 'green' (optional, option: yes or no, boolean)}
#'    \item{M_EARLYCROP}{A soil measure. Use of early crop varieties to avoid late harvesting (optional, option: yes or no, boolean)}
#'    \item{M_SLEEPHOSE}{A soil measure. Is sleephose used for slurry application (optional, option: yes or no, boolean)}
#'    \item{M_DRAIN}{A soil measure. Are under water drains installed in peaty soils (optional, option: yes or no, boolean)}
#'    \item{M_DITCH}{A soil measure. Are ditched maintained carefully and slib applied on the land (optional, option: yes or no, boolean)}
#'    \item{M_UNDERSEED}{A soil measure. Is grass used as second crop in between maize rows (optional, option: yes or no, boolean)}
#'    \item{M_LIME}{A soil measure. Has field been limed in last three years (option: yes or no, boolean)}
#'    \item{M_NONINVTILL}{A soil measure. Non inversion tillage (option: yes or no, boolean)}
#'    \item{M_SSPM}{A soil measure. Soil Structure Protection Measures, such as fixed driving lines, low pressure tires, and light weighted machinery (option: yes or no, boolean)}
#'    \item{M_SOLIDMANURE}{A soil measure. Use of solid manure (option: yes or no, boolean)}
#'    \item{M_STRAWRESIDUE}{A soil measure. Application of straw residues (option: yes or no, boolean)}
#'    \item{M_MECHWEEDS}{A soil measure. Use of mechanical weed protection (option: yes or no, boolean)}
#'    \item{M_PESTICIDES_DST}{A soil measure. Use of DST for pesticides (option: yes or no, boolean)}
#' }
"binnenveld"


#' Relational table linking soil management measures to ecosystem services
#' 
#' This table assigns which measures positively contribute to the ecosystem services included
#' 
#' \describe{
#'   \item{measure}{The name of measure}
#'   \item{I_M_SOILFERTILITY}{integrated soil management indicator for soil fertility}
#'   \item{I_M_CLIMATE}{integrated soil management indicator for soil carbon sequestration}
#'   \item{I_M_WATERQUALITY}{integrated soil management indicator for water quality}
#'   \item{I_M_BIODIVERSITY}{Integrated soil management indicator for soil biodiversity}
#' }
"management.obic"

#' Damage and reproduction of soil-borne pathogens and pests on crops
#' 
#' This table includes information from aaltjesschema (April 2021), a website where information is collected on the vulnerability of crops to plant parasitic nematodes and diseases that use nematodes as vector.
#' 
#' \describe{
#'   \item{crop}{crop as called in aaltjesschema}
#'   \item{name_scientific}{scientific name of nematode}
#'   \item{propagation}{how easily a nematode can propagate on a crop given as strings with 5 classes}
#'   \item{damage}{strings indicating how much damage a nematode can inflict on a crop, with 5 classes}
#'   \item{cultivar_dependent}{boolean whether there are differences in propgation between cultivars of the crop}
#'   \item{serotype_dependant}{boolean whether there are differences in propagation between serotypes of the pathogen}
#'   \item{dalgrond}{boolean whether information is valid for soiltype 'dalgrond'}
#'   \item{klei}{boolean whether information is valid for soiltype 'klei'}
#'   \item{loess}{boolean whether information is valid for soiltype 'loess'}
#'   \item{zand}{boolean whether information is valid for soiltype 'zand'}
#'   \item{zavel}{boolean whether information is valid for soiltype 'zavel'}
#'   \item{info}{string whether there is information on propgation, differentiating between none, yes, and some}
#'   \item{name_common}{string, common name of pathogen in Dutch, if no common name is available, scientific name is given}
#'   \item{nema_name}{string, full name of pathogen in aaltjesschema, includes common and scientific name}
#'   \item{grondsoort}{string with letters indicating for which soil the information is valid}
#'   \item{groen_br}{boolean indicating that the crop is a green manure on fallow}
#'   \item{groen_vs}{boolean indicating that the crop is a green manure in early stubble}
#'   \item{groen_od}{boolean indicating that the crop is a green manure beneath cover crop}
#'   \item{groen_ls}{boolean indicating that the crop is a green manure in late stubble}
#'   \item{groen_st}{boolean indicating that the crop is a green manure as drifting deck}
#'   \item{crop_name_scientific}{string, scientific name of crop species or genus}
#' }
"nema.crop.rot.obic"



#' Weather table
#'
#' This table contains the climatic weather data of the Netherlands for the period 1990-2020
#'
#' \describe{
#'   \item{month}{Month of the year}
#'   \item{A_TEMP_MEAN}{Mean monthly temperature}
#'   \item{A_PREC_MEAN}{Mean monthly precipitation}
#'   \item{A_ET_MEAN}{Mean monthly evapo-transpiration}
#' }
"weather.obic"


#' Makkink correction factor table
#'
#' This table contains the makkink correction factors for evapo-transpiration per month
#'
#' \describe{
#'   \item{crop_makkink}{Makkink crop category}
#'   \item{1}{Evapotranspiration correction factors for January}
#'   \item{2}{Evapotranspiration correction factors for February}
#'   \item{3}{Evapotranspiration correction factors for March}
#'   \item{4}{Evapotranspiration correction factors for April}
#'   \item{5}{Evapotranspiration correction factors for May}
#'   \item{6}{Evapotranspiration correction factors for June}
#'   \item{7}{Evapotranspiration correction factors for July}
#'   \item{8}{Evapotranspiration correction factors for August}
#'   \item{9}{Evapotranspiration correction factors for September}
#'   \item{10}{Evapotranspiration correction factors for October}
#'   \item{11}{Evapotranspiration correction factors for November}
#'   \item{12}{Evapotranspiration correction factors for December}
#' }
"crops.makkink"

#' Table with fractions of excess N which runs off to groundwater and surface water
#' 
#' This table contains the fractions of N overshot which runs off to groundwater / surface water, per soil type, crop type, and groundwater table
#' 
#' \describe{
#'   \item{gewas}{crop type}
#'   \item{bodem}{soil type}
#'   \item{ghg}{Lower value for groundwater table (cm-mv)}
#'   \item{glg}{Upper value for groundwater table (cm-mv)}
#'   \item{B_GT}{grondwatertrap}
#'   \item{nf}{Original values of N run-off fraction to surface water (kg N drain/ha/year per kg N overschot/ha/year) or groundwater (mg NO3/L per kg N overschot/ha/year)}
#'   \item{leaching_to-set}{Tells if leaching to ground water or surface water)}
#' }
#' 
#' 
"nleach_table"

#' Table with water retention properties of 'bouwstenen'
#' 
#' This table contains water retention curve parameters and typical mineral composition of 18 'bouwstenen'
#' 
#' \describe{
#'   \item{bouwsteen}{soil type bouwsteen}
#'   \item{omschrijving}{description of 'bouwsteen'}
#'   \item{thres}{residual water content (cm3/cm3). Table 3 of Wosten 2001}
#'   \item{thsat}{water content at saturation (cm3/cm3). Table 3 of Wosten 2001}
#'   \item{Ks}{saturated hydraulic conductivity (cm/d). Table 3 of Wosten 2001}
#'   \item{alpha}{parameter alpha of pF curve (1/cm) Table 3 of Wosten 2001}
#'   \item{l}{parameter l of pF curve (-). Table 3 of Wosten 2001}
#'   \item{n}{parameter n of pF curve (-). Table 3 of Wosten 2001}
#'   \item{sand\%}{sand content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{silt\%}{silt content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{clay\%}{clay content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{OM\%}{organic matter content (\%). Middle value of Table 1 of Wosten 2001}
#'   \item{bulkdensity}{soil bulk density (g/cm3). Middle value of Table 2 of Wosten 2001}
#'   \item{M50}{size of sand particles (um). Middle value of Table 2 of Wosten 2001}
#' }
"bouwsteen_tb"

#' Table with optimal pH for different crop plans
#' 
#' This table contains the optimal pH for different crop plans and soil types
#' 
#' \describe{
#'   \item{table}{The original table from Hanboek Bodem en Bemesting}
#'   \item{lutum.low}{Lower value for A_CLAY_MI}
#'   \item{lutum.high}{Upper value for A_CLAY_MI}
#'   \item{om.low}{Lower value for organic matter}
#'   \item{om.high}{Upper value for organic matter}
#'   \item{potato.low}{Lower value for fraction potatoes in crop plan}
#'   \item{potato.high}{Upper value for fraction potatoes in crop plan}
#'   \item{sugarbeet.low}{Lower value for fraction potatoes in crop plan}
#'   \item{sugarbeet.high}{Upper value for fraction potatoes in crop plan}
#'   \item{ph.optimum}{The optimal pH (pH_CaCl2) for this range}   
#' }
#' 
#' #' @references Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3
#' 
"tbl.ph.delta"
