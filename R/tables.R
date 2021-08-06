#' Linking table between crops and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' @format A data.frame with 465 rows and 21 columns:
#' \describe{
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_name}{The name of the crop, in lower case}
#'   \item{crop_waterstress}{Classification linking for linking crops to waterstress.obic}
#'   \item{crop_intensity}{Whether crop is root/tuber crop, rest crop, or other.}
#'   \item{crop_eos}{Effective soil organic matter produced by the crop in kg/ha}
#'   \item{crop_eos_residue}{Effective soil organic matter from plant residues in kg/ha}
#'   \item{crop_category}{Classification of crop per land use type (arable, maize, grass, nature)}
#'   \item{crop_rotation}{Classification of crop to determine function within crop rotations}
#'   \item{crop_crumbleability}{The category for this crop at crumbleablity}
#'   \item{crop_phosphate}{The category for this crop for evaluation phosphate availability}
#'   \item{crop_sealing}{The category for this crop at soil sealing}
#'   \item{crop_n}{The category for this crop for evaluation nitrogen}
#'   \item{crop_k}{The category for this crop for evaluation potassium}
#'   \item{crop_measure}{The category for this crop for evaluating measures}
#'   \item{nf_clay}{Allowed effective N dose on clay soils}
#'   \item{nf_sand.other}{Allowed effective N dose on sanndy soils}
#'   \item{nf_sand.sout}{Allowed effective N dose on sandy soils sensitive to leaching}
#'   \item{nf_loess}{Allowed effective N dose on loess soils}
#'   \item{nf_peat}{Allowed effective N dose on peat soils}
#'   \item{crop_name_scientific}{All-lower-case scientific name of the crop species. When crop is not species specific the genus of the crop is given}
#'   \item{crop_season}{Crop category for length growing season}
#' }
"crops.obic"

#' Linking table between soils and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' @format A data.frame with 7 rows and 2 columns:
#' \describe{
#'   \item{soiltype}{The name of the soil type}
#'   \item{soiltype.ph}{The category for this soil at pH}
#'   \item{soiltype.n}{The category for this soil at nitrogen}
#' }
"soils.obic"

#' Linking table between crops, soils, groundwatertables and water induced stresses in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' @format A data.frame with x rows and y columns:
#' \describe{
#'   \item{cropname}{The name of the crop}
#'   \item{soilunit}{The category for this soil, derived from 1:50.000 soil map}
#'   \item{gt}{The class describing mean highest and lowest groundwater table, derived from 1:50.000 soil map}
#'   \item{droughtstress}{The mean yield reduction due to drought (in percentage)}
#'   \item{wetnessstress}{The mean yield reduction due to watersurplus (in percentage)}
#'   \item{waterstress}{The mean combined effect water stress (due to deficiency or excess of water)}
#' }
"waterstress.obic"

#' Weight of indicators to calculate integrated scores
#' 
#' This table defines the weighting factors (ranging between 0 and 1) of indicator values to calculate integrated scores.
#' 
#' @format A data.frame with 20 rows and 2 columns:
#' \describe{
#'   \item{var}{The name of the weight}
#'   \item{weight}{weighing factor}
#' }
"weight.obic"

#' Effects of measures on soil indicators
#' 
#' This table defines the effects of 11 measures on soil indicators 
#' 
#' @format A data.frame with 7040 rows and 11 columns:
#' \describe{
#'   \item{m_nr}{The ID number of measure}
#'   \item{m_description}{The description of measure}
#'   \item{m_prio}{weighing factor for measure. This is not used in the script.}
#'   \item{m_treshold}{Threshold value of the indicator value. This is not used in the script.}
#'   \item{m_order}{Order of measures. When scores are tie, the measure with a smaller number is chosen.}
#'   \item{m_soilfunction}{description of the OBIC indicator varialbe}
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
#' @format A data.frame with 126 rows and 6 columns:
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
#' @format A data.frame with 126 rows and 6 columns:
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
#' @format A data.table with 29 rows and 5 columns:
#' \describe{
#'   \item{landuse}{The name of the crop or landuse category, used to link to crops.obic$crop_season}
#'   \item{req_days_pre_glg}{Required number of workable days before August 15 assuming this coincides with GLG, lowest groundwater}
#'   \item{req_days_post_glg}{Required number of workable days after August 15 assuming this coincides with GLG, lowest groundwater}
#'   \item{total_days}{Total number of days required for optimal growth or use}
#'   \item{derving}{Category to determine yield loss due to having a sub-optimal relative growing season length or RLG}
#' }
"season.obic"

#' Damage and reproduction of soil-borne pathogens and pests on crops
#' 
#' This table includes information from aaltjesschema (April 2021), a website where information is collected on the vulnerability of crops to plant parasitic nematodes and diseases that use nematodes as vector.
#' 
#' @format A data.table with 7059 rows and 21 columns:
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
#'   \item{info}{string whether there is information on propgation, diffrentiating between none, yes, and some}
#'   \item{name_common}{string, common name of pathogen in Dutch, if no common name is available, scientific name is given}
#'   \item{nema_name}{string, full name of pathogen in aaltjeschema, includes common and scientific name}
#'   \item{grondsoort}{string with letters indicating for which soil the information is valid}
#'   \item{groen_br}{boolean indicating that the crop is a green manure on fallow}
#'   \item{groen_vs}{boolean indicating that the crop is a green manure in early stubble}
#'   \item{groen_od}{boolean indicating that the crop is a green manure beneath cover crop}
#'   \item{groen_ls}{boolean indicating that the crop is a green manure in late stubble}
#'   \item{groen_st}{boolean indicating that the crop is a green manure as drifting deck}
#'   \item{crop_name_scientific}{string, scientific name of crop species or genus}
#' }
"nema.crop.rot.obic"

