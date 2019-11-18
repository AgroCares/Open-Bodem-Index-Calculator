#' Linking table between crops and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' @format A data.frame with 405 rows and 12 columns:
#' \describe{
#'   \item{crop_name}{The name of the crop}
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_crumbleability}{The category for this crop at crumbleablity}
#'   \item{crop_phosphate}{The category for this crop at phosphate availability}
#'   \item{crop_sealing}{The category for this crop at soil sealing}
#'   \item{crop_n}{The category for this crop at nitrogen}
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
#' @format A data.frame with 303 rows and 18 columns:
#' \describe{
#'   \item{maatregel_nr}{The ID number of measure}
#'   \item{omschrijving}{The description of measure}
#'   \item{Prio_M}{weighing factor for measure. This is not used in the script.}
#'   \item{Dremp_S}{Threshold value of the indicator value. This is not used in the script.}
#'   \item{OBICvariable}{Name of OBIC soil indicator variable}
#'   \item{Ef_M}{Effect of measure on soil indicator. +++/++/+/0/nr/-}
#'   \item{filter}{whether the record is used (1) or unused (2)}
#'   \item{melkveehouderij (alleen gras)}{Relevance of the effect on dairy farms (1/0)}
#'   \item{melkveehouderij incl mais naast gras}{}
#'   \item{akkerbouw}{Relevance of the effect on arable farms (1/0)}
#'   \item{groente}{Relevance of the effect on vegetable (1/0)}
#'   \item{boomteelt}{Relevance of the effect on tree nurseries (1/0)}
#'   \item{klei}{Relevance of the effect on clay (1/0)}
#'   \item{veen}{Relevance of the effect on peat (1/0)}
#'   \item{zand}{Relevance of the effect on sand (1/0)}
#'   \item{loss}{Relevance of the effect on loss (1/0)}
#' }
"maatregel.obic"
