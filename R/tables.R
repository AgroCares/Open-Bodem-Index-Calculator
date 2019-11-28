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