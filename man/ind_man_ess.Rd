% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/management.R
\name{ind_man_ess}
\alias{ind_man_ess}
\title{Calculate the indicator for sustainable management given a required ecoystem service}
\usage{
ind_man_ess(D_MAN, B_LU_BRP, B_SOILTYPE_AGR, type)
}
\arguments{
\item{D_MAN}{(numeric) The value of Sustainable Management calculated by \code{\link{calc_man_ess}}}

\item{B_LU_BRP}{(numeric) The crop code from the BRP}

\item{B_SOILTYPE_AGR}{(character) The type of soil}

\item{type}{(character) type of ecosystem service to evaluate the impact of soil management. Options: I_M_SOILFERTILITY, I_M_CLIMATE, I_M_WATERQUALITY, and I_M_BIODIVERSITY}
}
\value{
The evaluated score for the evaluated soil management for a specific ecosystem service. A numeric value between 0 and 1. 
This is done for the following ESS: I_M_SOILFERTILITY, I_M_CLIMATE, I_M_WATERQUALITY and I_M_BIODIVERSITY.
}
\description{
This function calculates the the sustainability of strategic management options for a given ecoystem service as calculated by \code{\link{calc_man_ess}}
The main source of this indicator is developed for Label Duurzaam Bodembeheer (Van der Wal, 2016)
}
\examples{
ind_man_ess(D_MAN = 3.5,B_LU_BRP = 1019, B_SOILTYPE_AGR = 'dekzand',type = 'I_M_SOILFERTILITY')
ind_man_ess(D_MAN = c(2,6,15), B_LU_BRP = c(1019,256,1019),B_SOILTYPE_AGR = rep('dekzand',3),
type = 'I_M_SOILFERTILITY')

}
