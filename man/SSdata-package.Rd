\name{SSdata-package}
\docType{package}
\title{
  Extracts Fishery Independent and Commercial Landings Data for the Scotian Shelf from DFO-Maritimes Databases
}

\description{ \pkg{SSdata} provides functions to extract fishery independent (summer research vessel groundfish survey) and commercial landings data for the Scotian Shelf from DFO-Maritimes databases. Following Bundy, Gomez, and Cook (2017), fishery independent data can be extracted for four different spatial scales: Shelf, ESS/WSS, NAFO divisions,and strata. Commercial landings can be extracted for three spatial scales: Shelf, ESS/WSS, and NAFO divisions. Dataframes can be formatted for use in the \code{marindicators} package.
 }

\details{ User must define \code{channel = odbcConnect("ptran", uid = ###, pwd = ###)} in the global environment using the 'RODBC' package. This channel must have access to tables from several databases (Table 1).

Table 1: Databases and tables for which the user's channel must have access to extract fisheries independent and commercial landings data.
\tabular{ll}{
  \bold{DATABASE} \tab \bold{TABLES}  \cr
  \bold{Fishery independent data} \tab \cr
  groundfish \tab gscat, gsdet, gsinf, gs_lengths \cr
  mfd_stomach \tab nafo_strat \cr
  \bold{Commercial landings data} \tab \cr
  comland \tab nafo_summary, nafo_area_codes  \cr
  cl \tab sub_trips_XXXX\eqn{^1}, identified_catches  \cr
  mfd_obfmi database \tab marfis_catch_effort  \cr
  gomezc database \tab indiseas_marfis2allcodes 

  }
  
\eqn{^1} _XXXX represents the 4-digit year; e.g., sub_trips_1999


Four RData files are stored in the package to help extract and format the data:

1. \code{strat_weights} has two columns: \code{STRAT} and the corresponding \code{AREA}. Each \code{AREA} is converted to tow units in \code{stratifyBiomass()} using the conversion \code{TOW UNITS = AREA/((35./6080.2)*1.75)} until 1981 and \code{TOW UNITS = AREA/((41./6080.2)*1.75)} after 1981.

2. \code{species_codes} has three columns: \code{ALLCODES} (commercial species code), \code{SPECIES} (research vessel species code), and \code{PROPORTION_OF_LANDINGS} (for species that have one \code{ALLCODES} value but more than one \code{SPECIES} value). Used in \code{LANDdataframe()} to replace commercial species codes with research vessel species codes.

3. \code{landings_groupings} links NAFO sub-unit to area \code{ID} for the spatial scales of interest (Shelf, ESS/WSS, NAFO divisions). Applied in \code{LANDdataframe()}.

4. \code{commercial_groups} links species to commercial groups. 11 columns: \code{ALLCODES} (commercial species code), \code{ALLNAMES} (commercial species name), and a column for each commercial group (\code{GROUNDFISH}, \code{CLUPEIDS}, \code{INVERTEBRATES}, \code{FORAGE}, \code{FLATFISH}, \code{FINFISH}, \code{LARGE_PELAGICS}, \code{GADOIDS}, \code{SKATES}). Species that belong to a commercial group have a \code{1} in the corresponding column (and \code{NA} in all other columns). Applied in \code{extractLAND()}.
}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 }

