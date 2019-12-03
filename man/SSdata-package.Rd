\name{SSdata-package}
\docType{package}
\title{
  Extracts Fishery Independent and Dependent Data for the Scotian Shelf from DFO-Maritimes Databases
}

\description{ \pkg{SSdata} provides functions to extract fishery independent and dependent data for the Scotian Shelf from DFO-Maritimes databases. Following Bundy, Gomez, and Cook (2017), fishery independent data can be extracted for four different spatial scales: Shelf, Ess/WSS, NAFO divisions,and strata. Commercial landings data are not readily available at the strata scale, and so fishery dependent data can be extracted for three spatial scales: Shelf, Ess/WSS, and NAFO divisions. Dataframes can be formatted for use in the \code{marindicators package}.
 }

\details{ User must define \code{channel = odbcConnect("ptran", uid = ###, pwd = ###)} in the global environment. This channel must have access to the XXXX database to extract fishery independent data and the NAFO, ZIF, and MARFIS databases to extract the commercial landings data.

XX excel files in folder path/extra info are required to extract and format the code:

1. Herring files (**update this when sorted out**).

2. stratweights.csv has two columns: \code{STRAT} and the corresponding \code{AREA}. Each \code{AREA} is converted to tow units in \code{stratifyBiomass()}, \code{herringAggregate()}, and \code{herringAtLength()} using the conversion \code{TOW UNITS = AREA/((35./6080.2)*1.75)} until 1981 and \code{TOW UNITS = AREA/((41./6080.2)*1.75)} after 1981.

3. SpeciesCodes.csv has three columns: \code{ALLCODES} (commercial species code), \code{SPECIES} (research vessel species code), and \code{PROPORTION_OF_LANDINGS} (for species that have one \code{ALLCODES} value but more than one \code{SPECIES} value). Used in \code{LANDdataframe()} to replace commercial species codes with research vessel species codes (for use with \code{marindicators}).

4. landingsgroupings.csv links NAFO sub-unit to area \code{ID} for the spatial scales of interest (Shelf, ESS/WSS, NAFO divisions). Applied in \code{LANDdataframe()}.

5. commercialGroups.csv links species to commercial groups. 11 columns: \code{ALLCODES} (commercial species code), \code{ALLNAMES} (commercial species name), and a column for each commercial group (\code{GROUNDFISH}, \code{CLUPEIDS}, \code{INVERTEBRATES}, \code{FORAGE}, \code{FLATFISH}, \code{FINFISH}, \code{LARGE_PELAGICS}, \code{GADOIDS}, \code{SKATES}). Species that belong to a commercial group have a \code{1} in the corresponding column (and \code{NA} in all other columns). Applied in \code{extractLAND()}.
}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 }
