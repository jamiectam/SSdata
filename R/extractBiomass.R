#'@title Calls functions to extract and stratify fishery independent data
#'@description Calls\code{biomassData()} and \code{stratifyBiomass()}. For more
#'  informations, see the help files for those functions.
#'@inheritParams biomassData
#'@param areas Areas of interest. A separate dataframe will be extracted for each
#'  area. Default is \code{areas = c("shelf", "esswss", "nafo", "strat")}.
#'@return This function creates directories to store extracted data. Within each
#'  folder, a separate RData file stores the data for each year from s.year to
#'  e.year, for strata from s.strat to e.strat.
#'
#'  From \code{biomassData()}:
#'
#'  path/data/aggregate stores files num_biomYEAR.RData (object name is \code{dat}).
#'  \code{dat} has 9 columns: \code{MISSION}, \code{SETNO}, \code{SPECIES},
#'  \code{YEAR}, \code{STRAT}, \code{BIOMASS}, \code{ABUNDANCE}, \code{QBIOMASS}
#'  and \code{QABUNDANCE}.
#'
#'  path/data/length stores files num_biom_at_lengthYEAR.RData (object name is
#'  \code{out}). \code{out} has 10 columns: \code{MISSION}, \code{SETNO}, \code{SPECIES},
#'  \code{FLEN}, \code{YEAR}, \code{STRAT}, \code{BIOMASS}, \code{ABUNDANCE},
#'  \code{QBIOMASS} and \code{QABUNDANCE}.
#'
#'  path/data/lenwgt stores file lw2015.RData (object name wt). wt has 4
#'  columns:  \code{STRAT}, \code{SPECIES}, \code{FLEN}, and \code{FWT}.
#'
#'  From \code{stratifyBiomass()}: path/data/stratfied/lengthBased/area stores
#'  the length-based stratified, q-adjusted (post-stratification) biomass and
#'  abundance in files YEAR.Rdata (object name \code{out}). \code{out} has 5 columns:
#'  \code{SPECIES}, \code{ID}, \code{FLEN}, \code{BIOMASS}, \code{ABUNDANCE}.
#'
#'  path/data/stratfied/notlengthBased/area stores the not length-based
#'  stratified, q-adjusted (post-stratification) biomass and abundance in files
#'  YEAR.Rdata (object name \code{out}). \code{out} has 4 columns: \code{SPECIES}, \code{ID},
#'  \code{BIOMASS}, \code{ABUNDANCE}.
#'@references Original code by DD.
#'@export

extractBiomass <- function(path, s.year, e.year, 
                           areas = c("shelf", "esswss", "nafo", "strat")){
  
  # Extract biomass & abundance data
  biomassData(path = path, s.year = s.year, e.year = e.year)
  
  # Stratify biomass & abundance data
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, 
                  lengthBased = F, areas = areas)
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, 
                  lengthBased = T, areas = areas)
  
  print("Biomass data extracted & stratified")
}

