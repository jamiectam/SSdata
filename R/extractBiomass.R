#'@title Calls functions to extract and stratify fishery independent data
#'@description Calls\code{biomassData()} and \code{stratifyBiomass()}. For more
#'  informations, see the help files for those functions.
#'@inheritParams biomassData
#'@return This function creates directories to store extracted data. Within each
#'  folder, a separate RData file stores the data for each year from s.year to
#'  e.year, for strata from s.strat to e.strat.
#'
#'  From \code{biomassData()}:
#'  path/data/aggregate stores BIOMASS, ABUNDANCE, QBIOMASS and QABUNDANCE at
#'  the set level. 
#'  
#'  path/data/length stores BIOMASS, ABUNDANCE, QBIOMASS and
#'  QABUNDANCE at the set level for lengths in 1 cm increments. Used in. . .
#'  
#'  path/data/lenwgt stores fish length at weight.
#'  
#'  From \code{stratifyBiomass()}:
#'  path/data/stratfied/lengthBased stores the length-based stratified,
#'  q-adjusted (post-stratification) BIOMASS and ABUNDANCE.
#'  
#'  path/data/stratfied/notlengthBased stores the not length-based stratified,
#'  q-adjusted (post-stratification) BIOMASS and ABUNDANCE.
#'@references Original code by DD.
#'@export

extractBiomass <- function(path, s.year, e.year, areas){
  
  # Extract biomass & abundance data
  biomassData(path = path, s.year = start.year, e.year = end.year)
  
  # Stratify biomass & abundance data
  stratifyBiomass(path = path, s.year = start.year, e.year = end.year, 
                  qadjPostStrat = T, lengthBased = F, areas = areas)
  stratifyBiomass(path = path, s.year = start.year, e.year = end.year, 
                  qadjPostStrat = T, lengthBased = T, areas = areas)
}

