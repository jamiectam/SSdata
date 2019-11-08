#'@title Calls functions to extract and stratify fishery independent data
#'@description Runs\code{biomassData()} to extract survey data and and run
#'  \code{stratifyBiomass()} to stratify biomass and abundance estimates.
#'@inheritParams biomassData
#'@inheritParams stratifyBiomass
#'@param areas Areas of interest. A separate dataframe will be extracted for
#'  each area. Default is \code{areas = c("shelf", "esswss", "nafo", "strat")}.
#'@return Returns data extracted and stratified by \code{biomassData()} and
#'  \code{stratifyBiomass()}.
#'  
#'  From \code{biomassData()}:
#'  
#'  path/data/aggregate stores BIOMASS, ABUNDANCE, QBIOMASS and QABUNDANCE at
#'  the set level.
#'
#'  path/data/length stores BIOMASS, ABUNDANCE, QBIOMASS and QABUNDANCE at the
#'  set level for LENGTH in 1 cm increments. 
#'  
#'  From\code{stratifyBiomass()}:data/stratified/folder/. The name of "folder" depends on the arguments
#'  \code{lengthbased} and \code{qadjusted}:
#'
#'  if(lengthBased & qadjusted), then folder is named "lengthbased_qadj"
#'
#'  if(lengthBased & !qadjusted), then folder is named "lengthbased_notqadj"
#'
#'  if(!lengthBased & qadjusted), then folder is named "notlengthbased_qadj"
#'
#'  if(!lengthBased & qadjusted), then folder is named "notlengthbased_notqadj"
#'
#'  Inside "folder" is a folder for each entry in \code{area}. Within each area
#'  folder is a .RData file for each year from s.year to e.year called
#'  year.RData (object name \code{out}).
#'
#'  \code{out} has 5 columns if \code{lengthbased = TRUE}: \code{SPECIES} ,
#'  \code{ID}, \code{FLEN}, \code{BIOMASS}, and \code{ABUNDANCE}.
#'
#'  \code{out} has 4 columns if \code{lengthbased = FALSE}: \code{SPECIES},
#'  \code{ID}, \code{BIOMASS}, and \code{ABUNDANCE}.
#'  
#'@references Original code by DD.
#'@export

extractRV <- function(path, s.year, e.year, lengthbased, qadjusted,
                           areas = c("shelf", "esswss", "nafo", "strat")){
  
  # Extract biomass & abundance data
  biomassData(path = path, s.year = s.year, e.year = e.year)
  
  # Stratify biomass & abundance data
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, 
                      lengthbased = lengthbased, qadjusted = qadjusted, areas = areas)
  
  
  print("Biomass data extracted & stratified")
}

