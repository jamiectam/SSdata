#'@title Exports fishery independent and commercial landings data
#'@description Exports fishery independent and commercial landings
#'  dataframes in a format suitable for the \code{marindicators} package.

#'@param path Filepath indicating where to create folders to store the output.
#'@param s.year Year for which to begin data compilation.
#'@param s.year Year for which to end data compilation.
#'@param areas.RV Areas for which to compile fishery independent data. Default
#'  is \code{areas.RV = c("strat", "nafo", "shelf", "esswss")}.
#'@param areas.RV Areas for which to compile commercial landings data. Default
#'  is \code{areas.RV = c("nafo", "shelf", "esswss")}.
#'@return The output is saved in directories created by the function:
#'
#'  path/output/Landings: An Rdata file of annual landings data with columns
#'  \code{ID}, \code{YEAR}, \code{SPECIES}, \code{CATCH}. The species codes in
#'  \code{SPECIES} are the RV codes. See ?LANDdataframe for more information.
#'  File name is area_landings.RData; object name is land.
#'
#'  path/output/LengthWeight: An Rdata file of annual length-weight data with
#'  columns \code{ID}, \code{YEAR}, \code{SPECIES}, \code{LENGTH},
#'  \code{WEIGHT}. File name is area_LengthWeight.RData; object name is
#'  \code{lw}.
#'
#'  path/output/RV/lengthBased: annual length-based biomass and abundance data,
#'  stratified and then q-corrected, with columns \code{ID}, \code{YEAR},
#'  \code{SPECIES}, \code{LENGTH}, \code{BIOMASS} and \code{ABUNDANCE}. File
#'  name is area_lengthBased.RData; object name is \code{RVdata}.
#'
#'  path/output/RV/notlengthBased: annual biomass and abundance data, stratified
#'  and then q-corrected, with columns \code{ID}, \code{YEAR}, \code{SPECIES},
#'  \code{BIOMASS} and \code{ABUNDANCE}. File name is area_notlengthBased.RData;
#'  object name is \code{RVdata}.

#'@references Original code by DD.
#'@export


compileDataframes <- function(path, s.year, e.year, areas.RV = c("strat", "nafo", "shelf", "esswss"), 
                              areas.land, 
                              csv = F, Rdata = TRUE){
  
  # Extract and stratify fishery independent data
  extractBiomass(path = path, s.year = s.year, e.year = e.year, areas = areas.RV)
  # Extract landings data
  extractLandings(path) 
  
  # Format fishery independent data
  RVdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, lengthBased = FALSE, csv = csv, Rdata = Rdata)
  
  RVdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, lengthBased = TRUE, csv = csv, Rdata = Rdata)
  
  LWdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, csv = csv, Rdata = Rdata)
  
  
  # Format landings data and use RV species code!
  LANDdataframe(path = path, areas = areas.land, csv = csv, Rdata = Rdata)
  
}