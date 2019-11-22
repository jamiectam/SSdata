#'@title Exports fishery independent and commercial landings data
#'@description Exports fishery independent and commercial landings dataframes in
#'  a format suitable for the \code{marindicators} package. These dataframes
#'  correspond to \code{X}, \code{X_length}, \code{LenWt.table}, and \code{land}
#'  in the \code{marindicators} package.
#'@details Calls \code{biomassData()}, \code{stratifyBiomass()},
#'  \code{RVdataframe()}, \code{LWdataframe()}, and \code{LANDdataframe()}.
#'
#'  User must define \code{channel = odbcConnect("ptran", uid = ###, pwd = ###)}
#'  in the global environment. This channel must have access to the XXXX (RV)
#'  and NAFO, ZIF, and MARFIS (commercial) databases.
#'@inheritParams biomassData
#'@param path Filepath indicating where to create folders to store the output.
#'@param s.year Year for which to begin data compilation.
#'@param e.year Year for which to end data compilation.
#'@param areas.RV Areas for which to compile fishery independent data. Default
#'  is \code{areas.RV = c("strat", "nafo", "esswss", "shelf")}.
#'@param areas.land Areas for which to compile commercial landings data. Default
#'  is \code{areas.RV = c("nafo", "esswss", "shelf")}.
#'@param csv Logical value indicating whether to export dataframe as a .csv
#'  file. Default is \code{csv = TRUE}.
#'@param rdata Logical value indicating whether to export dataframe as a .RData
#'  file. Default is \code{rdata = TRUE}.
#'@return The output is saved in directories created by the functions:
#'
#'  From \code{RVdataframe}: path/output/RV/area/lengthbased: annual
#'  length-based biomass and abundance data, q-corrected and then stratified,
#'  with columns \code{ID}, \code{YEAR}, \code{SPECIES}, \code{LENGTH},
#'  \code{BIOMASS} (UNITS) and \code{ABUNDANCE} (UNITS). File name is
#'  area_lengthBased.RData; object name is \code{RVdata}.
#'
#'  path/output/RV/area/notlengthBased: annual biomass and abundance data,
#'  q-corrected and then stratified, with columns \code{ID}, \code{YEAR},
#'  \code{SPECIES}, \code{BIOMASS} (UNITS) and \code{ABUNDANCE} (UNITS). File
#'  name is area_notlengthBased.RData; object name is \code{RVdata}.
#'
#'  From \code{LWdataframe()}: path/output/LengthWeight: An Rdata file of annual
#'  length-weight data with columns \code{ID}, \code{YEAR}, \code{SPECIES},
#'  \code{LENGTH} (cm), \code{WEIGHT} (UNITS). File name is
#'  area_LengthWeight.RData; object name is \code{lw}.
#'
#'  From \code{LANDdatafrmae()}: path/output/Landings: An Rdata file of annual
#'  landings data with columns \code{ID}, \code{YEAR}, \code{SPECIES},
#'  \code{CATCH} (UNITS). The species codes in \code{SPECIES} are the RV codes.
#'  See \code{?LANDdataframe} for more information. File name is
#'  area_landings.RData; object name is land.
#'  
#'@references Original code by DD.
#'@export


compileDataframes <- function(path, s.year, e.year, areas.RV = c("strat", "nafo", "shelf", "esswss"),
                              areas.land, csv = FALSE, rdata = TRUE){

  # Extract fishery independent data
  biomassData(path = path, s.year = s.year, e.year = e.year, s.strat = 440, e.strat = 495,
              vessel.correction = TRUE)
  
  # Stratify fishery independent data
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, lengthbased = TRUE, qadjusted = TRUE,
                  areas = areas.RV)
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, lengthbased = TRUE, qadjusted = FALSE,
                  areas = areas.RV)
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, lengthbased = FALSE, qadjusted = TRUE,
                  areas = areas.RV)
  stratifyBiomass(path = path, s.year = s.year, e.year = e.year, lengthbased = FALSE, qadjusted = FALSE,
                  areas = areas.RV)
  
  
  # Format fishery independent data
  RVdataframe(path = path,  s.year = s.year, e.year = e.year,
              areas = areas.RV, lengthbased = TRUE, qadjusted = TRUE, csv = csv, rdata = rdata)
  RVdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, lengthbased = TRUE, qadjusted = FALSE, csv = csv, rdata = rdata)
  RVdataframe(path = path,  s.year = s.year, e.year = e.year,
              areas = areas.RV, lengthbased = FALSE, qadjusted = TRUE, csv = csv, rdata = rdata)
  RVdataframe(path = path,  s.year = s.year, e.year = e.year,
              areas = areas.RV, lengthbased = FALSE, qadjusted = FALSE, csv = csv, rdata = rdata)

  # Extract and format length-weight data
  LWdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, update_LW = TRUE, csv = csv, rdata = rdata)
  
  # Extract and format landings data
  LANDdataframe(path = path, areas = areas.land, update_LAND = TRUE, csv = csv, rdata = rdata)
  
  
}