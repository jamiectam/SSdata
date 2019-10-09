#'@title Formats fishery independey survey data for use in the
#'  \code{marindicators} package
#'@description This function imports data for each area from
#'  path/data/stratified/lengthbased or path/data/stratified/notlengthbased,
#'  adds column \code{YEAR}, and binds all years together.
#'@inheritParams biomassData
#'@param path The filepath to the data folder created by
#'  \code{extractBiomass()}.
#'@param areas Areas of interest. A separate dataframe will be exported for each
#'  area. Default is \code{areas = c("shelf", "esswss", "nafo", "strat")}.
#'@param lengthBased Logical parameter indicating whether to format length-based
#'  \code{lengthBased = TRUE} or not length-based data  \code{lengthBased =
#'  FALSE}.
#'@param csv Logical value indicating whether to export dataframe as a .csv
#'  file. Default is \code{csv = TRUE}.
#'@param rdata Logical value indicating whether to export dataframe as a .RData
#'  file. Default is \code{rdata = TRUE}.
#'@return This function creates a directory to save the exported data.
#'
#'  If \code{lengthBased} is \code{TRUE}: path/output/RV/lengthBased and stores
#'  the dataframe in area_lengthBased.RData (object name is \code{RVdata})
#'  and/or area_RVdata.csv. The dataframe has 6 columns: \code{ID} (the area
#'  ID), \code{YEAR}, \code{SPECIES} (the research vessel species code),
#'  \code{LENGTH}, \code{BIOMASS}, and \code{ABUNDANCE}.
#'
#'  OR
#'
#'  If \code{lengthBased} is \code{FALSE}: path/output/RV/notlengthBased and
#'  stores the dataframe in area_notlengthBased.RData (object name is
#'  \code{RVdata}) and/or area_RVdata.csv. The dataframe has 5 columns:
#'  \code{ID} (the area ID), \code{YEAR}, \code{SPECIES} (the research vessel
#'  species code), \code{BIOMASS}, and \code{ABUNDANCE}.
#'
#'@references Original code by DD.
#'@importFrom utils write.csv
#'@export

RVdataframe <- function(path, s.year, e.year, areas = c("shelf", "esswss", "nafo", "strat"), 
                        lengthBased, csv = TRUE, rdata = TRUE){
  
  years <- c(s.year:e.year)
  
  for (j in 1:length(areas)){
    
    areas.j = areas[j]
    allData <- NULL
    
    for (i in 1:length(years)){
      
      if(lengthBased) path.input <- paste(path, "/data/stratified/lengthBased/", areas.j,"/", sep = "")
      if(!lengthBased) path.input <- paste(path, "/data/stratified/notlengthBased/", areas.j,"/", sep = "")
      
      load(paste(path.input, years[i],".RData",sep=""))		
      data.i = out
      rm(out)
      
      year.i = years[i]
      YEAR.i = rep(year.i, nrow(data.i))
      
      data.i = cbind(YEAR.i, data.i)
      
      if(lengthBased) names(data.i) = c("YEAR", "SPECIES", "ID", "LENGTH", "BIOMASS", "ABUNDANCE")
      if(!lengthBased) names(data.i) = c("YEAR", "SPECIES", "ID", "BIOMASS", "ABUNDANCE")
      
      allData <- rbind(allData, data.i)
      
    }
    
    if(lengthBased) {
      dir.create(paste(path, "/output/RV/lengthBased", sep = ""), recursive = T, showWarnings = F)
      path.output <- paste(path, "/output/RV/lengthBased/", areas.j, "_lengthBased", sep = "")
    }
    if(!lengthBased) {
      dir.create(paste(path, "/output/RV/notlengthBased", sep = ""), recursive = T, showWarnings = F)
      path.output <- paste(path, "/output/RV/notlengthBased/", areas.j, "_notlengthBased", sep = "")
    }
    
    RVdata <- allData
    if(csv) write.csv(RVdata, file = paste(path.output, ".csv",sep=""), row.names = FALSE)
    # save data as an R .RData file as well
    if(rdata) save(RVdata, file = paste(path.output, ".RData", sep=""))
  
  }
  
}