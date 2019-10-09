#'@title Formats commercial landings data for use in the \code{marindicators}
#'  package
#'@description This function imports data from
#'  path/data/landings.landings.RData, attaches labels for the spatial scales of
#'  interest ("shelf", "esswss", and "nafo") in column \code{ID}, and replaces
#'  commercial species codes with the research vessel species codes.
#'@details Area ID's are assiged from the file path//Extra
#'  Info/landingsgroupings.csv.
#'
#'  Commercial and research vessel species codes are matched from the file
#'  path/Extra Info/SpeciesCodes.csv. the function accounts for the proportion
#'  of landings of species with more than one RV code but one commercial code.
#'@param path The filepath to the data folder created by
#'  \code{extractLandings()}.
#'@param areas Areas of interest. A separate landings dataframe will be exported
#'  for each area. Default is \code{areas = c("shelf", "esswss", "nafo")}
#'@param csv Logical value indicating whether to export landings dataframe as a
#'  .csv file. Default is \code{csv = TRUE}.
#'@param rdata Logical value indicating whether to export landings dataframe as
#'  a .RData file. Default is \code{rdata = TRUE}.
#'@return This function creates a directory output/Landings and stores the
#'  landings dataframe in area_land.RData (object name is \code{land}) and/or
#'  area_land.csv. The dataframe has 4 columns: \code{ID} (the area ID),
#'  \code{YEAR}, \code{SPECIES} (the research vessel species code) and
#'  \code{CATCH}.
#'@references Original code by DD.
#'@importFrom utils write.csv
#'@importFrom utils read.csv
#'@export


LANDdataframe <- function(path, areas = c("shelf", "esswss", "nafo"), csv = TRUE, rdata = TRUE){
  
  # Import data
  load(paste(path, "/data/landings/landings.RData", sep=""))    # load landings data 
  names(landings)[1] <- "ALLCODES"                    # change column name to "ALLCODES"
  
  grp <- read.csv(paste(path,"/Extra Info/landingsgroupings.csv", sep = ""))  # import table to match NAFO_UNIT (in landings) to area ID codes

  prop.land.table <- read.csv(paste(path, "/Extra Info/SpeciesCodes.csv", sep = ""),
                              header = TRUE, sep = ",")
  # import table to match ALLCODES with SPECIES and account for the proportion of landings of each SPECIES
  
  for (j in 1:length(areas)){
    
    data.j <- landings                                  
    areas.j = areas[j]
    
    wl <- grp[, c(1, which(toupper(areas.j) == names(grp)))] # extracts columns NAFOSUB and areas.J from grp
    names(wl) <- c('NAFO_UNIT', 'ID')                                      # name columns 
    data.j <- merge(data.j, wl, by = 'NAFO_UNIT')                          # merge landings dataframe with the area data
    
    data.j <- merge(data.j, prop.land.table, by = "ALLCODES")              # merge landings dataframe with species codes data
    data.j$CATCH <- data.j$CATCH * data.j$PROPORTION_OF_LANDINGS           # account for the proportion of landings of ALLCODES of each SPECIES
    land <- data.j[, c("ID", "YEAR", "SPECIES", "CATCH")]                  # create dataframe with the columns of interest
    
    dir.create(paste(path, "/output/Landings", sep = ""), recursive = T, showWarnings = F)
    path.output <- paste(path, "/output/Landings/", areas.j, "_land", sep = "")
    
    # save data as an Excel .csv file
    if(csv) write.csv(land, file = paste(path.output, ".csv",sep=""), row.names = FALSE)
    
    if(rdata) save(land, file = paste(path.output, ".RData", sep=""))
    
    print("landings dataframe exported")

  }
}