
LANDdataframe <- function(path, areas, csv = TRUE, Rdata = TRUE){
  
  # Import data
  load(paste(path, "/data/landings/landings.RData", sep=""))    # load landings data 
  names(land)[1] <- "ALLCODES"                    # change column name to "ALLCODES"
  
  grp <- read.csv(paste(path,"/Extra Info/landingsgroupings.csv", sep = ""))  # import table to match NAFO_UNIT (in land) to area ID codes

  prop.land.table <- read.csv(paste(path, "/Extra Info/SpeciesCodes.csv", sep = ""),
                              header = TRUE, sep = ",")
  # import table to match ALLCODES with SPECIES and account for the proportion of landings of each SPECIES
  
  for (j in 1:length(areas)){
    
    data.j <- land                                  
    areas.j = areas[j]
    
    wl <- grp[, c(1, which(toupper(areas.j) == names(grp)))] # extracts columns NAFOSUB and areas.J from grp
    names(wl) <- c('NAFO_UNIT', 'ID')                                # name columns 
    data.j <- merge(data.j, wl, by = 'NAFO_UNIT')                          # merge landings dataframe with the area data
    
    data.j <- merge(data.j, prop.land.table, by = "ALLCODES")              # merge landings dataframe with species codes data
    data.j$CATCH <- data.j$CATCH * data.j$PROPORTION_OF_LANDINGS              # account for the proportion of landings of ALLCODES of each SPECIES
    landings <- data.j[, c("ID", "YEAR", "SPECIES", "CATCH")]                # create dataframe with the columns of interest
    
    dir.create(paste(path, "/output/Landings", sep = ""), recursive = T, showWarnings = F)
    path.output <- paste(path, "/output/Landings/", areas.j, "_landings", sep = "")
    
    
    # save data as an Excel .csv file
    if(csv) write.csv(landings, file = paste(path.output, ".csv",sep=""), row.names = FALSE)
    
    if(Rdata) save(landings, file = paste(path.output, ".RData", sep=""))

  }
}