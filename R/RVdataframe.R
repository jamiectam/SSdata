# adds YEAR col

RVdataframe <- function(path, s.year, e.year, areas, lengthBased, 
                     csv = TRUE, Rdata = TRUE){
  
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
    if(Rdata) save(RVdata, file = paste(path.output, ".RData", sep=""))
  
  }
  
}