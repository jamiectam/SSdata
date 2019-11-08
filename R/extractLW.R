#'@title Extracts annual length-weight data
#'@description This function extracts annual length-weight data from fishery
#'  independent surveys on the Scotian Shelf.
#'@inheritParams biomassData
#'@param path Filepath indicating where to create folders to store the extracted
#'  data.
#'@return This function creates a directory to store extracted data:
#'
#' path/data/lenwgt stores fish weight at length. 
#'@references Modified code from AC's ExtractIndicators/R/biomassData.R
#'@export
#'@importFrom RODBC sqlQuery


extractLW <- function(path, s.year, e.year) {
  
  # Create file path to store data
  fna <- file.path(path, "data", "lenwgt")  
  dir.create(fna, recursive = T, showWarnings = F)
  
  yr <- s.year:e.year
  
  for(i in 1:length(yr)){
    
    # Extract data from SQL
    wt <- sqlQuery(channel,paste("select distinct strat,spec species,flen,fwt from groundfish.gsinf i, groundfish.gsdet d where i.mission=d.mission and i.setno=d.setno and to_char(sdate,'yyyy') = ",yr[i]," and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and fwt is not null and flen is not null;",sep=""))
    
    # Save data
    save(wt, file = paste(fna, "/lw", yr[i], ".Rdata",sep=""), compress = T)
  }
  
}



