#'@title Sources all functions in the path.
#'@description I don't think I will need this when the package is built. This
#'  function sources all of the functions in SSdata/R, making them available for
#'  use by the package.
#'@param path The file path to the package
#'@references Modeified code from AC's ExtractIndicators/R/amc helpers.R
#'@export


loadfun <- function(path) {
  
  a <- dir(file.path(path,'R'))        # vector of all the files in SSdata/R
  
  for(i in 1:length(a)) {              # source all of the files in SSdata/R
    source(file.path(path,'R',a[i]))  
  }
  
}