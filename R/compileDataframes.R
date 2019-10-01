

compileDataframes <- function(path, s.year, e.year, areas.RV, areas.land, csv = F, Rdata = TRUE){
  
  
  RVdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, lengthBased = FALSE, csv = csv, Rdata = Rdata)
  
  RVdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, lengthBased = TRUE, csv = csv, Rdata = Rdata)
  
  LWdataframe(path = path,  s.year = s.year, e.year = e.year, 
              areas = areas.RV, csv = csv, Rdata = Rdata)
  
  
  # Compile landings data and use RV species code!
  LANDdataframe(path = path, areas = areas.land, csv = csv, Rdata = Rdata)
  
}