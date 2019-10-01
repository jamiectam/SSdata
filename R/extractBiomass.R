
extractBiomass <- function(path, s.year, e.year, areas){
  
  # Extract biomass data
  biomass.redo = T
  if(biomass.redo == TRUE) biomassData(path = path, s.year = start.year, e.year = end.year)
  
  # Stratify biomass data
  stratifyBiomass(path = path, s.year = start.year, e.year = end.year, 
                  qadjPostStrat = T, lengthBased = F, areas = areas)
  stratifyBiomass(path = path, s.year = start.year, e.year = end.year, 
                  qadjPostStrat = T, lengthBased = T, areas = areas)
}