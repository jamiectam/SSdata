# October 1, 2019

# Step 1: Set Up ----------------------------------------------------------
# Load required packages (RODBC and reshape)
# If you see a Warning message that says that package is not installed:
## Go to Tools --> Install Packages. Choose "Install from: Respository (CRAN)" 
## and type the name of the package you need to install
require(RODBC)
require(reshape)
options(stringsAsFactors=F)

# Set working directory and path
setwd("C:/Users/DEMPSEYD/Desktop/SS_data/")
path <- file.path("C:/Users/DEMPSEYD/Desktop/SS_data") 

# Set username and password for accessing databases
channel <- odbcConnect("ptran", uid="GOMEZC", pwd="Branch22")

source(paste(path,'/R/amc helpers.R',sep=""))
loadfun(path) # sources all functions in path/R


# Step 2: Enter years and areas for which to extract data -----------------
start.year <- 2015
end.year <- 2015
areas.RV <- c('strat','nafo','shelf','esswss')
areas.land <- c('nafo','shelf','esswss')


# Step 3: Extract research vessel and landings data -------------------------------
# These data are stored in path/data and then called in Step 4

# Biomass & Abundance data (q-adjusted post-strat)
extractBiomass(path = path, s.year = start.year, e.year = end.year, areas= areas.RV)

# Landings data 
extractLandings(path) 


# Step 4: Format Data for marindicators -----------------------------------
compileDataframes(path = path, s.year = start.year, e.year = end.year, 
                 areas.RV = areas.RV, areas.land = areas.land, csv = FALSE, Rdata = TRUE)
  







