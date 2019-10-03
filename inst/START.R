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
# this might change when package is built, i.e., can point to somewhere NOT the package directory
setwd("C:/Users/DEMPSEYD/Desktop/SSdata/")
path <- file.path("C:/Users/DEMPSEYD/Desktop/SSdata") 

# Set username and password for accessing databases
channel <- odbcConnect("ptran", uid="GOMEZC", pwd="Branch22")

source(paste(path,'/R/loadfun.R',sep=""))
loadfun(path) # sources all functions in path/R (don't think I need this once the package is working)


# Step 2: Enter years and areas for which to extract data -----------------
start.year <- 2015
end.year <- 2015
areas.RV <- c('strat','nafo','shelf','esswss')
areas.land <- c('nafo','shelf','esswss')


# Step 3: Extract research vessel and landings data -------------------------------
# These data are stored in path/data and then called in Step 4

# Biomass & Abundance data (q-adjusted post-strat)
#extractBiomass(path = path, s.year = start.year, e.year = end.year, areas = areas.RV)

# Landings data 
#extractLandings(path) 


# Step 4: Format Data for marindicators -----------------------------------
compileDataframes(path = path, s.year = start.year, e.year = end.year, 
                 areas.RV = areas.RV, areas.land = areas.land, csv = FALSE, Rdata = TRUE)
  






