#channel <- odbcConnect('bank.canso3','mfd_stomach','bx547mj9')
#channel
#if reading from Catalina's computer:
source('C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/Catalina/indicatorSpaceTime.R')
source('C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/Catalina/amc helpers.R')

#source('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/scripts to extract indicators/indicatorSpaceTime.R')
#source('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/scripts to extract indicators/amc helpers.R')

#source('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/scripts to extract indicators/extract_indi.R')
#utils:::menuInstallPkgs()
#head(A)

################################################################
################################################################
################################################################
#Corrected for catchability

A <- biodiversityData(s.year=1970,e.year=2012,q.corr=T,add.zero.sets=T)  
#setwd("C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/ess.wss/q")
setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina")

#Attribute: Resource Potential 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('SKATES','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('CLUPEIDS','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('FINFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('FLATFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('GADOIDS','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('GROUNDFISH','BIOMASS'),stratified=T,means=F,totals=T)

#there is no q for invertebrates January 08, 2013 08:39:25 AM 
indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('INVERTEBRATES','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('FORAGE','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('LBENTHIVORE','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('MBENTHIVORE','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('PISCIVORE','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('PLANKTIVORE','BIOMASS'),stratified=T,means=F,totals=T)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('ZOOPISCIVORE','BIOMASS'),stratified=T,means=F,totals=T)

A <- biodiversityData(s.year=1970,e.year=2012,q.corr=T,add.zero.sets=T)  
#setwd("C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/ess.wss/q")
setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/q")

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='biomassRatioInv2Dem',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='biomassRatioPel2Dem',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

#Not indicator 
indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='resourcePotential',args=c('ALL','ABUNDANCE'),stratified=T,means=F,totals=T)

###Attribute: Stability and Resistance to Perturbations (q corrected)

###Fishing pressure [biomass/landings)] = invCVBiomass
indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='invCVBiomass',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)

###IFishing pressure [biomass/landings)] = invCVBiomass (ONLY FOR FINFISH)
#indi <- indicatorSpaceTime(dat=A,groups='ess.wss',indicator='invCVBiomass',args=c('FINFISH','BIOMASS'),stratified=T,means=F,totals=T)

##rm(A)
##gc(reset=T)
##stop()

# ################################################################
# ################################################################
# ################################################################
# #NO Corrected for catchability
# 
# B <- biodiversityData(s.year=1970,e.year=2012,q.corr=F,add.zero.sets=T)  
# setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/noq")
# #setwd("C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/ess.wss/noq")
# 
# ##Attribute: Resource Potential (no q corrected)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('SKATES','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('CLUPEIDS','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('FINFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('FLATFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('GADOIDS','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('GROUNDFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('INVERTEBRATES','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('FORAGE','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('ALL','ABUNDANCE'),stratified=T,means=F,totals=T)
# 
# ##Attribute: Stability and Resistance to Perturbations (no q corrected)
# 
# ##Fishing pressure [biomass/landings)] = invCVBiomass
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='invCVBiomass',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)
# 
# ## Fishing pressure [biomass/landings)] = invCVBiomass (ONLY FOR FINFISH)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='invCVBiomass',args=c('FINFISH','BIOMASS'),stratified=T,means=F,totals=T)
# 
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('LBENTHIVORE','BIOMASS'),stratified=T,means=F,totals=T)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('MBENTHIVORE','BIOMASS'),stratified=T,means=F,totals=T)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('PISCIVORE','BIOMASS'),stratified=T,means=F,totals=T)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('PLANKTIVORE','BIOMASS'),stratified=T,means=F,totals=T)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='resourcePotential',args=c('ZOOPISCIVORE','BIOMASS'),stratified=T,means=F,totals=T)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='biomassRatioInv2Dem',args=c('ZOOPISCIVORE','BIOMASS'),stratified=T,means=T,totals=F)
# indi <- indicatorSpaceTime(dat=B,groups='ess.wss',indicator='biomassRatioPel2Dem',args=c('ZOOPISCIVORE','BIOMASS'),stratified=T,means=T,totals=F)

#    stop()

################################################################
################################################################
################################################################
##All other indicators different from resource potential
##Corrected for catchability

C <- biodiversityData(s.year=1970,e.year=2012,q.corr=T,add.zero.sets=F)  
setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/q")
#setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/richness without invertebrates")
##setwd("C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/ess.wss/q")

###source("C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/indicatorSpaceTime.R")

####Attribute: Biodiversity (q corrected)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='kemptonQ',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='hillN1',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='shannon',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='hillN2',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='pielouSpeciesEvenness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='esswss',indicator='margalefSpeciesRichness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='speciesRichness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='margalefSpeciesRichness',args=c('GROUNDFISH','BIOMASS'),stratified=F,means=T,totals=F)

##Attribute: Stability and Resistance to Perturbations (q corrected)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanMaxL',args=c('ALL','ABUNDANCE'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanMaxL',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F,include.herring=T)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanMaxAge',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='biomassPerTL',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)

#Attribute: Structure and Functioning (Q CORRECTED)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('LBENTHIVORE','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('MBENTHIVORE','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('PISCIVORE','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('PLANKTIVORE','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='communityCondition',args=c('ZOOPISCIVORE','BIOMASS'),stratified=F,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='largeSpeciesIndicator',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='predatoryFish',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanTrophicLevelCommunity',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/q")
indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanLengthCommunity',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='meanLengthCommunity',args=c('ALL','ABUNDANCE'),stratified=T,means=T,totals=F)

indi <- indicatorSpaceTime(dat=C,groups='ess.wss',indicator='largeFishIndicator',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)

##rm(C)
##stop()


################################################################
################################################################
################################################################
##All other indicators different from resource potential
##No corrected for catchability

# D <- biodiversityData(s.year=1970,e.year=2012,q.corr=F,add.zero.sets=F) 
# setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina/ess.wss/noq")
# #source("C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/indicatorSpaceTime.R")
# setwd("C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/ess.wss/noq")
# 
# ##Attribute: Biodiversity (no q corrected)
# 
# indi.5 <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='kemptonQ',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='hillN1',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='shannon',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='hillN2',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='pielouSpeciesEvenness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='margalefSpeciesRichness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='speciesRichness',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='margalefSpeciesRichness',args=c('GROUNDFISH','BIOMASS'),stratified=F,means=T,totals=F,saveIndicatorData=T)
# 
# ##Attribute: Stability and Resistance to Perturbations (no q corrected)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='meanMaxL',args=c('ALL','ABUNDANCE'),stratified=T,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='meanMaxL',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='meanMaxL',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F,include.herring=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='meanMaxAge',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='biomassPerTL',args=c('ALL','BIOMASS'),stratified=T,means=F,totals=T)
# 
# ##Attribute: Structure and Functioning (NO Q CORRECTED)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='communityCondition',args=c('ALL','BIOMASS'),stratified=F,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='largeSpeciesIndicator',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='predatoryFish',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)
# 
# indi <- indicatorSpaceTime(dat=D,groups='ess.wss',indicator='meanTrophicLevelCommunity',args=c('ALL','BIOMASS'),stratified=T,means=T,totals=F)
# 
# 
