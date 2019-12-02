#'@title Allocates 4X length-based herring index into sets and length classes
#'  based on the RV survey.
#'@description This function is called by \code{biomassData} to allocate the 4X
#'  herring index from XXXX into sets and length classes based on the proportion
#'  from the resaerch vessel survey.
#'
#'  **I think there are two mistakes in here: 1. The zero sets are not used to
#'  calculate the strata means. 2. The strata weights are different than in
#'  stratifyBiomass() for before 1982
#'@details User must define \code{channel = odbcConnect("ptran", uid = ###, pwd
#'  = ###)} in the global environment. This channel must have access to the XXXX
#'  databases.
#'
#'**Add description of stratweights here
#'
#'  The 4X length-based herring index is based on XXXX. This data must be stored
#'  in path/extra info/**name.csv.
#'
#'  The /code{ABUNDNACE} in **name.csv is allocated across the research vessel
#'  summer strata and length classes observed in the survey based on a
#'  stratification/destratification scheme:
#'
#'  1. Stratify research vessel survey \code{ABUNDANCE} and determine the total
#'  \code{ABUNDANCE} over the whole area.
#'
#'  2. Calculate the proportion of the total \code{ABUNDANCE} that is
#'  contributed by each strata.
#'
#'  3.  Calculate the proportion of the mean \code{ABUNDANCE} in each strata
#'  that is contributed by each set.
#'
#'  4. Use the proportions in 2. and 3. to destratify the 4X herring index
#'  \code{ABUNDACE}.
#'
#'  5. Use the mean weight of fish to calculate the \code{BIOMASS} in each set:
#'  \deqn{\code{BIOMASS} = \code{ABUNDANCE} * Weight_{Avg}}.
#'
#'  (Notes:
#'
#'  1. AC's code did not account for the different tow units after the gear
#'  change when de-stratifying the index, but DID when re-stratifying the
#'  biomass. The change in tow units is now accounted for in both procedures.
#'
#'  2. AC's code did not account for the zero sets when calculating the mean
#'  abundance in each strata. This is now accounted for.)
#'
#'@inheritParams biomassData
#'@references Modified code from AC's ExtractIndicators/R/VPAherring.R
#'@importFrom RODBC sqlQuery
#'@export

herringAtLength <- function(path, s.year, e.year) {
  
  # Read in herring index
  her = read.csv(file.path(path,'extra info','4xherringbiomassDec2015.csv'))     # Aggregated abundance
  her$BIOMASS<-her$BIOMASS*1000; her$ABUNDANCE<-her$ABUNDANCE*1000               # Convert units to kg
 
  her.l = read.csv(file.path(path,'extra info','4xherringatlengthDec2015.csv'))  # Abundance per length class
  her.l$BIOMASS<-her.l$BIOMASS*1000;	her.l$ABUNDANCE<-her.l$ABUNDANCE*1000      # Convert units to kg
  
  # Length-based research vessel survey observations of herring in 4X
  dat <- sqlQuery(channel, paste("select i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year,flen, c.spec,clen*nvl(decode(totwgt,0,1,totwgt),1)/nvl(decode(sampwgt,0,1,sampwgt),1)*1.75/dist Abundance from 
						groundfish.gsinf i, groundfish.gscat c, groundfish.gsdet d where i.mission=c.mission and i.setno=c.setno and i.mission=d.mission and i.setno=d.setno and c.spec=d.spec
						and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and  strat between '470' and '495' and type=1 and c.spec=60",sep=""))
  #not using the herring at length as age length keys cover more sizes than are observed in the survey
  
  # Zero sets: Research vessel sets from 4X where no herring were observed (needed to calculate mean abundance in each strata)
  dat.full <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, 60 spec, 0 Abundance from 
						groundfish.gsinf i where to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and
				strat between '470' and '495' and type=1 ;",sep=""))

  dat.full$ID <- 1:nrow(dat.full)
  
  # Observed length classes for each year
  FL <- unique(dat[,c('FLEN','YEAR')])
  FL <- FL[order(FL$YEAR,FL$FLEN),]
  y <- unique(FL$YEAR)
  
  # Full dataset: add zero sets for each length class 
  ou <- NULL
  for(i in 1:length(y)) {                      # loop over years
    d <- dat.full[dat.full$YEAR == y[i], ]     # subset zeo sets to current year
    e <- FL[FL$YEAR == y[i], 'FLEN']           # length classes observed in current year  
    l <- length(e)                             # number of length classes observed in the current year
    
    # Repeat row of d (zero sets) for each length class and bind together
    d.f <- d[rep(seq_len(nrow(d)), each = l),] 
    d.f$FLEN <- e
    ou <- rbind(ou, d.f)
  }	
  
  aa <- merge(dat, ou, all = T)                                                   # merge the observations and zero sets
  dat <- aggregate(ABUNDANCE ~ YEAR + STRAT + FLEN + MISSION + SETNO,             # Aggregate abundance 
                   data = aa, FUN = sum)                                          # Changed to data = aa (from data = dat in AC's ExtractIndicators code)
  
  dat.agg <- aggregate(ABUNDANCE ~ YEAR + STRAT + FLEN, data = dat, FUN = 'mean') # Calculate mean abundance within each strata
  
  # Import strata weights and merge with dat ***these are different from stratifyBiomass()
 # wts <- sqlQuery(channel,paste("select strat,area, area/((41./6080.2)*1.75) tunits from groundfish.gsstratum where strat between '470' and '495';"))
  #dat.agg1 <- merge(dat.agg,wts,by='STRAT')
  
  # Import strata weights and merge with dataframe of mean ABUNDANCE and BIOMASS estimates
  st.weights  <- read.csv(file.path(path, "extra info", "stratweights.csv"))  # strata weights
  dat.agg1 <- merge(dat.agg, st.weights, by = 'STRAT')
  
  # Add tow units (different gear after 1981, so conversion from AREA to TUNITS depends on the year)
  dat.agg1$TUNITS <- ifelse(dat.agg1$YEAR <= 1981, dat.agg1$AREA/((35./6080.2)*1.75),
                            dat.agg1$AREA/((41./6080.2)*1.75))
  
  # Scale abundance in each strata by corresponding strata weight
  dat.agg1$AEST <- dat.agg1$ABUNDANCE*dat.agg1$TUNITS           
  
  # Calculate abundance over whole area
  dat.agg2 <- aggregate(AEST ~ YEAR + FLEN, data = dat.agg1, FUN='sum') 
  
  # Calculate proportion of stratified abundance within each strata
  dat.agg3 <- merge(dat.agg1, dat.agg2, by = c('YEAR','FLEN'))
  dat.agg3$PROP.A <- dat.agg3$AEST.x/dat.agg3$AEST.y            
  
  # Merge length-based herring index with observed lengths from survey
  be <- merge(her.l, FL, by = c('YEAR','FLEN'), all.y = T)      
  
  # Calculate total biomass and abundance for each year (aggragated over length)
  be <- merge(be, aggregate(cbind(ABUNDANCE, BIOMASS) ~ YEAR,   
                            data = be, FUN = sum), by = 'YEAR')
  
  # Calculate proportion of biomass and abundance at each length
  be$pBiom <- be$BIOMASS.x/be$BIOMASS.y
  be$pAbund <- be$ABUNDANCE.x/be$ABUNDANCE.y
  
  # Allocate biomass and abundance to each length
  be <- merge(be[,c('YEAR','FLEN','pBiom','pAbund')], her, by = c('YEAR'))
  be$ABUNDANCE <- be$ABUNDANCE*be$pAbund
  be$BIOMASS <- be$BIOMASS*be$pBiom
  
  her <- be[,c('YEAR','FLEN','ABUNDANCE','BIOMASS')]
  
  # Calculate the mean weight of fish
  her$WT <- her$BIOMASS/her$ABUNDANCE
  
  # Allocate the overall abundance into strata and divide by tow units to get mean per tow
  dat4 <- merge(dat.agg3, her, by = c('YEAR','FLEN'))
  dat4$A <- dat4$PROP.A * dat4$ABUNDANCE.y/dat4$TUNITS # Mean per tow
  dat4 <- dat4[, c('YEAR','STRAT','FLEN','A')] 
  
  # Calculate the proportion of ABUNDANCE and BIOMASS in each strata that is from each set
  dat.agg9 <- aggregate(ABUNDANCE ~ YEAR + STRAT + FLEN, data = dat, FUN = 'sum')   
  dat5 <- merge(dat, dat.agg9, by = c('YEAR','STRAT','FLEN'))                       
  # ABUNDANCE.x is the abundance in each set; ABUNDANCE.y is total abundaunce in each strata
  
  # Number of sets for each strata each year
  dat6 <- aggregate(SETNO ~ YEAR + STRAT + FLEN, data = dat5, FUN = length) 
  names(dat6)[ncol(dat6)] <-'NSETS'
  dat7 <- merge(dat5, dat6, by = c('YEAR','STRAT','FLEN'))
  # Proportion of mean strata ABUNDANCE that is from each set 
  dat7$Abyset <- with(dat7,(ABUNDANCE.x)/(ABUNDANCE.y)) 
  
  # Allocate the overall ABUNDANCE recorded in "her" into sets based on the proportions from the survey
  dat8 <- merge(dat7, dat4, by = c('YEAR','STRAT','FLEN'))
  dat8$TOTNO <- with(dat8, A*NSETS*Abyset)           
  
  dat8 <- merge(dat8, her, by=c('YEAR','FLEN'))	
  dat8$TOTWGT <- with(dat8,TOTNO*WT)
  
  her.dat <- dat8[,c(1:5,11,15)]
  hh <- her.dat
  hh[is.na(hh)] <-0
  names(hh)[6:7] <- c('ABUNDANCE','BIOMASS')
  hh$QBIOMASS <- hh$BIOMASS <- hh$BIOMASS #in KG
  hh$QABUNDANCE<- hh$ABUNDANCE
  hh$SPECIES <- 60
  hh
}
