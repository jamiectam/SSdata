#'@title Allocates 4X herring index into sets based on the RV survey.
#'@description This function is called by \code{biomassData} to allocate the 4X
#'  herring index into sets based on proportions from the research vessel
#'  survey.
#'
#'  q-correction is not applied because the index estimates total biomass.
#'
#'@details User must define \code{channel = odbcConnect("ptran", uid = ###, pwd
#'  = ###)} in the global environment. This channel must have access to the XXXX
#'  databases.
#'
#'  This function requires file path/extra info/stratweights/csv.
#'  stratweights.csv has two columns: \code{STRAT} and the corresponding
#'  \code{AREA}. Each \code{AREA} is converted to tow units using the conversion
#'  \code{TOW UNITS = AREA/((35./6080.2)*1.75)} until 1981 and \code{TOW UNITS =
#'  AREA/((41./6080.2)*1.75)} after 1981
#'
#'  The 4X herring index is based on XXXX. This data must be stored in
#'  path/extra info/**name.csv.
#'
#'  The \code{ABUNDANCE} in **name.csv is allocated across the research vessel
#'  summer strata based on a stratification/destratification scheme:
#'
#'  1. Stratify research vessel survey \code{ABUNDANCE} and determine the total
#'  \code{ABUNDANCE} over the whole area.
#'
#'  2. Calculate the proportion of the total \code{ABUNDANCE} in each strata.
#'
#'  3.  Calculate the proportion of the mean \code{ABUNDANCE} in each strata
#'  that is contributed by each set.
#'
#'  4. Use the proportions in 2. and 3. to destratify the 4X herring index
#'  \code{ABUNDANCE}.
#'
#'  5. Use the mean weight of fish to calculate the \code{BIOMASS} in each set:
#'  \deqn{\code{BIOMASS} = \code{ABUNDANCE} * Weight_{Avg}}
#'
#'  The re-stratified biomass should equal the biomass index.
#'
#'  (Note: AC's code did not account for the different tow units after the gear
#'  change when de-stratifying the index, but DID when re-stratifying the
#'  biomass. The change in tow units is now accounted for in both procedures.)
#'
#'@inheritParams biomassData
#'@references Modified code from AC's ExtractIndicators/R/VPAherring.R
#'@importFrom RODBC sqlQuery
#'@export


herringAggregate <- function(path, s.year, e.year) {
  #her <- sqlQuery(channel,paste('select year,area,spec,abundance*1000 abundance,biomass*1000 biomass,wt from indiseas_4X_herring'))
  
  # Read in herring biomass index
  her = read.csv(paste(path, "/extra info/4XherringbiomassDec2015.csv", sep = ""))
  her$ABUNDANCE = her$ABUNDANCE*1000
  her$BIOMASS = her$BIOMASS*1000
  
  if(!exists("her")) {
    print(paste("4X herring index not found in", path, "/extra info/**name.csv", sep = ""))
  }
  
  # Research vessel survey observations of herring in 4X
  dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, spec,totno*1.75/i.dist Abundance,totwgt*1.75/i.dist biomass from 
					groundfish.gsinf i, groundfish.gscat c where i.mission=c.mission and i.setno=c.setno and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and
					strat between '470' and '495' and type=1 and spec=60",sep=""))
  
  # Research vessel sets from 4X where no herring were observed (needed to calculate mean abundance in each strata)
  dat.full <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, 60 spec, 0 Abundance,0 biomass from 
					groundfish.gsinf i where to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and
					strat between '470' and '495' and type=1 ;",sep=""))
  
  # Merge the observations and zero sets
  aa <- merge(dat, dat.full, all = TRUE)
  dat <- aa[!duplicated(aa[,1:6], fromLast = T),]
  
  # Calculate mean ABUNDANCE and BIOMASS in each strata
  dat.agg <- as.data.frame(aggregate(dat[,c('ABUNDANCE', 'BIOMASS')],
                                     by = dat[c('YEAR', 'STRAT')], FUN = 'mean'))
  
  # Import strata weights and merge with dataframe of mean ABUNDANCE and BIOMASS estimates
  st.weights  <- read.csv(file.path(path, "extra info", "stratweights.csv"))  # strata weights
  dat.agg1 <- merge(dat.agg, st.weights, by = 'STRAT')
  
  # Add tow units (different gear after 1981, so conversion from AREA to TUNITS depends on the year)
  dat.agg1$TUNITS <- ifelse(dat.agg1$YEAR <= 1981, dat.agg1$AREA/((35./6080.2)*1.75),
                            dat.agg1$AREA/((41./6080.2)*1.75))
 
  # Scale the mean ABUNDANCE and BIOMASS based on strata weights
  dat.agg1$AEST <- dat.agg1[,'ABUNDANCE']*dat.agg1[,'TUNITS']  # AEST: Estimated abundance in each strata
  dat.agg1$BEST <- dat.agg1[,'BIOMASS']*dat.agg1[,'TUNITS']    # BEST: Estimated biomass in each strata
  
  # Estimate ABUNDANCE and BIOMASS for the whole area (sum over all strata)
  dat.agg2 <- as.data.frame(aggregate(dat.agg1[,c('AEST','BEST')],
                                      by = (dat.agg1['YEAR']), FUN='sum'))
  names(dat.agg2)[2] <-'AGGAEST'  # AGGAEST: total abundance over the whole area
  names(dat.agg2)[3] <-'AGGBEST'  # AGGBEST: total biomass over the whole area
  
  # Calculate the proportion of total stratified ABUNDANCE and BIOMASS from each strata
  dat.agg3 <- merge(dat.agg1, dat.agg2, by = 'YEAR') # Create dataframe with totals for each strata and whole area
  dat.agg3$PROP.A <- dat.agg3$AEST/dat.agg3$AGGAEST  # PROP.A: Proportion of total ABUNDANCE from each strata
  dat.agg3$PROP.B <- dat.agg3$BEST/dat.agg3$AGGBEST  # PROP.B: Proportion of total BIOMASS from each strata
  
  # Allocate the overall ABUNDANCE and BIOMASS recorded in "her" into strata based on the proportions from the survey
  # Divide by strata weights to get the mean ABUNDANCE and BIOMASS per strata
  dat4 <- merge(dat.agg3, her, by = 'YEAR')
  dat4$A <- dat4$PROP.A * dat4$ABUNDANCE.y/dat4$TUNITS
  dat4$B <- dat4$PROP.B * dat4$BIOMASS.y/dat4$TUNITS
  dat4 <- dat4[,c('YEAR','STRAT','A','B')] # A: Mean ABUNDANCE per strata; B: mean biomass per strata
  
  # Calculate the proportion of ABUNDANCE and BIOMASS in each strata that is from each set
  dat5 <- merge(dat, dat.agg, by = c('YEAR','STRAT'))                      # ABUNDANCE and BIOMASS at the set level AND strata level
  dat6 <- aggregate(dat5$YEAR, by = dat5[c('YEAR','STRAT')], FUN = length) # Number of sets for each strata each year
  dat7 <- merge(dat5, dat6, by = c('YEAR','STRAT'))                         
  
  dat7$Abyset <- with(dat7,(ABUNDANCE.x)/(ABUNDANCE.y)) # Abyset: proportion of mean strata ABUNDANCE that is from each set 
  dat7$Bbyset <- with(dat7,(BIOMASS.x)/(BIOMASS.y))     # Bbyset: proportion of mean strata BIOMASS that is from each set 
  
  # Allocate the overall ABUNDANCE recorded in "her" into sets based on the proportions from the survey
  dat8 <- merge(dat7, dat4, by = c('YEAR','STRAT')) # dat4 is the ABUNDANCE and BIOMASS from her allocated to each strata
  dat8$TOTNO <- with(dat8, A*Abyset)                # Mean ABUNDANCE in each set
  # TOTNO is the mean ABUNDANCE per strata * proportion of mean ABUNDANCE in each set
  
  # Can allocation biomass similarly OR . . . 
  # dat8$TOTWGT <- with(dat8,B*Bbyset)
  # her.dat <- dat8[, c(3, 4, 5, 6, 1, 2, 7, 17, 18)]
  # names(her.dat)[8:9] <- c('TOTNO','TOTWGT')
  # hh <- her.dat
  # hh[is.na(hh)] <-0
  
  # . . . Calculate biomass based on the mean annual weight
  dat8 <- merge(dat8, her, by = 'YEAR')	
  dat8$TOTWGT <- with(dat8, TOTNO*WT)
  
  her.dat <- dat8[,c(3, 4, 5, 6, 1, 2, 7, 17, 23)]
  hh <- her.dat
  hh <- hh[, -which(names(hh) %in% c('YDDMMSS', 'XDDMMSS'))]
  hh[is.na(hh)] <- 0
  
  names(hh) <-c('MISSION','SETNO','YEAR','STRAT','SPEC','ABUNDANCE','BIOMASS')
  hh
  
}