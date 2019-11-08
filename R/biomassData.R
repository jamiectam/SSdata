#'@title Extracts fishery independent data
#'@description Extracts annual length-based and not length-based
#'  (i.e., aggregated over length) data for the Scotian Shelf. QABUNDANCE and
#'  QBIOMASS are q-adjusted at the set level (i.e., before stratification).
#'
#'  Herring biomass/abundance is treated separately from the other species.
#'@param path Filepath indicating where to create folders to store the extracted
#'  data.
#'@param s.strat Stratum for which to begin data extraction. Default is
#'  \code{s.strat = 440}.
#'@param e.strat Stratum for which to end data extraction. Default is
#'  \code{e.strat = 495}.
#'@param s.year Year for which to begin data extraction.
#'@param e.year Year for which to end data extraction.
#'@param vessel.correction Logical value indicating whether to apply vessel
#'  correction to BIOMASS and ABUNDANCE. Default is vessel.correction = TRUE.
#'@return This function creates directories to store extracted data. Within each
#'  folder, data for each year is saved in a separate RData file.
#'
#'  path/data/aggregate stores BIOMASS, ABUNDANCE, QBIOMASS and QABUNDANCE at
#'  the set level.
#'
#'  path/data/length stores BIOMASS, ABUNDANCE, QBIOMASS and QABUNDANCE at the
#'  set level for LENGTH in 1 cm increments. 
#'@references Modified code from AC's ExtractIndicators/R/biomassData.R
#'@importFrom stats aggregate
#'@importFrom reshape melt
#'@importFrom RODBC sqlQuery
#'@export

biomassData <- function(path, s.strat = 440, e.strat = 495, s.year, e.year,
                        vessel.correction = TRUE) {
  
  yr <- s.year:e.year
  
  #get 4X herring data
  her <- herringAtLength(path1=path)
  
  for(i in 1:length(yr)) {
    
    # At Length ---------------------------------------------------------------
    
    # Table with "SPECIES", "FUNGROUP", "Q", "LENCORR" (88 observations)
    catch_coefs <- sqlQuery(channel, paste('select * from gomezc.indiseas_catchability_coeffs'))
    catch_coefs[catch_coefs == 'NULL'] <- NA
    outputs <- list()
    m = 0
    
    # Extract table with 12 columns: 
    ## YEAR, STRAT, MISSION, YYDDMMSS, XDDMMSS, SETNO, FLEN, 
    ## ABUNDANCE, QABUNDANCE, QBIOMASS, BIOMASS, SPECIES
    # QABUNDANCE and QBIOMASS are q-adjusted at the set level (BEFORE stratification). 
    for (j in 1:nrow(catch_coefs)) {
      outputs[[j]] <- qBiomass(species = catch_coefs[j, 1], fun_group = catch_coefs[j, 2], 
                               q = catch_coefs[j, 3], len_corr = catch_coefs[j, 4], year = yr[i]) 
    }
    out <- as.data.frame(do.call(rbind, outputs))
    
    f <- out[out$SPEC==60 & out$STRAT >= 470 & out$STRAT <= 495,]  # extract herring observations
    out <- out[setdiff(rownames(out), rownames(f)),]               # discard herring observations from out
    mi <- unique(out$MISSION)				
    h <- her[her$MISSION %in% mi,]                                 # add in new herring observations
    out <- out[, -which(names(out) %in% c('YDDMMSS', 'XDDMMSS'))]
    out <- rbind(out, h)
    
    if(vessel.correction) out <- vesselCorr(out)                   # apply vessel correction
    
    # Export length based data (biomass and abundance at 1 cm intervals)
    fna <- paste(path,"/data/length/",sep="")
    dir.create(fna, recursive = T, showWarnings = F)
    fna <- paste(fna,"num_biom_at_length",yr[i],".RData",sep="")
    save(out, file = fna, compress = T)
    
    # End of At Length ------------------------------------------------------
    
    # Begin aggregate ---------------------------------------------------------
    # Sum over length
    ag.out <- aggregate(cbind(QBIOMASS, BIOMASS, QABUNDANCE, ABUNDANCE) ~ 
                        YEAR + STRAT + MISSION + SETNO + SPECIES, 
                        data = out, FUN = sum)
    
    # Remove herring at length
    f <- ag.out[ag.out$SPEC == 60 & ag.out$STRAT >= 470 & ag.out$STRAT <= 495,]
    ag.out <- ag.out[setdiff(rownames(ag.out),rownames(f)),]
    ag.out <- ag.out[ag.out$BIOMASS>0,]
    
    # Extract table with 7 columns:
    ## MISSION, SETNO, STRAT, YEAR, SPEC, ABUNDANCE, BIOMASS
    dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat, to_char(sdate,'yyyy') year, spec,sum(nvl(totno,0)*1.75/i.dist) Abundance,sum(nvl(totwgt,0)*1.75/i.dist) biomass from 
							groundfish.gsinf i, groundfish.gscat c, mfd_stomach.nafo_strat sg where i.mission=c.mission and i.setno=c.setno and i.strat=sg.strat and to_char(sdate,'mm') in ('06','07','08') and
							i.strat between '",s.strat,"' and '",e.strat,"' and type=1 and spec<9000 and to_char(sdate,'yyyy')=",yr[i],"
							group by i.mission,i.setno,i.strat,slat , slong ,to_char(sdate,'yyyy'), spec;",sep=""))
    
    # Fix high biomass estimate(s) for dogfish
    dat[dat$BIOMASS > 8000 & dat$SPEC != 220,'BIOMASS'] <- 0 # added in November 08, 2013 
    
    # Change NA for abundance or biomass based on mean size of animals captured (for invs only)
    dat[is.na(dat$ABUNDANCE), 'ABUNDANCE'] <- 0
    dat[is.na(dat$BIOMASS), 'BIOMASS'] <- 0
    
    # Add in the aggregated herrring
    her1 <- herringAggregate(path1=path)
    her1 <- her1[her1$ABUNDANCE>0,]
    f <-dat[dat$SPEC==60 & dat$STRAT>=470 & dat$STRAT<=495,]
    dat <- dat[setdiff(rownames(dat),rownames(f)),]
    mi <- unique(dat$MISSION)				
    h <- her1[her1$MISSION %in% mi,]
    
    dat <- rbind(dat, h)
    
    # Handle the missing biomass or abudance data where the other is present
    # by using mean weight = biomass/abundance
    if(any(dat$ABUNDANCE == 0 | dat$BIOMASS==0)) {
      
      wt <- sqlQuery(channel,paste("select * from mean_wts_for_fill_in;",sep="")) # table of mean fish weight ("SPEC" and "MEAN_WT_FISH", 640 observations)
      dat <- merge(dat, wt, by = c('SPEC'), all.x = T)                            # merge dat and wt
      dat[is.na(dat$MEAN_WT_FISH), 'MEAN_WT_FISH'] <- 0.5                         # if mean fish weight is NA, assign mean weight of 0.5
      
      if(any(dat$ABUNDANCE == 0)) {
        dat$ABUNDANCE[dat$ABUNDANCE == 0] <- dat$BIOMASS[dat$ABUNDANCE==0]/dat$MEAN_WT_FISH[dat$ABUNDANCE==0] # abundance = biomass/mean weight  			
      } # DD added this bracket so that biomass correction is OUTSIDE of the if statement for abundance
      if(any(dat$BIOMASS == 0)) {
        dat$BIOMASS[dat$BIOMASS == 0] <- dat$ABUNDANCE[dat$BIOMASS==0]*dat$MEAN_WT_FISH[dat$BIOMASS==0]					
      }
    }

    # Change ABUNDANCE = inf to ABUNDANCE = 1 
    if(unique(dat$YEAR)==2012 & any(!is.finite(dat$ABUNDANCE))) dat[which(!is.finite(dat$ABUNDANCE)),'ABUNDANCE'] <- 1
    
    # If any biomass or abundance is still NA, stop code
    if(any(is.na(dat[,c('BIOMASS','ABUNDANCE')]))) browser()
    
    # Replace 0's with small values
    dat[dat$BIOMASS == 0 ,'BIOMASS'] <- 0.01	
    dat[dat$ABUNDANCE == 0 ,'ABUNDANCE'] <- 1
    
    dat$QABUNDANCE <- dat$ABUNDANCE 
    dat$QBIOMASS <- dat$BIOMASS
    dat$SPECIES <- dat$SPEC       # need this line? Drop SPECIES column at line 145
    if(vessel.correction) dat <- vesselCorr(dat) # Apply vessel correction
    dat <- dat[, -which(names(dat) == 'SPECIES')]
    
    # Add in zero sets. 
    # These are needed to calculate the stratified means
    extra.sets <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,to_char(sdate,'yyyy') year from groundfish.gsinf i, nafo_strat sg where 
			i.strat=sg.strat and to_char(sdate,'yyyy') =",yr[i]," and to_char(sdate,'mm') in ('06','07','08') and i.strat between '",s.strat,"' and '",e.strat,"' and type=1;",sep=""))
    s <- unique(dat$SPEC)	
    m <- matrix(0, nrow = dim(extra.sets)[1], ncol=length(s),
                dimnames = list(c(1:nrow(extra.sets)), s))	
    m <- cbind(extra.sets, m)
    h <- melt(m, id = c("MISSION", "SETNO", "STRAT", "YEAR"))
    names(h)[which(names(h) %in% c('variable','value'))] <- c('SPECIES','BIOMASS')
    h$QBIOMASS <- h$QABUNDANCE <- h$MEAN_WT_FISH <- h$ABUNDANCE <- 0
    names(dat)[1] <- 'SPECIES'
    
    l <- rbind(dat, h, all = T)
    
    dat <- l[!duplicated(l[,c('MISSION','SETNO','SPECIES')], fromLast = F),]
    
    # Add in the qadjusted data
    dat1 <- dat
    w <- merge(dat, ag.out, by = c('MISSION','SETNO','STRAT','SPECIES','YEAR'), all.x = T)	
    # Add columns for ABUNDANCE, QABUNDANCE, BIOMASS, and QBIOMASS
    w$ABUNDANCE <- 0		
    w$QABUNDANCE <- 0		
    w$BIOMASS <- 0		
    w$QBIOMASS <- 0		
    
    # Fill in columns for ABUNDANCE, QABUNDANCE, BIOMASS, and QBIOMASS with appropriate data
    # .x is from dat (not q adj); .y is from ag.out (qadj)
    w$ABUNDANCE <- ifelse(is.na(w$ABUNDANCE.y), w$ABUNDANCE.x, w$ABUNDANCE.y)
    w$BIOMASS <- ifelse(is.na(w$BIOMASS.y), w$BIOMASS.x, w$BIOMASS.y)
    w$QABUNDANCE <- ifelse(is.na(w$QABUNDANCE.y), w$QABUNDANCE.x, w$QABUNDANCE.y)
    w$QBIOMASS <- ifelse(is.na(w$QBIOMASS.y), w$QBIOMASS.x, w$QBIOMASS.y)
    
    # Export aggregated data
    dat <- w[, c('MISSION','SETNO','SPECIES','YEAR','STRAT','ABUNDANCE','BIOMASS','QABUNDANCE','QBIOMASS')]		
    fna <- paste(path,"/data/aggregate/",sep="")
    dir.create(fna, recursive = T, showWarnings = F)
    save(dat, file = paste(fna, "num_biom", yr[i], ".RData", sep=""))
    
    rm(dat, w)
    
    print(paste("biomassData() function: finished year", yr[i]))
  } # end of loop over years
  
} # end of function


