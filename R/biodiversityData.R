#'@title Extracts and exports fishery independent biomass and abundance data
#'@description Extracts and exports annual q-adjusted and not q-adjusted,
#'  length-based (separated into 1 cm length classes) and not length-based
#'  (aggregated over length) biomass and abundance data from the Scotian Shelf
#'  summer research vessel surveys. q-adjustments are applied at the set level
#'  (before stratification).
#'@details User must define \code{channel = odbcConnect("ptran", uid = ###, pwd
#'  = ###)} in the global environment. This channel must have access to the
#'  gscat, gsdet, gsinf, and gs_lengths tables from the groundfish database and
#'  the nafo_strat table from the mfd_stomach database.
#'
#'  Units: biomass - kg; abundance - numbers; length - cm.
#'
#'  If \code{BIOMASS} or \code{ABUNDANCE} data is missing when the other is
#'  present, the missing field is filled in based on the relationship
#'  \eqn{Weight_{Avg} = \code{BIOMASS}/\code{ABUNDANCE}}. \eqn{Weight_{Avg}} is
#'  from a sql call to "mean_wts_for_fill_in." If \eqn{Weight_{Avg}} is not
#'  available for a species, it is assumed that \eqn{Weight_{Avg} = 0.5}.
#'
#'  Abnormally high dogfish biomass estimates (> 8000 kg at the set level) are
#'  replaced with \code{BIOMASS = 0} kg.
#'
#'  Abundance estimates of \code{Inf} in 2012 are replaced with \code{ABUNDANCE
#'  = 1}.
#'
#'  Code will stop if any \code{BIOMASS} or \code{ABUNDANCE} estimates are
#'  \code{NA}.
#'
#'  Estimates of zero are replaced with small values: \code{BIOMASS = 0} is
#'  replaced with \code{BIOMASS = 0.01}; \code{ABUNDANCE = 0} is replaced with
#'  \code{ABUNDANCE = 1}.
#'@param path Filepath indicating where to create folders to store the extracted
#'  data.
#'@param s.strat Stratum for which to begin data extraction. Default is
#'  \code{s.strat = 440}. Code will stop with an error message if \code{s.strat
#'  < 440} or \code{s.strat > 495}.
#'@param e.strat Stratum for which to end data extraction. Default is
#'  \code{e.strat = 495}.  Code will stop with an error message if \code{e.strat
#'  < 440} or \code{e.strat > 495}.
#'@param s.year Year for which to begin data extraction.
#'@param e.year Year for which to end data extraction.
#'@param vessel.correction Logical value indicating whether to apply vessel
#'  correction to \code{BIOMASS} and \code{ABUNDANCE}. Default is
#'  \code{vessel.correction} = TRUE.
#'@return Creates directories to store extracted data.
#'
#'  Not length-based data are stored in path/data/aggregate/. This folder
#'  includes an RData file for each year called year.RData (object name
#'  \code{dat}). \code{dat} has 9 columns: \code{MISSION}, \code{SETNO},
#'  \code{SPECIES}, \code{YEAR}, \code{STRAT}, \code{BIOMASS}, \code{ABUNDANCE},
#'  \code{QBIOMASS} and \code{QABUNDANCE}.
#'
#'  Length-based data is stored in path/data/length/. This folder includes an
#'  RData file for each year called num_biom_at_length_year.RData (object name
#'  \code{out}). \code{out} has 10 columns: \code{MISSION}, \code{SETNO},
#'  \code{SPECIES}, \code{YEAR}, \code{STRAT}, \code{BIOMASS}, \code{ABUNDANCE},
#'  \code{QBIOMASS} and \code{QABUNDANCE}, and \code{FLEN}, where \code{FLEN} is
#'  length in 1 cm increments.
#'
#'@references Modified code from AC's biodiversityData.R
#'@importFrom stats aggregate
#'@importFrom reshape melt
#'@importFrom RODBC sqlQuery
#'@family RV functions
#'@export
#'

biodiversityData <- function(s.strat=440,e.strat=495,s.year,e.year,q.corr=T,add.zero.sets=F,by.length=F,warn=F) {

#herring data for 470-495 was estimated from the time series of VPA  - acoustic data from Arajuo and Bundy and partitioned to sets based on relative catch rates across sets
#use the add zero sets function for the resource potential function as it includes the sets with zero info

dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, spec,sum(totno*1.75/i.dist) Abundance,sum(totwgt*1.75/i.dist) biomass from 
                    groundfish.gsinf i, groundfish.gscat c, gomezc.nafo_strat sg where i.mission=c.mission and i.setno=c.setno and i.strat=sg.strat and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and
                    i.strat between '",s.strat,"' and '",e.strat,"' and type=1 and spec<9000
                    group by i.mission,i.setno,i.strat,slat , slong ,to_char(sdate,'yyyy'), spec;",sep=""))
#dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, spec,sum(totno*1.75/i.dist) Abundance,sum(totwgt*1.75/i.dist) biomass,
#   gridid,stratid,nafosubid,nafoid,esswssid,shelfid from 
#               groundfish.gsinf i, groundfish.gscat c, mfd_stomach.indicator_set_grids sg where i.mission=c.mission and i.setno=c.setno and i.mission=sg.mission and i.setno=sg.setno and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and to_char(sdate,'mm') in ('06','07','08') and
#               strat between '",s.strat,"' and '",e.strat,"' and type=1 and spec<9000
#               group by i.mission,i.setno,i.strat,slat , slong ,to_char(sdate,'yyyy'), spec,gridid,stratid,nafosubid,nafoid,esswssid,shelfid;",sep=""))
f <-dat[dat$SPEC==60 & dat$STRAT>=470 & dat$STRAT<=495,]
dat <- dat[setdiff(rownames(dat),rownames(f)),]
dat[!is.na(dat$BIOMASS)& dat$BIOMASS>8000 & dat$SPEC !=220,'BIOMASS'] <- 0
her <- sqlQuery(channel,paste("select i.mission,i.setno,i.strat,YDDMMSS,XDDMMSS,SPEC,year, totno abundance, totwgt biomass from 
 indiseas_4X_herring_summer i, nafo_strat sg where  i.strat=sg.strat and i.strat between '",s.strat,"' and '",e.strat,"' and year between ",s.year," and ",e.year,";",sep="" ))
her <- her[her$ABUNDANCE>0 | her$BIOMASS>0,]
dat <- rbind(dat,her)
dat <- dat[-which(is.na(dat['BIOMASS']) & is.na(dat['ABUNDANCE'])),]
dat[is.na(dat$ABUNDANCE),'ABUNDANCE'] <- 0
dat[is.na(dat$BIOMASS),'BIOMASS'] <- 0
if(any(dat$ABUNDANCE==0 | dat$BIOMASS==0)) {
  wt <- sqlQuery(channel,paste("select * from mean_wts_for_fill_in;",sep=""))
  dat <- merge(dat,wt,by=c('SPEC'),all.x=T)
  dat[is.na(dat$MEAN_WT_FISH),'MEAN_WT_FISH'] <- 0.5
  if(any(dat$ABUNDANCE==0)) {
    dat$ABUNDANCE[dat$ABUNDANCE==0] <- dat$BIOMASS[dat$ABUNDANCE==0]/dat$MEAN_WT_FISH[dat$ABUNDANCE==0]         
    if(any(dat$BIOMASS==0)) {
      dat$BIOMASS[dat$BIOMASS==0] <- dat$ABUNDANCE[dat$BIOMASS==0]*dat$MEAN_WT_FISH[dat$BIOMASS==0]                   
    }
  }
}
dat[dat$BIOMASS==0 & dat$ABUNDANCE==0,c('BIOMASS','ABUNDANCE')]<- c(0.01,1)

if(q.corr) {
  ## get teh data from q adj tables herring are already q adj
  dat1 <- sqlQuery(channel,paste("select year,q.strat,q.setno,yddmmss,xddmmss,qbiomass/1000 biomass,qabundance abundance,q.mission,species spec 
                     from q_biomass_abundance_by_set q, nafo_strat sg 
                        where  q.strat=sg.strat and q.strat between '",s.strat,"' and '",e.strat,"' and year between ",s.year," and ",e.year," and abundance>0;",sep=""))
  f <- merge(dat1,dat,all=T)
  dat <- f[!duplicated(f[,c(1,2,3,4,5,8,9)], fromLast=T),] # the from last works from bottom to top so the q corrected biomasses stay
}
if(add.zero.sets) {
  if(warn==T) {
    cat("Are you sure you want to add zero sets? \nThis is for use with the resourcePotential and invCVBiomass functions \n\t\t (y/n) \n")
    lens <- scan(what = "", nlines = 1, quiet = TRUE)
  }
  else lens <- 'y'
  if(lens=='y') {
    yr <- s.year:e.year
    oo<-list()
    for(i in 1:length(yr)) {
      dd <- dat[dat$YEAR==yr[i],]
      extra.sets <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,to_char(sdate,'yyyy') year, slat YDDMMSS, slong*-1 XDDMMSS from groundfish.gsinf i, nafo_strat sg where 
    i.strat=sg.strat and to_char(sdate,'yyyy') =",yr[i]," and to_char(sdate,'mm') in ('06','07','08') and i.strat between '",s.strat,"' and '",e.strat,"' and type=1;",sep=""))
      s <- unique(dat$SPEC)   
      m <- matrix(0,nrow=dim(extra.sets)[1],ncol=length(s),dimnames=list(c(1:nrow(extra.sets)),s))    
      m <-cbind(extra.sets,m)
      getPackage('reshape')
      h <- melt(m,id=c("MISSION", "SETNO", "STRAT", "YEAR", "YDDMMSS", "XDDMMSS"))
      names(h)[which(names(h) %in% c('variable','value'))]<- c('SPEC','BIOMASS')
      h$ABUNDANCE<-0
      h$MEAN_WT_FISH<-0
      l <- merge(dd,h,all=T)
      oo[[i]] <- l[!duplicated(l[,c('MISSION','SETNO','SPEC')], fromLast=T),] # the from last works from bottom to top so the q corrected biomasses stay
    }
    dat <- do.call(rbind,oo)
  }
}
return(dat)
}
