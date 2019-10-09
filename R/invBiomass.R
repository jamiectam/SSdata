#'@title Pretty sure this extracts the invertebrate biomass. But doesn't look
#'  like it is called by any other function so might not need it!
#'  @description Not sure why the default is \code{e.year = s.year}
#'@inheritParams biomassData
#'@param species Probably the species for which to extract data
#'@param region Probably the region for which to extract data
#'@param season Probably the season for which to extract data
#'@references Modified code from AC's ExtractIndicators/R/qBiomass.R
#'@export
#'@importFrom RODBC sqlQuery

inv.biomass <- function (species, region, season, s.year, e.year = s.year) {
  Biomass <-0
  if(grepl('4VSW',toupper(region))) {bstrat=443; estrat=466}
  if(grepl('4X',toupper(region))) {bstrat=470; estrat=495}
  if(grepl('4VW',toupper(region))) {bstrat=440; estrat=466}
  if(grepl('4VWX',toupper(region))) {bstrat=440; estrat=495}
  if(grepl('4VN',toupper(region))) {bstrat=440; estrat=442}
  if(grepl('S',toupper(season))) {seasons<-paste(c('06','07','08'),collapse="','"); strat.details<-sqlQuery(channel,paste("select s.strat, (area/(1.75*41/6080.2)) , s.strat  from groundfish.gsstratum s, mflib.gsmgt g where s.strat=g.strat and g.unit in ('",region,"') and s.strat between '440' and '495' order by s.strat;",sep=""))}
  if(grepl('W',toupper(season))) {seasons<-paste(c('01','02','03','04'),collapse="','"); strat.details<-sqlQuery(channel,paste("select s.strat, (area/(1.75*41/6080.2)) , s.strat  from groundfish.gsstratum s, mflib.gsmgt g where s.strat=g.strat and g.unit in ('",region,"') and s.strat between '401' and '411' order by s.strat;",sep=""))}	
  
  #null data gets put to zero with invert biomass prior to 1999
  data <- sqlQuery(channel, paste("select tot.mission,tot.setno,tot.strat,nvl(wgt,0) wgt 
                                  from (select i.mission,i.strat, i.setno, sum(totwgt) wgt from groundfish. gsinf i, groundfish.gscat c where
                                  i.mission=c.mission and i.setno=c.setno and spec in (",species,") and to_char(sdate,'mm') in ('",seasons,"') and strat in (select distinct strat from mflib.gsmgt where unit in ('",region,"'))
                                  and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and type=1
                                  group by i.mission, i.strat,i.setno) fish,
                                  (select i.mission,i.strat, i.setno from groundfish. gsinf i where
                                  to_char(sdate,'mm') in ('",seasons,"') and strat in (select distinct strat from mflib.gsmgt where unit in ('",region,"'))
                                  and to_char(sdate,'yyyy') between ",s.year," and ",e.year," and type=1) tot
                                  where tot.mission=fish.mission(+) and tot.setno=fish.setno(+);",sep=""))
  
  if(nrow(data)>1) {
    strat.means <- aggregate(data$WGT, by=list(data$STRAT), FUN=mean)
    strat.n <- aggregate(data$WGT, by=list(data$STRAT), FUN=length)
    strat.wgts <- data.frame(Group.1=strat.details[,1],Wgt=strat.details[,2]/sum(strat.details[,2]), Trawl.Units= strat.details[,2])
    strata.data <- merge(merge(strat.means,strat.n,by='Group.1'),strat.wgts,by='Group.1')
    names(strata.data) <-c('Strat','Mean','NSets','AreaWt','Trawl.Units')
    Biomass <- data.frame(Species=species,Year=s.year, Area= region, BiomassT=sum(strata.data$Mean*strata.data$Trawl.Units)/1000)
  }
  return(Biomass)
}

