#data extract
#source('C:/Documents and Settings/cooka/Desktop/Scripts/Indicators/indicatorSpaceTime.R')
#source('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/scripts to extract indicators/indicatorSpaceTime.R')
#source("C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/indicatorSpaceTime.R")
	require(RODBC)
	require(PBSmapping)
	require(RColorBrewer)
	require(sqldf)
	require(reshape)
	memory.limit(4000)
	options(stringsAsFactors=F)
	
	source("R:\\Science\\Population Ecology Division\\Shared\\Adam Cook\\rmaps\\convert.dd.dddd.r")
	#source('C:/Documents and Settings/cooka/Desktop/Scripts/Indicators/getData.R')
	source("C:/Users/GomezC/Dropbox/Ecosystem Indicators/scripts to extract indicators/Catalina/getData.R")
	#source('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/scripts to extract indicators/getData.R')
  #if(!any(ls()=='atl.1')) {
#				atl<-read.table("R:\\Science\\Population Ecology Division\\Shared\\Adam Cook\\rmaps\\atlanticsHigh.ll", header=T)
#				atl.1<-thinPolys(atl,0.01)
#				rm(atl)
#				attr(atl.1,"projection")<-"LL"
#				}
	
	#setwd('C:/Documents and Settings/cooka/Desktop/Ecosytem indicators/Spatial Scales/analyses')
	setwd("C:/Users/GomezC/Dropbox/Ecosystem Indicators/outputs/Catalina")
  #setwd('C:/Documents and Settings/cooka/My Documents/Dropbox/Ecosystem Indicators/outputs/')
	#A <- biodiversityData(s.year=1970,e.year=2012,q.corr=T,add.zero.sets=T)				
	#B <- biodiversityData(s.year=1970,e.year=2012,q.corr=F,add.zero.sets=T)		
 	# C <- biodiversityData(s.year=1970,e.year=2012,q.corr=T,add.zero.sets=F)  
	#D <- biodiversityData(s.year=1970,e.year=2012,q.corr=F,add.zero.sets=F) 
	#E <- getLandings()
	
####
#indicator naming convention --- landings indicators are upper camel case and biomass indicators are lower camel case
###
	 indicator=c('kemptonQ',	'margalef','shannon','hillN1','hillN2',	'pielouSpeciesEvenness','margalefSpeciesRichness',	'speciesRichnessMeans',		'resourcePotential','speciesRichness',
						'meanMaxL',	'biomassPerTL',	'invCVBiomass',	'meanMaxAge','communityCondition','largeSpeciesIndicator','meanTrophicLevelCommunity','predatoryFish','biomassRatioInv2Dem',
						'biomassRatioPel2Dem','meanLengthCommunity','IVILandings','MeanTLLandings','FishingInBalance','TrophicBalanceIndex','LandByGroup','MarineTrophicIndex','FishingPressure',
						'LandingsDiversity')
						

	indicatorSpaceTime <- function(dat,groups=c('grid','strat','nafosub','shelf','esswss','bank','basin'),do.reg=F,lin.reg=T,bp.reg=F,subset.yr,npoints=15,out.table=T,
						file.name.plot.regs='Indicator',file.name.map="Indicator map",plot.map=T,agg.region=c('mean'),crit.p=0.1,n.breaks=10,
						indicator=c('kemptonQ',	'margalef',	'shannon','hillN1',	'hillN2',	'pielouSpeciesEvenness',
						'margalefSpeciesRichness',	'speciesRichnessMeans',	'speciesRichness',	'resourcePotential','meanMaxL',
						'biomassPerTL',	'invCVBiomass',	'meanMaxAge',	'communityCondition',	'largeSpeciesIndicator','meanTrophicLevelCommunity','predatoryFish','biomassRatioInv2Dem',
						'biomassRatioPel2Dem','meanLengthCommunity','IVILandings','MeanTLLandings','FishingInBalance','TrophicBalanceIndex','LandingsDiversity'),
						args=c('ALL','BIOMASS',user.defined=F), preDefinedGroups=T,stratified=T,means=F,totals=T, saveIndicatorData=T)
						
						 {
	if(substr(indicator,1,1) !=toupper(substr(indicator,1,1))){		#this sets up the nonlandings functions		
	cat('BIOMASS indicator')	 
	######################################
	#ARGS
	#dat is the input dataframe with headers of 'YEAR', 'X','Y','INDI', and if metrics based on strata then a column of 'STRAT'
	#groups is for grid or strata
	#lin reg is for linear regression
	#bp.reg is for breakpoint linear regression
	#npoints is a filter to remove relationships lacking data points
	#out.table prints the table
	#crit.p is the critical p for signicance of lm slopes
	#plot.map makes the trend plot for slopes from lm
	#agg.region chooses how to aggregate region either mean median or variance
	#indicator indicates what indicator you want to calculate
	#args are the extra arguments to pass onto the indicator calc 
	#		1) species grouping--finfish or all
	#       2) metric --ABUNDANCE or BIOMASS
	#       3) anything else to add  
	#TODO what to add into the maps from bp reg
	######################################						
		
    if(preDefinedGroups==FALSE) {
	#The saved grid for use in analysis at 0.6deg
	if(groups=='grid') {
			gr <- sqlQuery(channel,paste('select * from SCOTIAN_SHELF_GRID;'))
			}
	if(groups=='strat') {
			gr <- sqlQuery(channel,paste('select * from SCOTIAN_SHELF_STRAT_POLYS;'))
			gr <- gr[order(gr$PID,gr$POS),]
			gr$SID <- 1
			}
	if(groups=='nafo.sub') {
			gr <- sqlQuery(channel,paste('select * from NAFO_SUBUNIT_RV_SURVEY;'))
			gr <- gr[order(gr$PID,gr$POS),]
			}
	if(groups=='nafo') {
			gr <- sqlQuery(channel,paste('select * from NAFO_RV_SURVEY'))
			gr <- gr[order(gr$PID,gr$POS),]
			gr$SID <- 1
			}
	if(groups=='esswss') {					
			gr <- sqlQuery(channel,paste('select * from ESS_WSS_RV_SURVEY;'))
			gr <- gr[order(gr$PID,gr$SID,gr$POS),]
			}
	if(groups=='shelf') {					
			gr <- sqlQuery(channel,paste('select * from SHELF_RV_SURVEY;'))
		}

		if(any(names(dat)=='XDDMMSS')) {
		dat$X<-convert.dd.dddd(dat$XDDMMSS)
		dat$Y<-convert.dd.dddd(dat$YDDMMSS)
		}
		dat$EID <- 1:nrow(dat)
		dp <- split(dat,f=dat$MISSION)
		for(i in 1:length(dp)) {
			dw <- findPolys(dp[[i]],gr,maxRows=1e6)	
			dp[[i]] <- merge(dp[[i]],dw,by='EID')
		}
		
		dat <- do.call(rbind,dp)
		dat$IDS <- paste(dat$PID,dat$SID,sep=".")
		dat$ID <- paste(dat$MISSION,dat$SETNO,sep=".")
		
		dat1 <- dat
		}
		if(preDefinedGroups==TRUE) {
		#dat <- dat[,c(1:9,which(names(dat)==paste(toupper(groups),'ID',sep='')))]
		#names(dat)[length(dat)] <- 'IDS'
		dat$ID <- paste(dat$MISSION,dat$SETNO,sep=".")
		#dat$SPEC <- as.numeric(dat$SPEC)
		dat1 <- dat
		}
		
		if(indicator %in% c('resourcePotential','invCVBiomass') & !any(dat1$BIOMASS==0)) {
			
			stop('resourcePotential, invCVBiomass, biomassRatioInv2Dem require the \n inclusion of zero sets \n please run the biodiversityData script with add.zero.sets=T\n')
		
		}
		
		seti <- unique(dat1[c("YEAR", "STRAT", "SETNO","MISSION","ID")])
		yrs <- unique(dat1$YEAR)
			
				out<-list()
				for(i in 1:length(yrs)) {
						u <- dat1[dat1$YEAR==yrs[i],]
						if(indicator=='kemptonQ') 				 	out[[i]] <- kemptonQ(u, metric=args[2])
						if(indicator=='margalef')                	out[[i]] <- margalef(u,group=args[1], metric=args[2])
						if(indicator=='shannon')                 	out[[i]] <- shannon(u,group=args[1], metric=args[2])
						if(indicator=='hillN1')                  	out[[i]] <- hillN1(u,group=args[1], metric=args[2])
						if(indicator=='hillN2')                  	out[[i]] <- hillN2(u,group=args[1], metric=args[2])
						if(indicator=='pielouSpeciesEvenness')   	out[[i]] <- pielouSpeciesEvenness(u,group=args[1], metric=args[2])
						if(indicator=='margalefSpeciesRichness') 	out[[i]] <- margalefSpeciesRichness( u,group=args[1], metric=args[2])
						if(indicator=='speciesRichnessMeans')      	out[[i]] <- speciesRichnessMeans(u,group=args[1], metric=args[2])
						if(indicator=='speciesRichness')         	out[[i]] <- speciesRichness(u,group=args[1],grps=groups)
						if(indicator=='resourcePotential' | 
									indicator=='invCVBiomass')		out[[i]] <- resourcePotential(u,group=args[1], metric=args[2],user.defined=F)#arg[3])  
						if(indicator=='biomassRatioInv2Dem')		out[[i]] <- biomassRatioInv2Dem(u)
						if(indicator=='biomassRatioPel2Dem')		out[[i]] <- biomassRatioPel2Dem(u)
						if(indicator=='meanMaxL')                	out[[i]] <- meanMaxL(u, metric=args[2])
						if(indicator=='meanMaxAge')              	out[[i]] <- meanMaxAge(u, metric=args[2])
						if(indicator=='biomassPerTL')            	out[[i]] <- biomassPerTL(u, metric=args[2])
						if(indicator=='communityCondition')    	 	out[[i]] <- communityCondition(u,group=args[1])	
						if(indicator=='largeSpeciesIndicator')   	out[[i]] <- largeSpeciesIndicator(u)	
						if(indicator=='predatoryFish')   		 	out[[i]] <- predatoryFish(u)	
						if(indicator=='meanTrophicLevelCommunity')  out[[i]] <- meanTrophicLevelCommunity(u)	
						print(yrs[i])
				}
			outs <- as.data.frame(do.call(rbind,out))
			if(indicator=='speciesRichness') {
			dat1 <- outs
			names(dat1)[1:2] <- c('IDS','INDI')
			dat2 <- dat1[,c(3,1,2)]
			dat2 <- dat2[order(dat2$Year),]
			dat3 <- dat2[!is.na(dat2[,2]),]
			}
			else { 
			if(indicator=='meanLengthCommunity') {
			dat1 <- meanLenComm(grps=groups,metric=args[2])
				}
			if(indicator=='largeFishIndicator') {
			dat1 <- largeFishIndicator(grps=groups,metric=args[2])
				}
			
			if(!indicator %in% c('meanLengthCommunity','largeFishIndicator')) {
			dat1 <- merge(seti,outs,by='ID')
			names(dat1)[length(dat1)] <- 'INDI'
			dat1 <- dat1[is.finite(dat1[,ncol(dat1)]),]
			}	

		if(stratified==TRUE) {
				ww <- sqlQuery(channel,paste("select * from nafo_strat"))
					wx <- ww[,c('STRAT','TUNITS',paste(toupper(groups),'ID',sep=''))]
				names(wx)[length(wx)] <- "IDS"				
				wm <- within(wx,{wts=TUNITS/ave(TUNITS,as.integer(IDS),FUN=sum)})
				
					
		if(indicator=='biomassPerTL' ) {
		
						dat2 <- aggregate(INDI~YEAR+STRAT+TL, data=dat1,FUN=agg.region,na.action=na.omit)
						dat2<- merge(dat2,wm,by=c('STRAT'))
						
						if(totals==TRUE) {
					dat2$INDI<- dat2$INDI*dat2$TUNITS
					dat3 <- aggregate(INDI~YEAR+IDS+TL,data=dat2,FUN=sum)
					}
					if(means==TRUE) {
					dat2$INDI<- dat2$INDI*dat2$wts
					dat3 <- aggregate(INDI~YEAR+IDS+TL,data=dat2,FUN=mean)
				}
			}
		if(indicator=='biomassRatioInv2Dem' | indicator=='biomassRatioPel2Dem') {
		names(dat1)[6:7] <- c('GFISH','INVS')
								dat2 <- aggregate(dat1[,c('GFISH','INVS')],by=dat1[,c('YEAR','STRAT')],dat=dat1,FUN=agg.region)
								dat2<- merge(dat2,wm,by=c('STRAT'))
						if(totals==TRUE) {
					stop('Run this as means')
					}
					if(means==TRUE) {
					dat2$GFISH<- dat2$GFISH*dat2$wts
					dat2$INVS<- dat2$INVS*dat2$wts
					dat3 <- aggregate(dat2[,c('GFISH','INVS')],by=list(dat2$YEAR,dat2$IDS),FUN=sum)
					dat3$INDI <- dat3$INVS/dat3$GFISH
					dat3 <- dat3[,c(1,2,5)]
					names(dat3)[1:2] <- c('YEAR','IDS')
					}
		} 
		
		if( !indicator %in% c('biomassRatioInv2Dem','biomassRatioPel2Dem','biomassPerTL')) {	
						dat2 <- aggregate(INDI~YEAR+STRAT, data=dat1,FUN=agg.region,na.action=na.omit)     
						dat2<- merge(dat2,wm,by=c('STRAT'))
										
					if(totals==TRUE) {
					dat2$INDI<- dat2$INDI*dat2$TUNITS
					dat3 <- aggregate(INDI~YEAR+IDS,data=dat2,FUN=sum)
					}
					if(means==TRUE) {
					dat2$INDI<- dat2$INDI*dat2$wts
					dat3 <- aggregate(INDI~YEAR+IDS,data=dat2,FUN=sum)
						}
					}
				}
		if(indicator =='invCVBiomass') {
			dat3 <- invCVBiomass(X=dat3,window=5)
			}			
			
		if(stratified==FALSE) { #not bumped up to total area it is just the mean across all sets
		ww <- sqlQuery(channel,paste("select * from nafo_strat"))
			wx <- ww[,c('STRAT',paste(toupper(groups),'ID',sep=''))]
				names(wx)[length(wx)] <- "IDS"
			dat1 <-merge(dat1,wx,by='STRAT')		
		if(indicator=='biomassPerTL' ) {
							
				dat3 <- aggregate(INDI~YEAR+IDS+TL, data=dat1,FUN=agg.region,na.action=na.omit)
					} else {				
					dat3 <- aggregate(INDI~YEAR+IDS, data=dat1,FUN=agg.region,na.action=na.omit)     
					}
				}
				
	
	wl <- ww[,c(which(paste(toupper(groups),'ID',sep="")== names(ww)),which(toupper(groups)== names(ww)))]
	names(wl) <- c('IDS','NAMES')
	wl <- unique(wl[,c('IDS','NAMES')])
	dat3 <- merge(dat3,wl,"IDS")
	}
	}
	if(substr(indicator,1,1) ==toupper(substr(indicator,1,1))) {
	cat('LANDINGS indicator\n')
			grp <- sqlQuery(channel,paste('select * from landings_groupings;'))
			wl <- grp[,c(1,which(toupper(groups)== names(grp)))]
			names(wl) <- c('NAFO_UNIT','NAMES')
			dat <- merge(dat,wl,by='NAFO_UNIT')
		if(indicator=='IVILandings') 			out <- IVILandings(land=dat)
		if(indicator=='MeanTLLandings')			out <- MeanTLLandings(land=dat)
		if(indicator=='FishingInBalance')		out <- 	FishingInBalance(land=dat)
 		if(indicator=='TrophicBalanceIndex')  	stop('Under construction!!/n') #out <- TrophicBalanceIndex(land=dat,grps=groups) not working drop for now February 18, 2013 01:48:30 PM 
 		if(indicator=='LandByGroup') 			out <- LandByGroup(land=dat,group=args[1])
 		if(indicator=='MarineTrophicIndex') 	out <- MeanTLLandings(land=dat,cutoff=3.25)
 		if(indicator=='FishingPressure') 		out <- FishingPressure(land=dat,grps=groups, group=args[1]) 		
 		if(indicator=='LandingsDiversity') 		out <- LandingsDiversity(land=dat,grps=groups)

		dat3 <- out
		}
	
	if(do.reg==F) {
		if(saveIndicatorData) {
  		a<-getwd()
		b<-Sys.time()
		b<-format(b,format="%m-%d %H%M",tz='America/Halifax')
		f<-paste(indicator,groups,args[1],args[2],b,".csv",sep="-")
		write.csv(dat3,f)
   		cat("\n")
		cat('Your file is in:\n')
		cat(paste(a,"/",f,sep=""))
		cat("\n")
		cat("------------------------------- \n")
    	cat("\n")
		}
		return(dat3)
		
		} else {
			if(any(paste(file.name.plot.regs,".pdf",sep="")==dir())) {
	cat("Do you want to overwrite your previous file  \n", paste(file.name.plot.regs,".pdf? (y/n)\n",sep=""))
	lens <- scan(what = "", nlines = 1, quiet = TRUE)
	}
	if(lens=='y') {
	if(any(paste(file.name.map,".pdf",sep="")==dir())) {
	cat("Do you want to overwrite your previous file  \n", paste(file.name.map,".pdf? (y/n)\n",sep=""))
		lens <- scan(what = "", nlines = 1, quiet = TRUE)
		}
	if(lens=='y') {	
			
		ids <- unique(dat2$IDS)
			if(lin.reg) {
				output <- data.frame(ID=ids,Slope=NA,p=NA)
			pdf(paste(file.name.plot.regs,'.pdf',sep='')) 
		for(i in 1:length(ids)) {
			Z <- dat2[dat2$ID==ids[i],]
				if(nrow(Z)>npoints) {
					ZZ <- summary(lm(INDI~YEAR,data=Z))
					output[i,2:3] <- c(ZZ$coefficients[[2]],ZZ$coefficients[[8]])
					
						with(Z,plot(YEAR,INDI),xlab='Year',ylab='Indicator')
						abline(lm(INDI~YEAR,data=Z))
						title(ids[i])
							rp = vector('expression',2)
								rp[1] = substitute(expression(italic(Slope) == MYVAL), 
									list( MYVAL= format(ZZ$coefficients[[2]], digits = 3)))[2]
								rp[2] = substitute(expression(italic(p) == MYVAL2), 
									list(MYVAL2 = format(ZZ$coefficients[[8]], digits = 2)))[2]
						legend('topright',rp,bty='n',cex=0.8)
					dstamp()
					}
				}
				dev.off()
				cat(paste('Your regression plots file is in: \n', getwd(),"/",file.name.plot.regs,".pdf\n",sep=""))
	output[c('PID','SID')] <- as.data.frame(do.call(rbind,strsplit(output[,1],"\\.")))
	output$Slope <- ifelse(output$p<crit.p,output$Slope,0)
	
	#TODO need to do something with the na values where there is not enough data for the maps na omit works but sucks
		output <- na.omit(output)
     #   if(nrow(output)>10) {
	brks<-seq(min(output['Slope']),max(output['Slope']),length.out=n.breaks)
	negbrks <- length(brks[brks<=0])
	if(negbrks==0) {
				brks<-seq(min(output['Slope']),max(output['Slope']),length.out=9)
				cols<-c("#FFFFFF",brewer.pal((length(brks)-1),'Reds'))
				} else 
	if(length(brks)-negbrks==0) {
				brks<-seq(min(output['Slope']),max(output['Slope']),length.out=9)
				cols<-c(brewer.pal((length(brks)-1),'Blues'),"#FFFFFF")
				} else
							{
	brks <- c(brks[1:negbrks],0,brks[(negbrks+1):length(brks)])
	require(RColorBrewer)
	cols<-c(rev(brewer.pal(negbrks,'Blues')), "#FFFFFF",brewer.pal((length(brks)-1-negbrks),'Reds'))
	if(negbrks<3) cols <- cols[-1]
	if(length(brks)-negbrks<3) cols<-cols[-length(cols)]
	}
	#} else {
	
	
	#}
	browser()
	pwt <- output[,c('Slope','PID','SID')]
	names(pwt)<-c('Z','PID','SID')
			pwt[,3]<-as.numeric(pwt[,3])
			pwt[,2]<-as.numeric(pwt[,2])
			pdata<-as.PolyData(pwt)
			pdata<-makeProps(pdata,brks,"col",cols)
			pdata$col <- ifelse(pdata$Z==0,"#FFFFFF",pdata$col)
			 layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			box()
			attr(gr,"projection")<-"LL"
			addPolys(gr,polyProps=pdata)
			addPolys(atl.1,col='khaki')
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,c('Pos','','','','','','','','','','Neg'), fill=rev(cols),cex=.75,bty='n') #10
			arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
			savePlot(paste(file.name.map,'.pdf',sep=""),type="pdf")
			cat(paste('Your map plot is in: \n', getwd(),"/",file.name.map,".pdf\n",sep=""))

			}
	if(bp.reg) {
		output<-list()
		pdf(paste(file.name.plot.regs,'.pdf',sep='')) 
			for(i in 1:length(ids)) {			
				Z <- dat2[dat2$ID==ids[i],]
					if(nrow(Z)>npoints) {
						require(strucchange)
						y <- min(Z$YEAR):max(Z$YEAR)
						h <- with(Z,INDI[match(y,YEAR)])
						h <- ts(h,start=min(y),end=max(y))
						bp.h <- breakpoints(h ~ 1)
						if(!is.na(breakpoints(bp.h)[1])) {
					## the BIC also chooses one breakpoint
							j <- breakpoints(bp.h)$breakpoints							
							fm0 <- lm(h ~ 1)
							fm1 <- try(lm(h ~ breakfactor(bp.h, breaks = length(j))),silent=T)
							if(class(fm1)=='try-error') next
							plot(h)
							lines(ts(fitted(fm0), start = min(y)), col = 3)
							lines(ts(fitted(fm1), start = min(y)), col = 4)
							lines(bp.h)
							## confidence interval
							ci.h <- confint(bp.h)
							lines(ci.h)
							title(ids[i])
							legend('topright',bty='n',cex=0.7,legend=breakdates(bp.h))
						output[[i]] <- bp.h
						
					}
					print(i)
					}
				}
				dev.off()
				cat(paste('Your regression plots file is in: \n', getwd(),"/",file.name.plot.regs,".pdf\n",sep=""))
				cat(paste('Your map plot is in: \n', getwd(),"/",file.name.map,".pdf\n",sep=""))
			}
		}
		}
		}
		}
		
##species <- paste(1:900,collapse=",")
##dat <- sqlQuery(channel,paste("select to_char(sdate,'yyyy') year, c.mission,c.setno,slat YDDMMSS,slong*-1 XDDMMSS,sum(totwgt)*1.75/dist wt, sum(totno)*1.75/dist num from groundfish.gsinf i, groundfish.gscat c
##						where i.mission=c.mission and i.setno = c.setno and spec in (",species,") and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and type=1 
##						group by to_char(sdate,'yyyy'), c.mission,c.setno,slat,slong,dist;", sep=""))
##		
##		dat$X<-convert.dd.dddd(dat$XDDMMSS)
##		dat$Y<-convert.dd.dddd(dat$YDDMMSS)
##		
##dat <- dat[,c('YEAR','X','Y','WT')]
##names(dat)[4] <- 'INDI'
##dat['INDI'] <- log(dat['INDI']+1)
##
#	indicatorSpaceTime(dat=dat,agg.region='mean',groups='nafo.sub',lin.reg=T,bp.reg=F,npoints=25,file.name.map='Biomass grid')