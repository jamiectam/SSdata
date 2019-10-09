#'@title Applies length-based q-adjusted to biomass and abundance data (before
#'  stratification)
#'@description I'm not sure if I need this function since we are doing postStat.
#'  But it sets up the extraction of the biomass/abundance data in
#'  biomassData(). Note from AC: biomass q adju using medians from harley and
#'  myers tech report

#'@param species Species from catchability coefficients table
#'  (\code{catch_coef}) in \code{biomassData()}.
#'@param year Year to calculate the data
#'@param area Default is \code{area = "4VWX"}
#'@param fun_group Functional group from catchability coefficients table
#'  (\code{catch_coef}) in \code{biomassData()}.
#'@param q Initial q-correction from catchability coefficients table
#'  (\code{catch_coef}) in \code{biomassData()}.
#'@param len_corr Initial length correction from catchability coefficients table
#'  (\code{catch_coef}) in \code{biomassData()}.
#'@param nums Not really sure what this does.
#'@param a.b.plot Option to make length-weight plot.
#'@param by.set Return data by set. (?) Default is \code{by.set = TRUE}.
#'@param by.length Return data by set. (?) Default is \code{by.length = TRUE}.
#'@return Returns the q-ajusted biomass and abundance to ]code{bioamssData()}.
#'@references Modified code from AC's ExtractIndicators/R/qBiomass (function
#'  \code{biomass_q_adj()})
#'@importFrom stats coef
#'@importFrom graphics curve
#'@importFrom graphics title
#'@importFrom RODBC sqlQuery

qBiomass <- function(species, year, area = "4VWX", fun_group = NA, q = 0,
                        len_corr = 1, nums = F, a.b.plot = F, by.set = T, by.length=T) {
	#Checking to see if there is enough data to even bother with this species

	initial<-sqlQuery(channel,paste("select count(distinct c.mission||','||c.setno||','||",species,"||','||fshno) from groundfish.gsdet c, groundfish.gsinf i where i.mission=c.mission and i.setno=c.setno and to_char(sdate,'mm') in ('06','07','08') and strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"')) and spec=",species,";",sep=""))
	go<-ifelse(initial>30,TRUE,FALSE)
	if(go==TRUE) {
	
	#Obtain the numbers at length for 1 cm length interval
	aa<-sqlQuery(channel, paste("select distinct year,strat,mission,YDDMMSS,XDDMMSS,setno,flen,SUM(clen * DECODE(NVL(totwgt,0),0,1,DECODE(NVL(sampwgt,0),0,1,totwgt/sampwgt))) catch
	  			from 
	 				(SELECT distinct year, icl.strat, icl.mission, icl.setno, icl.flen FLEN,sampwgt,totwgt,nvl(clen,0)*1.75/dist clen, YDDMMSS,XDDMMSS
						 FROM
	 						(SELECT mission,setno, flen, SUM(clen) clen, AVG(fwt) avg_fwt
	    						FROM   groundfish.gsdet
	    						WHERE flen IS NOT NULL AND spec=",species,"
	    						GROUP BY mission,setno, FLEN) d,
	 						(SELECT year, mission, setno, strat, dist, totwgt, sampwgt, flen,YDDMMSS,XDDMMSS
	     						FROM
	       							(SELECT flen
	        							FROM groundfish.gs_lengths
	        							WHERE class=1
	          							AND flen <=(SELECT max(flen) + 1
	              										fROM groundfish.gsdet
	              										WHERE spec=",species," AND flen IS NOT NULL
	                									AND (mission, setno) IN
	                   										(SELECT DISTINCT i.mission, i.setno
	                     										FROM groundfish.gsinf i
	                     										WHERE to_char(sdate,'mm') in ('06','07','08') 
	                      										AND to_char(sdate,'yyyy') in (",year,") AND i.type=1))) l,
	       							(SELECT year, i.mission, i.setno, strat, dist, totwgt,YDDMMSS,XDDMMSS, sampwgt
	        							FROM
	        								(SELECT mission, setno, totwgt, sampwgt
	                							FROM groundfish.gscat WHERE spec=",species,") c,
	          								(SELECT to_char(sdate,'yyyy') year, i.mission, i.setno,slat YDDMMSS,slong*-1 XDDMMSS, i.strat, dist
	                							FROM groundfish.gsinf i
	           									WHERE  to_char(sdate,'mm') in ('06','07','08')
	                      						AND to_char(sdate,'yyyy') in (",year,") AND i.type=1) i
	        					WHERE i.mission=c.mission(+)
	          					AND i.setno=c.setno(+)) ic) icl
	 			WHERE icl.mission=d.mission(+) AND icl.setno=d.setno(+) AND icl.flen=d.flen(+) and icl.strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"')))
	 			 group by year,strat,mission,setno,flen, XDDMMSS,YDDMMSS;",sep=""	))
	 			 
		run<-ifelse(nrow(aa)<1,FALSE,TRUE)
	 	if(run==TRUE) {
	 	
				
		#order the data by fork length
		aa<-aa[order(aa$FLEN),]
	 	
		#adjust the catch at lengths by appropriate q and length correction
		
		#single q for species
	 	if(is.na(fun_group) && q>0 ) {aa$q<-q
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-paste('single_q-',q)
	 								} 
	 	
 		#cod length based q
	 	if(fun_group=='cod' && q==0 ) 
	 								{a1=-5.14
	 					 			b1=0.141
	 					 			g1=0.870
		 							aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'cod'
	 								}
	 	#haddock length based q
	 	if(fun_group=='haddock'&& q==0 ) 
	 							{	a1=-2.80
	 							  	b1=0.0661
	 					 			g1=1.5
		 							aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'haddock'
	 								}
	 	
	 	#pelagic gadoids length based q
	 	if(fun_group=='pg'&& q==0 )
	 	    				 	   {a1=-4.61
	 					 			b1=0.0789
	 					 			g1=0.58
		 							aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'pg'
									}
		
		#demersal gadoids length based q
		if(fun_group=='dg'&& q==0)
									{a1=-3.50
									 b1=0.0925
									 g1=0.968	
									 aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'dg'
	 								}
		
	 	#flat fish length based q
		if(fun_group=='ff'&& q==0)
									{a1=-4.35
									 b1=0.111
									 g1=0.831	
									 aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'ff'
	 								}
		
	 	# ling length based q	 	
	 	if(fun_group=='ling' && q==0) 
	 							  {a1=-13.9
									 b1=0.193
									 g1=1.66	
									 aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'ling'
		 							}
		# small pelagics length based q from H.Benoit
		if(fun_group=='sp' && q==0) 
	 							{a1=-17.7165
									 b1=0.175
									 g1=37710.7	
									 aa$q<-g1*(exp(a1+b1*(aa$FLEN*len_corr)))/(1+exp(a1+b1*(aa$FLEN*len_corr)))
		 							aa$corCatch<-aa$CATCH/aa$q
		 							aa$model<-'sp'
		 							}
								
	if(nums) {return(aa)}					
	 								
	#calculating appropriate length weight parameters (a's and b's)
							
	 	bb<-sqlQuery(channel,paste("select ",species,",flen,fwt from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and i.strat in (select strat from 
	 	mflib.gsmgt where unit in '",area,"') and to_char(sdate,'mm') in ('06','07','08') AND to_char(sdate,'yyyy') in ('",year,"') and spec in (",species,") and fwt is not null;",sep=""))
 
	 		 	if(nrow(bb)<100) {bb<-sqlQuery(channel,paste("select ",species,",flen,fwt from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and i.strat in (select strat from 
	 	mflib.gsmgt where unit in '",area,"') and to_char(sdate,'mm') in ('06','07','08') AND to_char(sdate,'yyyy') between ('",year,"'-5) and ('",year,"'+5) and spec in (",species,") and fwt is not null;",sep=""))
	 	}
	 	if(nrow(bb)<100){bb<-sqlQuery(channel,paste("select ",species,",flen,fwt from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and i.strat in (select strat from 
	 	mflib.gsmgt where unit in '",area,"') and to_char(sdate,'mm') in ('06','07','08') and spec in (",species,") and fwt is not null;",sep=""))
	 	}
	 	out<-with(bb,lm(log(FWT)~log(FLEN),bb))
	 	a.est<-coef(out)[1]
	 	b.est<-coef(out)[2]
	 if(a.b.plot) {
	 	#add in the lw plot (more of time keeper to see how far along the loop you are but nice to see none the less)
	 	with(bb,plot(FLEN,FWT))
	 	curve(exp(a.est+log(x)*b.est),add=T,col='red')
	 	title(paste(species,"-",year,"-",area))
		}
	 #q-adj biomass estimate
		aa$est.wt<-exp(a.est+log(aa$FLEN)*b.est)
		aa$qBiomass<-aa$corCatch*aa$est.wt
		aa$rvBiomass<-aa$CATCH*aa$est.wt
		names(aa)[8:14] <- c('ABUNDANCE','Q','QABUNDANCE','MODEL','ESTWT','QBIOMASS','BIOMASS')
	
	aa <- aa[,c(1:8,10,13,14)]
	aa$SPECIES <- species
	aa$QBIOMASS <- aa$QBIOMASS/1000 #for kg
	aa$BIOMASS <- aa$BIOMASS/1000  #for kg
	#print(paste(species,"-",year,"-",area))
	return(aa)
		}
		}
	}
	



