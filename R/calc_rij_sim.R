#=============================================================================
#Reproduction of the spreadsheets produced by 
#Kaitlyn Dionne and Chuck Parkens
#Author: Catarina Wor
#date: July 13th 2018
#=============================================================================

#**********IMPORTANT*************
# set directory to this file directory ctrl +shift +h on sublime
#should be automatic on Rstudio
#source("directories.R")
#==========================================================================

#packages
library(dplyr)
library(readxl)

library(ggplot2)

#==========================================================================
#auxiliary functions
source("func_tagcodeextract.R")
source("func_calcrij.R")
mysum<-function(x){return(sum(x,na.rm=T))}
#==========================================================================
#data
#setwd(data_dir)

catches <- read.csv("../data/Catch.csv",stringsAsFactors = FALSE,na.strings = c("NA", " "))
recs <- read.csv("../data/HarRecs.csv",stringsAsFactors = FALSE)
har_esc <-read.csv("../data/HarEscapementCWT2.csv",stringsAsFactors = FALSE)
Tag_codes<-read.csv("../data/Tag_codes.csv",stringsAsFactors = FALSE)
statweek<-read.csv("../data/statweek.csv")
PFMA_codes<-read.csv("../data/PFMAclassification_month.csv", na.string= c("NA",""," "))
#PFMA_codes_recs<-read.csv("../data/PFMAclassification_month_recs.csv", na.string= c("NA",""," "))

#add month category to catches and recoveries


catches$month<-statweek$month[match(catches$PERIOD_ID,statweek$Statweek)]
recs$month<-statweek$month[match(recs$PERIOD_ID,statweek$Statweek)]

catches<-left_join(catches, PFMA_codes[,-ncol(PFMA_codes)], by=c('STRATA_NAME'='Strata.name', 'month'='month',
 "STRATA_DESCRIPTION"="PFMA"))

recs<-left_join(recs, PFMA_codes[,-ncol(PFMA_codes)], by=c('STRATA_NAME'='Strata.name', 'month'='month'))

#match(catches$STRATA_NAME,PFMA_codes$Strata.name)
#catches$class<-PFMA_codes$Classification
#catch_treaty<-left_join(catches, PFMA_codes[,-ncol(PFMA_codes)], by=c('STRATA_NAME'='Strata.name', 'month'='month', 
#aggregate(catches$KNOWN_TAGS,list(catches$STRATA_DESCRIPTION,catches$STRATA_NAME,catches$STRATA_NAME,catches$month),sum)


#filter for har esc years
har_esc<-har_esc[har_esc$Spawning_Year>2008&har_esc$Spawning_Year<2016,]

#==========================================================================
#Processing data
#==========================================================================

#match observed tag codes with brood year, stock and age
#for tags recovered in the catch
recsa<-tagextract(tcImp=recs$TAG_CODE,
		 sampleyr=as.numeric(recs$RECS_YEAR), 
		 tctab_id=Tag_codes$CWT,
		  tctab_by=Tag_codes$BY,
		  tctab_stk=Tag_codes$Stock.Acr)

recs$BroodYear<-recsa$BroodYear
recs$Stock<-recsa$Stock
recs$Age<-recsa$Age

esca<-tagextract(tcImp=har_esc$Tagcode,
		 sampleyr=as.numeric(har_esc$Spawning_Year), 
		 tctab_id=Tag_codes$CWT,
		  tctab_by=Tag_codes$BY,
		  tctab_stk=Tag_codes$Stock.Acr)

har_esc$BroodYear<-esca$BroodYear
har_esc$Stock<-esca$Stock
har_esc$Age<-esca$Age

#=====================================================================
#clean up catch data

#remove observations for which the CWT estimates are not available
recs<-recs[-which(is.na(recs$RECS_STS_ID)),]
catches<-catches[-which(is.na(catches$CWT_ESTIMATE)),]



#tags by strata and brood year
obs_tags<-aggregate(recs$TAG_CODE,list(recs$RECS_YEAR,recs$STRATA_NAME,recs$BroodYear,recs$Stock,recs$Age,recs$PERIOD_NAME),length)
names(obs_tags)<-c("Year","fishery","BY","Stock","Age","PERIOD_NAME","Mij")
obs_tags$timeareastrata <-paste(obs_tags$fishery,obs_tags$PERIOD_NAME)
summary(obs_tags)

#summarize individual recoveries by STS_ID, fishery,year and time
recssum<-aggregate(recs$OBSERVED_HEADS,list(recs$RECS_STS_ID,recs$RECS_YEAR,recs$STRATA_ID,
										recs$STRATA_NAME,recs$PERIOD_TYPE_ID,recs$PERIOD_ID, recs$PERIOD_NAME),sum)
names(recssum)<-c("STS_ID","Year","STRATA_ID","STRATA_NAME","RECS_PERIOD_TYPE_ID","RECS_PERIOD_ID","PERIOD_NAME","OBSERVED_HEADS" )
recssum$timeareastrata<-paste(recssum$STRATA_NAME,recssum$PERIOD_NAME)

recsrepeat<-aggregate(recs[,"CWT_ESTIMATE"],list(recs$RECS_STS_ID,recs$RECS_YEAR,recs$STRATA_ID,
										recs$STRATA_NAME,recs$PERIOD_TYPE_ID,recs$PERIOD_ID, recs$PERIOD_NAME),unique)
recssum$CWT_ESTIMATE<-(recsrepeat$x)

res2 <- full_join(catches, recssum, by=c('STS_ID'='STS_ID', 'STS_YEAR'='Year','STRATA_ID'='STRATA_ID',
										'STRATA_NAME'='STRATA_NAME','PERIOD_TYPE_ID'='RECS_PERIOD_TYPE_ID',
										'PERIOD_ID'='RECS_PERIOD_ID', 'PERIOD_NAME'='PERIOD_NAME'#,'CWT_ESTIMATE'='CWT_ESTIMATE'
										))
res2$timeareastrata <-paste(res2$STRATA_NAME,res2$PERIOD_NAME)


#filter for only fisheries in which tagshave been recovered.
res3<-res2[which(!is.na(res2$OBSERVED_HEADS)),]



#res3[is.na(res3$CATCH_TOTAL),]

tottag<-NULL
tothead<-NULL
for(a in 1:nrow(res3)){
	tottag[a]<-sum(c(res3$LOST_TAGS[a],res3$UNREADABLE_TAGS[a],res3$UNRESOLVED_TAGS[a],res3$KNOWN_TAGS[a]),na.rm=T)
	tothead[a]<-sum(c(res3$LOST_TAGS[a],res3$NO_TAGS[a],res3$UNREADABLE_TAGS[a],res3$UNRESOLVED_TAGS[a],res3$KNOWN_TAGS[a]),na.rm=T)
}


GNirecsim<-aggregate(tottag,list(res3$STS_YEAR,res3$timeareastrata,res3$Classification),function(x){return(0.1^2)})
names(GNirecsim)<-c("year","strata","group","GNi" )


tag<-aggregate(tothead,list(year=res3$STS_YEAR,strata=res3$timeareastrata,group=res3$Classification),function(x)sum(x,na.rm=T))

cattot<-aggregate(res3$CATCH_TOTAL,list(year=res3$STS_YEAR,strata=res3$timeareastrata,group=res3$Classification),function(x)sum(x,na.rm=T))

samplerate<-aggregate(1/res3$CWT_ESTIMATE.y,list(year=res3$STS_YEAR,strata=res3$timeareastrata,group=res3$Classification),unique)

cbind(rec$strata,cattot$strata,samplerate$strata,tag$strata,rec$strata==cattot$strata,cattot$strata==samplerate$strata,samplerate$strata==tag$strata)

cattot$x[which(cattot$x<tag$x)]<-tag$x[which(cattot$x<tag$x)]/samplerate$x[which(cattot$x<tag$x)]


GNirecsim$catch<-cattot$x
GNirecsim$phi<-samplerate$x
rec<-aggregate(res3$KNOWN_TAGS/tottag,by=list(Year=res3$STS_YEAR,strata=res3$timeareastrata,group=res3$Classification),unique)
GNirecsim$lambda<-rec$x


#phii = Fraction of catch that is inspected for CWT
#names(har_esc)TODO: figure out why sample rate is weird in 2013


#process escapement data
#mij=#  n decoded cwt by stratum and age
#escapement
esctag<-aggregate(har_esc$Total_Number_CWT_Observed[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
		by=list(Age=har_esc$Age[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			timeareastrata=har_esc$Sex[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			Year=har_esc$Spawning_Year[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"]),sum)



ProportionAFCfishStratum<-har_esc$Total_AFC_Sampled_Stratum/har_esc$Total_Number_Fish_Examined_Mark_Status_Stratum 

NumberAFCfishStratum<-har_esc$Total_Number_Fish_Stratum*ProportionAFCfishStratum

SampleRate<-har_esc$Total_Number_AFC_Heads_Sampled_Stratum/(NumberAFCfishStratum)




head(har_esc)
names(har_esc)

GNiescsim<-aggregate(har_esc$CV_Number_Fish_Stratum^2,list(har_esc$Spawning_Year,har_esc$Sex),unique)
names(GNiescsim)<-c("year","strata","GNi" )
GNiescsim$group<-"escapement"


samprt<-aggregate(SampleRate,list(har_esc$Spawning_Year,har_esc$Sex),unique)
escmt<-aggregate(NumberAFCfishStratum,list(har_esc$Spawning_Year,har_esc$Sex),unique)


#decoding rate of CWTs

dec_tags_esc<-aggregate(har_esc$Total_Number_CWT_Observed[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
		by=list(Year=har_esc$Spawning_Year[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			strata=har_esc$Sex[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"]
			),sum)
all_tags_esc<-aggregate(har_esc$Total_Number_CWT_Observed,by=list(Year=har_esc$Spawning_Year,strata=har_esc$Sex),sum)
dim(all_tags_esc)

phi_esc <- as.numeric(aggregate(SampleRate,list(Year=har_esc$Spawning_Year,strata=har_esc$Sex),unique)$x)

#lambdai_esc<-data.frame(lambdai=dec_tags_esc$x/all_tags_esc$x, Year=dec_tags_esc$Year,strata=dec_tags_esc$stratum,fishery="escapement",group="escapement")



GNiescsim$catch<-escmt$x
GNiescsim$phi<-samprt$x
GNiescsim$lambda<-dec_tags_esc$x/all_tags_esc$x


GNisim<-rbind(GNirecsim,GNiescsim)
#=======================================================================================
#produce quantities to be used in function

obstags<-data.frame(Age=c(obs_tags$Age,esctag$Age),
	Year=c(obs_tags$Year,esctag$Year),
	timeareastrata=c(obs_tags$timeareastrata,as.character(esctag$timeareastrata)),
	Mij=c(obs_tags$Mij,esctag$x))

#decoding rate of CWTs


#% of individuals tagged - assuming !00% of individuals are tagged, i.e. only monitoring exploitation rates of CWTd fish
thetaj<-rep(1, length(unique(obstags$Age)))
#==========================================================================
#draft functions


#start writing and test them here

yrs<-unique(GNisim$year)

ags<- unique(obstags$Age)
nsims<-200
rijsims<-matrix(NA,ncol=nsims,nrow=nrow(obstags))
Tisims<-matrix(NA,ncol=nsims,nrow=nrow(GNisim))
Tgsims<-matrix(NA,ncol=nsims,nrow=nrow(aggregate(GNisim$catch,list(year=GNisim$year,GNisim$group),sum)))
ERsims<-matrix(NA,ncol=nsims,nrow=nrow(aggregate(GNisim$catch,list(year=GNisim$year,GNisim$group),sum)))

for(n in 1:nsims){

	Catchobs<-NULL
	sampleobs<-NULL
	phiobs<-NULL
	#lamdaobs<-NULL
	rijobs<-NULL
	rij<-NULL
	grp<-NULL
	Ti<-NULL
	Tiobs<-NULL
	yrgi<-NULL
	grpgi<-NULL


	a=1

	for(y in 1:length(yrs)){

		stra<-unique(GNisim$strata[GNisim$year==yrs[y]])
		g<-unique(GNisim$group[GNisim$year==yrs[y]])
	
		for(ta in 1:length(stra) ){


			#catch error
			Catchobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-ceiling(GNisim$catch[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]*exp(rnorm(1,mean=0, 
																	sd=sqrt(GNisim$GNi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]) )))
			#Catchobs[a]<-ceiling(GNisim$catch[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]*exp(rnorm(1,mean=0, 
			#														sd=sqrt(GNisim$GNi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]) )))
			
			#sampling error
			sampleobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-rbinom(1,Catchobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]],
																		GNisim$phi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]])
			#sampleobs[a]<-rbinom(1,Catchobs[a],
			#						GNisim$phi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]])
	
			if(sampleobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]>0){

				#observed sample rate
				phiobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-sampleobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]/Catchobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]
	
				
				sampl<-GNisim$catch[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]*GNisim$phi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]
				#of tahs and age comes into play
	
				mtmp<- obstags$Mij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]
				ag<- obstags$Age[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]
	
				
				#expand sample to account for incomplete decoding rate
				#" true"
				#expanded number of observed tags
				msize<-sum(mtmp)/GNisim$lambda[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]
	
				# hypergeometric distribution for number of tags in catch sample
				msizeobs<-rhyper(1,msize/sampl*Catchobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]],
						(sampl-msize)/sampl*Catchobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]],
						sampleobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]])


				# error for decoding rate
				msizedecoded<-rbinom(1,msizeobs,GNisim$lambda[GNisim$year==yrs[y]&GNisim$strata==stra[ta]])
			
				
				

				if(msizedecoded>0){
					obslambda<-msizedecoded/msizeobs
					propmij<-mtmp/sum(mtmp)
					obsmij<-rmultinom(1,msizedecoded,propmij)
				}else{
					obslambda<-1
					obsmij<-rep(0,length(mtmp))
				}

				#Expanded number of tags representing a catch sample with obs error
				rijobs[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-obsmij/(obslambda*
																phiobs[a]*
																thetaj[match(ag,ags)])
			
			}else{
				rijobs[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-rep(0,length(thetaj[match(ag,ags)]))
				
			
			}	
			
			Tiobs[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-sum(rijobs[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]])
			yrgi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-GNisim$year[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]
			grpgi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-as.character(GNisim$group[GNisim$year==yrs[y]&GNisim$strata==stra[ta]])

			#grp[obstag$Year==yrs[y]&obstag$timeareastrata==stra[ta]]<-as.character(GNi$group[GNi$year==yrs[y]&GNi$strata==stra[ta]])
			


			rij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-mtmp/(GNisim$lambda[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]*
					GNisim$phi[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]*thetaj[match(ag,ags)])
			Ti[GNisim$year==yrs[y]&GNisim$strata==stra[ta]]<-sum(rij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]])

			
			


		}
	}

	
	length(rij)
	length(Tiobs)

	Tgobs<-aggregate(Tiobs,list(year=yrgi,group=grpgi),sum)
	Tyear<-aggregate(Tgobs$x,list(year=Tgobs$year),sum)
	Tgobs$ER<-Tgobs$x/Tyear$x[match(Tgobs$year,Tyear$year)]


	rijsims[,n]<-rijobs
	Tisims[,n]<-Tiobs
	Tgsims[,n]<-Tgobs$x
	ERsims[,n]<-Tgobs$ER

}


myquant<-function(x){return(quantile(x,c(0.025,.5,0.975)))}

ERint<-apply(ERsims,1,myquant)


infotg<-aggregate(Ti,list(year=yrgi,group=grpgi),sum)

infotg$group
Tgobs$group

Tg<-aggregate(Ti,list(year=yrgi,group=grpgi),sum)
Tyear<-aggregate(Tg$x,list(year=Tg$year),sum)
ER<-Tg$x/Tyear$x[match(Tg$year,Tyear$year)]
			


df<-data.frame(year=infotg$year,group=infotg$group,estimate=ER,med=ERint[2,],lower=ERint[1,],upper=ERint[3,])


pa<-ggplot(df, aes(x=year,y=estimate,color=as.factor(group)))
pa<-pa+geom_point(aes(x=year,y=estimate,color=as.factor((group))))
pa<-pa+geom_line(aes(x=year,y=estimate,color=as.factor((group))))
pa<-pa+geom_line(aes(x=year,y=med),color="black")
pa<-pa+geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill=as.factor(group)),alpha=.4)
pa<-pa+ facet_wrap(~group, scales="free")
pa<-pa+ theme_bw(16) +labs(  color = "Strata", fill="Strata")
pa<-pa+ylab("CYER or escapement")
pa


summary(rijsims)
which(is.na(rijsims))



#========================================================================
nrow(GNi)
#start writing and test them here

yrs<-unique(GNi$year)

ags<- unique(obstags$Age)
nsims<-200
rijsims<-matrix(NA,ncol=nsims,nrow=nrow(obstags))
Tisims<-matrix(NA,ncol=nsims,nrow=nrow(GNi))
Tgsims<-matrix(NA,ncol=nsims,nrow=nrow(aggregate(GNi$catch,list(year=GNi$year,GNi$group),sum)))
ERsims<-matrix(NA,ncol=nsims,nrow=nrow(aggregate(GNi$catch,list(year=GNi$year,GNi$group),sum)))

for(n in 1:nsims){

	Catchobs<-NULL
	sampleobs<-NULL
	phiobs<-NULL
	lamdaobs<-NULL
	rijobs<-NULL
	rij<-numeric(length=nrow(obstags))
	grp<-NULL
	Ti<-NULL
	Tiobs<-NULL
	yrgi<-NULL
	grpgi<-NULL


	a=1

	for(y in 1:length(yrs)){

		stra<-unique(GNi$strata[GNi$year==yrs[y]])
		g<-unique(GNi$group[GNi$year==yrs[y]])
	
		for(ta in 1:length(stra) ){
	
			#Catchobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-ceiling(GNi$catch[GNi$year==yrs[y]&GNi$strata==stra[ta]]*exp(rnorm(1,mean=0, 
			#														sd=sqrt(GNi$GNi[GNi$year==yrs[y]&GNi$strata==stra[ta]]) )))
			Catchobs[a]<-ceiling(GNi$catch[GNi$year==yrs[y]&GNi$strata==stra[ta]]*exp(rnorm(1,mean=0, 
																	sd=sqrt(GNi$GNi[GNi$year==yrs[y]&GNi$strata==stra[ta]]) )))
			
			#sampleobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-rbinom(1,Catchobs[GNi$year==yrs[y]&GNi$strata==stra[ta]],
			#															GNi$phi[GNi$year==yrs[y]&GNi$strata==stra[ta]])
			sampleobs[a]<-rbinom(1,Catchobs[a],
									GNi$phi[GNi$year==yrs[y]&GNi$strata==stra[ta]])
	
			if(sampleobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]>0){

				phiobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-sampleobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]/Catchobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]
	
				sampl<-GNi$catch[GNi$year==yrs[y]&GNi$strata==stra[ta]]*GNi$phi[GNi$year==yrs[y]&GNi$strata==stra[ta]]
				#of tahs and age comes into play
	
				mtmp<- obstags$Mij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]
				ag<- obstags$Age[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]
	
	
				msize<-sum(mtmp)/GNi$lambda[GNi$year==yrs[y]&GNi$strata==stra[ta]]
	
				msizeobs<-rhyper(1,msize/sampl*Catchobs[GNi$year==yrs[y]&GNi$strata==stra[ta]],
						(sampl-msize)/sampl*Catchobs[GNi$year==yrs[y]&GNi$strata==stra[ta]],
						sampleobs[GNi$year==yrs[y]&GNi$strata==stra[ta]])

				msizedecoded<-rbinom(1,msizeobs,GNi$lambda[GNi$year==yrs[y]&GNi$strata==stra[ta]])
			
				
				

				if(msizedecoded>0){
					obslambda<-msizedecoded/msizeobs
					propmij<-mtmp/sum(mtmp)
					obsmij<-rmultinom(1,msizedecoded,propmij)
				}else{
					obsmij<-rep(0,length(mtmp))
				}


				#jj<-(unique(obstag$Age[obstag$Year==yrs[y]&obstag$timeareastrata==stra[ta]]))

			#for(j in 1:length(jj)){

				rijobs[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-obsmij/(obslambda*
																phiobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]*
																thetaj[match(ag,ags)])
			
			}else{
				rijobs[obstas$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-rep(0,length(thetaj[match(ag,ags)]))
				
			
			}	
			
			Tiobs[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-sum(rijobs[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]])
			yrgi[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-GNi$year[GNi$year==yrs[y]&GNi$strata==stra[ta]]
			grpgi[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-GNi$group[GNi$year==yrs[y]&GNi$strata==stra[ta]]

			#grp[obstag$Year==yrs[y]&obstag$timeareastrata==stra[ta]]<-as.character(GNi$group[GNi$year==yrs[y]&GNi$strata==stra[ta]])
			


			rij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]]<-mtmp/(GNi$lambda[GNi$year==yrs[y]&GNi$strata==stra[ta]]*
					GNi$phi[GNi$year==yrs[y]&GNi$strata==stra[ta]]*thetaj[match(ag,ags)])
			Ti[GNi$year==yrs[y]&GNi$strata==stra[ta]]<-sum(rij[obstags$Year==yrs[y]&obstags$timeareastrata==stra[ta]])

			
			GNi$strata==stra[ta]


		}
	}

	
	length(rij)
	length(Tiobs)

	Tgobs<-aggregate(Tiobs,list(year=yrgi,group=grpgi),sum)
	Tyear<-aggregate(Tgobs$x,list(year=Tgobs$year),sum)
	Tgobs$ER<-Tgobs$x/Tyear$x[match(Tgobs$year,Tyear$year)]


	rijsims[,n]<-rijobs
	Tisims[,n]<-Tiobs
	Tgsims[,n]<-Tgobs$x
	ERsims[,n]<-Tgobs$ER

}


myquant<-function(x){return(quantile(x,c(0.025,.5,0.975)))}

ERint<-apply(ERsims,1,myquant)


infotg<-aggregate(Ti,list(year=GNi$year,group=GNi$group),sum)


Tg<-aggregate(Ti,list(year=GNi$year,group=GNi$group),sum)
Tyear<-aggregate(Tg$x,list(year=Tg$year),sum)
ER<-Tg$x/Tyear$x[match(Tg$year,Tyear$year)]
			


df<-data.frame(year=infotg$year,group=infotg$group,estimate=ER,med=ERint[2,],lower=ERint[1,],upper=ERint[3,])


p<-ggplot(df, aes(x=year,y=estimate,color=as.factor(group)))
p<-p+geom_point(aes(x=year,y=estimate,color=as.factor((group))))
p<-p+geom_line(aes(x=year,y=estimate,color=as.factor((group))))
p<-p+geom_line(aes(x=year,y=med),color="black")
p<-p+geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill=as.factor(group)),alpha=.4)
p<-p+ facet_wrap(~group, scales="free")
p<-p+ theme_bw(16) +labs(  color = "Strata", fill="Strata")
p<-p+ylab("CYER or escapement")
p

ggsave(paste("../figs/sim_CYER.pdf", sep=""), plot=p)


#==========================================================================
#use functions
