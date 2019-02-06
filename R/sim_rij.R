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

summary(catches)
summary(recs)
summary(har_esc)
summary(statweek)
summary(PFMA_codes)

head(catches)
head(recs)
#add month category to catches and recoveries
length(unique(recs$CWT_ESTIMATE[!is.na(recs$RECS_STS_ID)]))
length(unique(res3$CWT_ESTIMATE.y))
length(unique(res3$CWT_ESTIMATE.x))
length(unique(res3$CWT_ESTIMATE.x))



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
obs_tags$group<-PFMA_codes$Classification[match(obs_tags$fishery,PFMA_codes$Strata.name)]
head(obs_tags)

#summarize individual recoveries by STS_ID, fishery,year and time
recssum<-aggregate(recs$OBSERVED_HEADS,list(recs$RECS_STS_ID,recs$RECS_YEAR,recs$STRATA_ID,
										recs$STRATA_NAME,recs$PERIOD_TYPE_ID,recs$PERIOD_ID, recs$PERIOD_NAME),sum)
names(recssum)<-c("STS_ID","Year","STRATA_ID","STRATA_NAME","RECS_PERIOD_TYPE_ID","RECS_PERIOD_ID","PERIOD_NAME","OBSERVED_HEADS" )
recssum$timeareastrata<-paste(recssum$STRATA_NAME,recssum$PERIOD_NAME)

head(recs)

recsrepeat<-aggregate(recs[,"CWT_ESTIMATE"],list(recs$RECS_STS_ID,recs$RECS_YEAR,recs$STRATA_ID,
										recs$STRATA_NAME,recs$PERIOD_TYPE_ID,recs$PERIOD_ID, recs$PERIOD_NAME),unique)


recssum$CWT_ESTIMATE<-(recsrepeat$x)

res2 <- full_join(catches, recssum, by=c('STS_ID'='STS_ID', 'STS_YEAR'='Year','STRATA_ID'='STRATA_ID',
										'STRATA_NAME'='STRATA_NAME','PERIOD_TYPE_ID'='RECS_PERIOD_TYPE_ID',
										'PERIOD_ID'='RECS_PERIOD_ID', 'PERIOD_NAME'='PERIOD_NAME'#,'CWT_ESTIMATE'='CWT_ESTIMATE'
										))
res2$timeareastrata <-paste(res2$STRATA_NAME,res2$PERIOD_NAME)
dim(res2)



#filter for only fisheries in which tags have been recovered.
res3<-res2[which(!is.na(res2$OBSERVED_HEADS)),]
head(res3)
summary(res3)

tottag<-NULL
sample_all<-NULL
for(a in 1:nrow(res3)){
	tottag[a]<-sum(c(res3$LOST_TAGS[a],res3$UNREADABLE_TAGS[a],res3$UNRESOLVED_TAGS[a],res3$KNOWN_TAGS[a]),na.rm=T)
	sample_all[a]<-sum(c(res3$SAMPLE_TOTAL[a],res3$SPORT_MARK_SAMPLE_TOTAL[a]),na.rm=T)
}


res3$CATCH_TOTAL

GNirec<-aggregate(tottag,list(res3$STS_YEAR,res3$timeareastrata,res3$Classification),function(x){return(0.1^2)})
names(GNirec)<-c("year","strata","group","GNi" )
dim(GNirec)
dim(res3)
#join fisheries and escapement data
#dfcat<-data.frame(timeareastrata=c(as.character(res3$timeareastrata),as.character(har_esc$Sex)),STS_YEAR=c(res3$STS_YEAR,har_esc$Spawning_Year))

#process escapement data
#mij=#  n decoded cwt by stratum and age

esctag<-aggregate(har_esc$Total_Number_CWT_Observed[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
		by=list(Age=har_esc$Age[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			timeareastrata=har_esc$Sex[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			Year=har_esc$Spawning_Year[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"]),sum)




ProportionAFCfishStratum<-har_esc$Total_AFC_Sampled_Stratum/har_esc$Total_Number_Fish_Examined_Mark_Status_Stratum 

NumberAFCfishStratum<-har_esc$Total_Number_Fish_Stratum*ProportionAFCfishStratum

####This is the portion that I think is wrong - what is the total sample of CWT fish and what is the total number of CWT escapement.  
SampleRate<-har_esc$Total_Number_AFC_Heads_Sampled_Stratum/(NumberAFCfishStratum)


har_esc[har_esc$Spawning_Year==2009,]

head(har_esc)
names(har_esc)

GNiesc<-aggregate(har_esc$CV_Number_Fish_Stratum^2,list(har_esc$Spawning_Year,har_esc$Sex),unique)
names(GNiesc)<-c("year","strata","GNi" )
GNiesc$group<-"escapement"
GNi<-rbind(GNirec,GNiesc)
#=======================================================================================
#produce quantities to be used in function

obstag<-data.frame(Age=c(obs_tags$Age,esctag$Age),
	Year=c(obs_tags$Year,esctag$Year),
	timeareastrata=c(obs_tags$timeareastrata,as.character(esctag$timeareastrata)),
	Mij=c(obs_tags$Mij,esctag$x)
	)

#decoding rate of CWTs

dec_tags_esc<-aggregate(har_esc$Total_Number_CWT_Observed[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
		by=list(strata=har_esc$Sex[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"],
			Year=har_esc$Spawning_Year[har_esc$Tagcode!="No Pin"&har_esc$Tagcode!="no data"]),sum)
all_tags_esc<-aggregate(har_esc$Total_Number_CWT_Observed,by=list(strata=har_esc$Sex,Year=har_esc$Spawning_Year),sum)
dim(all_tags_esc)

names(har_esc)

lambdai_esc<-data.frame(lambdai=dec_tags_esc$x/all_tags_esc$x, Year=dec_tags_esc$Year,strata=dec_tags_esc$stratum,fishery="escapement",group="escapement")
names(lambdai_esc)


#lambdai_esc[match(har_esc$Spawning_Year,lbyrs)]
#####parei aqui
names(res3)
summary(res3)



lambdai_rec<-aggregate(data.frame(catch_total=res3$"CATCH_TOTAL",lambda=res3$KNOWN_TAGS/tottag,sample_all),by=list(strata=res3$timeareastrata,Year=res3$STS_YEAR,fishery=res3$STRATA_NAME,strata=res3$timeareastrata,group=res3$Classification),unique)
names(lambdai_rec)
lambdai_rec$cv_catch<-0.1

lambdaphi<-rbind(lambdai_rec,lambdai_esc)

dim(lambdaphi)
summary(dfcat)
#phii = Fraction of catch that is inspected for CWT
#names(har_esc)TODO: figure out why sample rate is weird in 2013
phi_esc <- as.numeric(aggregate(SampleRate,list(strata=har_esc$Sex,Year=har_esc$Spawning_Year),unique)$x)
phi_rec <- aggregate(1/res3$CWT_ESTIMATE.x,by=list(strata=res3$timeareastrata,Year=res3$STS_YEAR,fishery=res3$STRATA_NAME),unique)$x

lambdaphi$phi <- c(phi_rec,phi_esc)




#==========================================================================
#use functions

B<-list(obs_tag=obstag, 
	yrs=unique(obstag$Year),
	lambdaphi=lambdaphi,
	thetaj=rep(1, length(unique(obstag$Age))), #assume all fish are tagged
	Gthetaj=rep(0, length(unique(obstag$Age))), #assume % tagged is known
	GNi=GNi) #assume 10% cv of catches

myrij<-calc_rij(B)

#head(myryij)
#summary(myrij)


dim(myrij)


#myryij<-aggregate(myrij[,c("rij","Var_rij")],list(strata=myrij$fishery,year=myrij$yr),mysum)
myryij<-myrij

names(myryij)<-c("rij","year","age","stratatime","finestrata","strata","Var_rij","Gpij" )

A<-list(rij=myryij,
	GNi=GNi) #assume 10% cv of catches


myTi<-calc_Ti(A)

Z<-list(dfrij=myryij,dfTi=myTi,agespec=FALSE)
cyerdf<-calc_CYER(Z)
head(cyerdf)
cyerdf1<-cyerdf#[!(cyerdf$strata=="escapement"),]


p<-ggplot(cyerdf, aes(x=year,y=CYER,color=as.factor(strata)))
p<-p+geom_point(aes(x=year,y=CYER,color=as.factor((strata))))
p<-p+geom_line(aes(x=year,y=CYER,color=as.factor((strata))))
p<-p+geom_ribbon(aes(x=year,ymin=CYERlo,ymax=CYERhi,fill=as.factor(strata)),alpha=.4)
p<-p+ facet_wrap(~strata, scales="free")
p<-p+ theme_bw(16) +labs(  color = "Strata", fill="Strata")
p
ggsave(paste("../figs/treatyBC_CYER.pdf", sep=""), plot=p)



#==========================================================================================================
#high catch CV
#=======================================================================================
GNirecplus<-GNirec
GNirecplus$GNi<-.3^2
GNiplus<-rbind(GNirecplus,GNiesc)

Bh<-list(cat=dfcat,
	obs_tag=obstag, 
	yrs=unique(obstag$Year),
	lambdaphi=lambdaphi,
	thetaj=rep(1, length(unique(obstag$Age))), #assume all fish are tagged
	Gthetaj=rep(0, length(unique(obstag$Age))),
	GNi=GNiplus) #assume 30% cv of catches

myrij_h<-calc_rij(Bh)
head(myrij_h)
#myryij<-aggregate(myrij[,c("rij","Var_rij")],list(strata=myrij$fishery,year=myrij$yr),mysum)
myryij_h<-myrij_h

names(myryij_h)<-c("rij","year","age","stratatime","finestrata","strata","Var_rij","Gpij" )

A_h<-list(rij=myryij_h,
	GNi=GNiplus) #assume 30% cv of catches

myTi_h<-calc_Ti(A_h)

Z<-list(dfrij=myryij_h,dfTi=myTi_h,agespec=FALSE)
cyerdf_h<-calc_CYER(Z)
head(cyerdf)
cyerdf_h1<-cyerdf_h#[!(cyerdf_h$strata=="escapement"),]


cyerdfall<-rbind(cyerdf1,cyerdf_h1)

cyerdfall$error_level<-c(rep(.1, length=nrow(cyerdf1)),rep(.3,length=nrow(cyerdf_h1)))

ph<-ggplot(cyerdfall, aes(x=year,y=CYER,color=as.factor(error_level)))
ph<-ph+geom_point(aes(x=year,y=CYER,color=as.factor((error_level))))
ph<-ph+geom_line(aes(x=year,y=CYER,color=as.factor((error_level))))
ph<-ph+geom_ribbon(aes(x=year,ymin=CYERlo,ymax=CYERhi,fill=as.factor(error_level)),alpha=.4)
ph<-ph+ facet_wrap(~strata, scales="free")
ph<-ph+ theme_bw(16) +labs(  color = "Error level", fill="Error level")
ph









#==========================================================================================================
#NO ESCAPEMENT
#=======================================================================================
#produce quantities to be used in function - no escapement

#join fisheries and escapement data
dfcat1<-data.frame(timeareastrata=c(as.character(res3$timeareastrata)),STS_YEAR=c(res3$STS_YEAR))


obstag1<-data.frame(Age=c(obs_tags$Age),
	Year=c(obs_tags$Year),
	timeareastrata=c(obs_tags$timeareastrata),
	Mij=c(obs_tags$Mij))

#decoding rate of CWTs

rec<-aggregate(res3$KNOWN_TAGS/tottag,by=list(strata=res3$timeareastrata,Year=res3$STS_YEAR,fishery=res3$STRATA_NAME),unique)

lambdaphi1<-data.frame(lambdai=rec$x, Year=rec$Year,strata=rec$strata,fishery=rec$fishery)

#phii = Fraction of catch that is inspected for CWT

lambdaphi1$phi <- c(phi_rec)
#==========================================================================
#use functions

B<-list(cat=dfcat1,
	obs_tag=obstag1, 
	yrs=unique(obstag1$Year),
	lambdaphi=lambdaphi1,
	thetaj=rep(1, length(unique(obstag1$Age))), #assume all fish are tagged
	Gthetaj=rep(0, length(unique(obstag1$Age))),
	GNi=rep(0.1^2,length(unique(obstag1$timeareastrata)))) #assume 10% cv of catches

myrij1<-calc_rij(B)

#myryij<-aggregate(myrij[,c("rij","Var_rij")],list(strata=myrij$fishery,year=myrij$yr),mysum)
myryij1<-myrij1

names(myryij1)<-c("rij","year","age","stratatime","strata","Var_rij","Gpij" )

A<-list(rij=myryij1,
	GNi=rep(0.1^2,length(unique(myryij1$strata)))) #assume 10% cv of catches


myTi1<-calc_Ti(A)

Z<-list(dfrij=myryij1,dfTi=myTi1,agespec=FALSE,CVT=.1)
cyerdf0<-calc_CYER(Z)
head(cyerdf)
cyerdf_n1<-cyerdf0[!(cyerdf0$strata=="escapement"),]


p1<-ggplot(cyerdf_n1, aes(x=year,y=CYER,color=as.factor(strata)))
p1<-p1+geom_point(aes(x=year,y=CYER,color=as.factor((strata))))
p1<-p1+geom_line(aes(x=year,y=CYER,color=as.factor((strata))))
p1<-p1+geom_ribbon(aes(x=year,ymin=CYERlo,ymax=CYERhi,fill=as.factor(strata)),alpha=.4)
p1<-p1+ facet_wrap(~strata,nco=3)
p1<-p1+ theme_bw(16) +labs(  color = "Strata", fill="Strata")
p1
ggsave(paste(figs_dir,"no_esc_CYER.pdf", sep=""), plot=p1)









#no escapement - high catch cv

#join fisheries and escapement data
dfcat1<-data.frame(timeareastrata=c(as.character(res3$timeareastrata)),STS_YEAR=c(res3$STS_YEAR))


obstag1<-data.frame(Age=c(obs_tags$Age),
	Year=c(obs_tags$Year),
	timeareastrata=c(obs_tags$timeareastrata),
	Mij=c(obs_tags$Mij))

#decoding rate of CWTs

rec<-aggregate(res3$KNOWN_TAGS/tottag,by=list(strata=res3$timeareastrata,Year=res3$STS_YEAR,fishery=res3$STRATA_NAME),unique)

lambdaphi1<-data.frame(lambdai=rec$x, Year=rec$Year,strata=rec$strata,fishery=rec$fishery)

#phii = Fraction of catch that is inspected for CWT
#names(har_esc)TODO: figure out why sample rate is weird in 2013

lambdaphi1$phi <- c(phi_rec)
#==========================================================================
#use functions

B_nh<-list(cat=dfcat1,
	obs_tag=obstag1, 
	yrs=unique(obstag1$Year),
	lambdaphi=lambdaphi1,
	thetaj=rep(1, length(unique(obstag1$Age))), #assume all fish are tagged
	Gthetaj=rep(0, length(unique(obstag1$Age))),
	GNi=rep(0.3^2,length(unique(obstag1$timeareastrata)))) #assume 10% cv of catches

myrij_nh1<-calc_rij(B_nh)

#myryij<-aggregate(myrij[,c("rij","Var_rij")],list(strata=myrij$fishery,year=myrij$yr),mysum)
myryij_nh1<-myrij_nh1

names(myryij_nh1)<-c("rij","year","age","stratatime","strata","Var_rij","Gpij" )

A_nh<-list(rij=myryij_nh1,
	GNi=rep(0.3^2,length(unique(myryij_nh1$strata)))) #assume 10% cv of catches


myTi_nh1<-calc_Ti(A_nh)

Z<-list(dfrij=myryij_nh1,dfTi=myTi_nh1,agespec=FALSE,CVT=.3)
cyerdf0_nh<-calc_CYER(Z)
cyerdf_nh1<-cyerdf0_nh[!(cyerdf0_nh$strata=="escapement"),]

cyerdfall1<-rbind(cyerdf_n1,cyerdf_nh1)

cyerdfall1$error_level<-c(rep(.1, length=nrow(cyerdf_n1)),rep(.3,length=nrow(cyerdf_nh1)))


p1<-ggplot(cyerdfall1, aes(x=year,y=CYER,color=as.factor(error_level)))
p1<-p1+geom_point(aes(x=year,y=CYER,color=as.factor((error_level))))
p1<-p1+geom_line(aes(x=year,y=CYER,color=as.factor((error_level))))
p1<-p1+geom_ribbon(aes(x=year,ymin=CYERlo,ymax=CYERhi,fill=as.factor(error_level)),alpha=.4)
p1<-p1+ facet_wrap(~strata,nco=3)
p1<-p1+ theme_bw(16) +labs(  color = "Error level", fill="Error level")
p1
ggsave(paste(figs_dir,"no_esc_CYER.pdf", sep=""), plot=p1)


