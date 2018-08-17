#========================================================================
#Author: Catarina Wor
#Date: June 29th 2018 
# Function to calculate AEQ 
# based on a spreadsheet originally prepared by: Gary S. Morishima
#========================================================================

#Definitions
#
######### AEQ ##########
#Adult Equivalents (AEQs) represent the probability that a fish of a 
#given age would mature in the absence of fishing.
#


#Q<-list(Mij_df=obstag1,survival=c(0.6,0.7,0.8,0.9),matrate=c(0.02,0.4, 0.7,1))

calc_AEQ<-function(Q){

	with(Q,{

	AEQ<-list()
	myyr<-NULL
	myfishery<-NULL
	yr<-unique(Mij_df$Year)
	
	
	a<-1
	for(y in 1:length(yr)){
		
		fishery<-unique(Mij_df$timeareastrata[Mij_df$Year==yr[y]])


		for(i in 1:length(fishery)){
			age<-unique(Mij_df$Age[Mij_df$Year==yr[y]&Mij_df$timeareastrata==fishery[i]])

			if(length(age)>0){
			
			P<-matrix(NA, ncol=length(age),nrow=length(age))

			for (ti in 1:length(age)){

				for (mi in 1:length(age)){

					if(ti==mi){
							P[mi,ti]<-matrate[age[mi]-1]
					}else{
						if(ti>mi){
							P[mi,ti]<-0.0	
						}else{
							P[mi,ti]<-P[mi-1,ti]/matrate[age[mi-1]-1]*(1-matrate[age[mi-1]-1])*survival[age[mi]-1]*matrate[age[mi]-1]
						}
					}
				}
			}
			AEQ[[a]]<-apply(P,2,sum)
			names(AEQ[[a]])<-age

			myyr[a]<-yr[y]
			myfishery[a]<-fishery[i]
		
			a=a+1
		}
		}
	}

	return(list(AEQ=AEQ,year=myyr,fishery=myfishery))

	})



}

