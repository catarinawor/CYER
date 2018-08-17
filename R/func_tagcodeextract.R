#====================================================
#extract info from tag code
#Author Catarina Wor
#Date July 2018 
#====================================================





tagextract<-function(tcImp, sampleyr, tctab_id, tctab_by,tctab_stk){

	#tcImp - tag code of tag recovered
	#sampleyr - year of recovery for all tcImp
	#tctab_id - list of available tag codes
	#tctab_by - brood year for all tctab_id
	#tctab_stk - stock id for all tctab_id


	#Extract tag information from tag code 
	BroodYear<-NULL
	Age<-NULL
	Stock<-character(length(tcImp))

	for(y in 1:length(tcImp)){
		if(!is.na(tcImp[y])){
			if(sum(tctab_id==(as.character(tcImp[y])))==1){
				BroodYear[y]<-tctab_by[which(tctab_id==as.character(tcImp[y]))]
				Stock[y]<-as.character(tctab_stk[which(tctab_id==as.character(tcImp[y]))])
				Age[y]<-sampleyr[y]-BroodYear[y]
			}else{
				if(sum(tctab_id==as.character(tcImp[y]))>1){
					BroodYear[y]<-tctab_by [which(tctab_id==as.character(tcImp[y]))][1]
					Stock[y]<-as.character(tctab_stk[which(tctab_id==as.character(tcImp[y]))][1])
					Age[y]<-sampleyr[y]-BroodYear[y]
				}else{
					BroodYear[y]<-NA
					Stock[y]<-NA
					Age[y]<-NA
				}
			}
		}else{
			BroodYear[y]<-NA
			Stock[y]<-NA
			Age[y]<-NA
		}
	}

	return(list(BroodYear=BroodYear,
		Stock=Stock,
		Age=Age))
}
