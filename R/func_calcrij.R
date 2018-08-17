#=============================================================================
#function to calculate catch estimate an variance based on Bernard and Clarke 1996 paper
#Author: Catarina Wor
#date: August 2018
#=============================================================================


calc_rij<-function(B){
	
	with(B,{

	rij<-NULL
	rij_yr<-NULL
	rij_ag<-NULL
	rij_fish<-NULL
	rij_yrfish<-character()
	Gpij<-NULL
	Var_rij<-NULL

	a=1

	for(y in 1:length(yrs)){ 

		ii<-as.character(unique(lambdaphi$strata[lambdaphi$Year==yrs[y]]))

		for(i in 1:length(ii)){ 
			jj<-(unique(obs_tag$Age[obs_tag$Year==yrs[y]&as.character(obs_tag$timeareastrata)==as.character(ii[i])]))

			for(j in 1:length(jj)){

				mtmp<-obs_tag$Mij[obs_tag$timeareastrata==ii[i]&obs_tag$Age==jj[j]&obs_tag$Year==yrs[y]]
				rij[a]<-mtmp/(lambdaphi$lambdai[which(lambdaphi$strata==ii[i]&lambdaphi$Year==yrs[y])]*
					lambdaphi$phi[lambdaphi$strata==ii[i]&lambdaphi$Year==yrs[y]]*thetaj[j])
	
				rij_yr[a]<-yrs[y]
				rij_ag[a]<-jj[j]
				rij_fish[a]<-ii[i]
				rij_yrfish[a]<-as.character(lambdaphi$fishery[lambdaphi$strata==ii[i]&lambdaphi$Year==yrs[y]])

				
				if(mtmp>0){
				Gpij[a]<-(1-unique(lambdaphi$lambdai[lambdaphi$strata==ii[i]&lambdaphi$Year==yrs[y]])*
					unique(lambdaphi$phi[lambdaphi$strata==ii[i]&lambdaphi$Year==yrs[y]])*thetaj[j])/mtmp
				Var_rij[a]<- (rij[a]^2)*(Gpij[a]+GNi[i]-Gpij[a]*GNi[i])
				}else{
					Gpij[a]<-NA
					Var_rij[a]<-NA

				}

				
				#print(paste("y is:",y))
				#print(paste("i is:",i))
				#print(paste("j is:",j))
				#print(a)
				#print(length(jj))
				#print(length(jj))
				a<-a+1
			}
		}
	}

	dfrij<-data.frame(rij=rij,yr=rij_yr,age=rij_ag,strata=rij_fish,fishery=rij_yrfish,Var_rij=Var_rij,Gpij=Gpij)



	return(dfrij)

	})

}


calc_Ti<-function(A){
	
	with(A,{


	uyr<-unique(rij$year)



	Ti<-NULL
	varTi<-NULL
	year<-NULL
	strat<-NULL
	a=1


	for(y in 1:length(uyr)){ 

			ii<-as.character(unique(rij$strata[rij$year==uyr[y]]))

		
		for(i in 1:length(ii)){ 
			
			Ti[a]<-sum(rij$rij[rij$year==uyr[y]&rij$strata==ii[i]])


			tmp<-(rij$rij[rij$year==uyr[y]&rij$strata==ii[i]]%o%rij$rij[rij$year==uyr[y]&rij$strata==ii[i]])*GNi[i]

			varTi[a]<-sum(rij$Var_rij[rij$year==uyr[y]&rij$strata==ii[i]],na.rm=T)+
						2*sum(tmp[lower.tri(tmp)])

			year[a]<-uyr[y]
			strat[a]<-ii[i]

			a=a+1

		}
		

	}

	dfTi<-data.frame(Ti=Ti,var_Ti=varTi,year=year,strata=strat)

	return(dfTi)

	})

}

calc_CYER<-function(Z){
	
	with(Z,{
		
		ER<-NULL
		VarER<-NULL
		yr<-NULL
		ag<-NULL
		str<-NULL

		yrs<-unique(dfrij$year)
		
		a=1
		for(y in 1:length(yrs)){ 

			ii<-as.character(unique(dfrij$strata[dfrij$year==yrs[y]]))
			for(i in 1:length(ii)){ 
			
				if(agespec==TRUE){
				
					jj<-(unique(dfrij$age[dfrij$year==yrs[y]&as.character(dfrij$strata)==as.character(ii[i])]))

					for(j in 1:length(jj)){

						ER[a]<-dfrij$rij[dfrij$strata==ii[i]&dfrij$age==jj[j]&dfrij$year==yrs[y]]/dfTi$Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]

						VarER[a] <- ER[a]^2*(dfrij$Var_rij[dfrij$strata==ii[i]&dfrij$age==jj[j]&dfrij$year==yrs[y]]/
								dfrij$rij[dfrij$strata==ii[i]&dfrij$age==jj[j]&dfrij$year==yrs[y]]^2+
								dfTi$var_Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]/dfTi$Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]^2)

						

								yr[a]<-yrs[y]
								ag[a]<-jj[j]
								str[a]<-ii[i]

								a=a+1
					}
				}else{

				
					ER[a]<-dfTi$Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]/sum(dfTi$Ti[dfTi$year==yrs[y]],na.rm=T)


					VarER[a] <- ER[a]^2*(dfTi$var_Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]/dfTi$Ti[dfTi$strata==ii[i]&dfTi$year==yrs[y]]^2+
						(sum(dfTi$Ti[dfTi$year==yrs[y]],na.rm=T)*CVT)^2/sum(dfTi$Ti[dfTi$year==yrs[y]],na.rm=T)^2)
						
					yr[a]<-yrs[y]
					ag[a]<-NA
					str[a]<-ii[i]

					a=a+1
				}

			}

		}

				

		ans<-data.frame(CYER=ER,varCYER=VarER,CYERhi=ER+sqrt(VarER)*1.96,CYERlo=ER-sqrt(VarER)*1.96,age=ag,strata=str,year=yr)

		return(ans)

	})

}

