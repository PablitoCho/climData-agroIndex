##
 # FILE climAgro.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

##
 # COMPUTE tav AND amp APSIM CONSTANTS
 # annual average ambient temperature (TAV)
 # annual amplitude in mean monthly temperature (AMP)
 ###############################################################################
 # > ref
 # http://www.apsim.info/Wiki/public/Upload/OtherProducts/tav_amp.pdf
 # results confirmed in face of the tav_amp.exe dos application
 ###############################################################################
agro_tavamp <- function(metD)
{
	# init
	dMean <- (metD$data$tmin+metD$data$tmax)/2
	monthlyMean <- array(0,dim=c(12,5))
	yearlyAMP <- array(NA,dim=(metD$data$yyyy[length(metD$data$yyyy)]-metD$data$yyyy[1]+1))

	# want only full years
	lastIncomplete <- FALSE;
	line <-1
	while(metD$data$mm[line]!=1) line <-line+1

	year <- metD$data$yyyy[line]
	for (l in line:length(metD$data$yyyy)){
		
		# yearly AMP of last completed year
		if (metD$data$yyyy[l]!=year){
			yearlyAMP[year-metD$data$yyyy[1]+1] <- max(monthlyMean[,3]/monthlyMean[,4])-min(monthlyMean[,3]/monthlyMean[,4])
			year <- metD$data$yyyy[l]
			monthlyMean[,3] <- 0
			monthlyMean[,4] <- 0
			if(year==metD$data$yyyy[length(metD$data$yyyy)]){
				if(metD$data$mm[length(metD$data$mm)]!=12 || metD$data$dd[length(metD$data$dd)]!=31)	break
			}
		}

		# monthly mean (col 1&2 used for TAV, 3&4 for amp)
		month <- metD$data$mm[l]
		if (!is.na(dMean[l])){ # avoid NAs
			monthlyMean[month,1] <- monthlyMean[month,1]+dMean[l]
			monthlyMean[month,2] <- monthlyMean[month,2]+1
			monthlyMean[month,3] <- monthlyMean[month,3]+dMean[l]
			monthlyMean[month,4] <- monthlyMean[month,4]+1
		}
	}
	
	# finalise computation
	yearlyAMP[year-metD$data$yyyy[1]+1] <- max(monthlyMean[,3]/monthlyMean[,4],na.rm=T)-min(monthlyMean[,3]/monthlyMean[,4],na.rm=T)
	monthlyMean[,5] <- monthlyMean[,1]/monthlyMean[,2]

	# check for incomplete last year, if incomplete, max-min will be 'Inf'
	if(any(is.infinite(yearlyAMP)))	yearlyAMP <- yearlyAMP[!is.infinite(yearlyAMP)]

	amp <- ifelse(all(is.na(yearlyAMP)),NA,mean(yearlyAMP,na.rm=T))
	tav <- ifelse(all(is.na(monthlyMean[,5])),NA,mean(monthlyMean[,5]))
	if(!is.na(amp) && (amp<0 || amp>50))	{
		print("# WARNING: amp is not in the APSIM range allowed (0>amp>50)",quote=FALSE)
		browser()
	}
	if(!is.na(tav) && (tav<0 || tav>50))	{
		print("# WARNING: tav is not in the APSIM range allowed (0>tav>50)",quote=FALSE)
		browser()
	}

	metD$clim$amp <- round(amp,2)
	metD$clim$tav <- round(tav,2)

return(metD)
rm(dMean,yearlyAMP,monthlyMean,year,line,l,month,amp,tav)
}

