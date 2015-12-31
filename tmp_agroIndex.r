source('/home/olivier/Desktop/Codes/xpos-r/ClimateDataTools/ClimFormats/dataRead.r')
source('/home/olivier/Desktop/Codes/GH_csag-uct/PlayingCodes/dateTools.r')


## working with
# From: Shingirai Nangombe <ssnangombe@yahoo.com>
# Date: Wed, Sep 2, 2015 at 4:41 PM
# Subject: Re: in preparation of the CCDA conference
# To: Sabine Homann-Kee Tui <shomannkeetui@gmail.com>
# Dear Sabine
# I hope I find you well. 
#
# The definition of rainfall onset or start of season is when a place receives 20mm of rainfall in 2 to 3 days and there is condition of no dry spell of at least 10 days within the next 30 days.
#
# On average, Nkayi area according to our past data, has its onset rains between 25 November and 1 December. Of late, the season in most parts of the country, Nkayi included, is starting late and ending prematurely hence causing the seasons to become shorter. In Zimbabwe, climatologically it is very common to have a mid season dry spell around 25 January to around the beginning of February however, these days the intra seasonal dry spells are becoming more and longer than in the past.
# I hope this can help you a bit.
# Regards
# _ _ _
# Shingirai S. Nangombe
# Principal Meteorologist 
# Meteorological Services Department
# Cnr Hudson & Bishop Gaul Avenue
# P. O. Box BE 150,
# Belvedere, Harare
# Zimbabwe
# Mob: +263 773 484 952 , +263 712 870 887
# Tel: +263 4 778160
# Fax: +263 4 778161
# Email: ssnangombe@yahoo.com

##### STAT RUNNING WINDOW and wrapping functions
stat_windowSum <- function(data,winWidth,from='c')
{	return(stat_window(data,winWidth,from,'sum'))
}
stat_windowMean <- function(data,winWidth,from='c')
{	return(stat_window(data,winWidth,from,'mean'))
}
stat_window <- function(data,winWidth,from,type)
# data : vector
# windWidth : integer : window width
# from : {'f','c','l'} : recorded in first, center or last day of window
# from : NB : when 'c', actual center if winWidth is odd, centered right if winWidth is even
{
	switch(from,
		'f' = rec <- 1, 			# first day of the window
		'l' = rec <- winWidth,			# last day of the window
		'c' = rec <- floor(winWidth/2)+1,	# centered (closest right) of the window
		{	print('\"from\" takes only \'f\':first or \'c\':centered or \'l\':last',quote=FALSE)
			browser()}
	)

	wStat <- array(NA,dim=length(data))
	wStat[rec:(rec+length(data)-winWidth)] <- 0
	for (winS in 1:(length(data)-winWidth+1)){
		switch( type,
			'sum' = wStat[rec] <- sum(data[winS:(winS+winWidth-1)]),
			'mean' = wStat[rec] <- mean(data[winS:(winS+winWidth-1)])
		)
		rec <- rec +1
	}
return(wStat)
}

##### 366 days per year
vec2table366 <- function(data,year,juld,hemi='n')
{
	# dim[1] : number of years (+1 if south as to start 01-jul)
	# dim[2] : consider adding up NA where no 29-feb
	table <- array	(NA,dim=c((year[length(year)]-year[1]+ifelse(hemi!='s',1,2)),366))	

	y <- 1
	d <- ifelse(hemi!='s',juld[1],(juld[1]+182)%%366) # 01-jul is 183 (1+182)
	for (i in 1:length(data)){
		# add up NA 29-feb where it is non-leap year
		if(!is.leapYear(year[i]) & juld[i]==60){
			table[y,d] <- NA
			d <- d+1
		}
	
		table[y,d] <- data[i]

		d <- d+1
		if (d>dim(table)[2]){
			y <- y +1
			d <- 1
		}
	}
return(table)
}

compute_3days_index <- function(data=nkayi$data,hemi)
{
# sum rain over 3 days running window, on the last day
	sumINtWi <- stat_windowSum(data$rain,3,'l')
	sumINtWi_01<-ifelse(sumINtWi>=20,1,0)
# transform into 366 days
	sumINtWi_table <- vec2table366(sumINtWi_01,data$yyyy,data$juld,hemi)
	for (y in 1:32){	#NA at 242 (hemi=='s')
		if (is.na(sumINtWi_table[y,242])){	
			sumINtWi_table[y,242]<-ifelse((sumINtWi_table[y,241]==1&&sumINtWi_table[y,243]==1),1,0)
	}	}
return(sumINtWi_table)
}

compute_10days_index <- function(data=nkayi$data,hemi)
{
	dayNo <- 10
	noRain <- 4 #mm
	# sum rain over 10 days running window, on the first day
	sumINtWi <- stat_windowSum(data$rain,dayNo,'f')
	drySpell10_01<-ifelse(sumINtWi<noRain,0,1)
	for (i in 1:(length(drySpell10_01)-dayNo+1)){
		drySpell10_01[i] <- ifelse(all(drySpell10_01[i:(i+21)]==1),1,0) #i:i+21 day 1 to day 21 (which shows 21 to 31)
	}
	# transform into 366 days
	sumINtWi_table <- vec2table366(drySpell10_01,data$yyyy,data$juld,hemi)
	for (y in 1:32){	#NA at 242 (hemi=='s')
		if (is.na(sumINtWi_table[y,242])){
			sumINtWi_table[y,242]<-ifelse((sumINtWi_table[y,241]==1&&sumINtWi_table[y,243]==1),1,0)
	}	}
return(sumINtWi_table)
}

compute_first <- function(data01_1,data01_2,operator="and")
{	
	and01 <- data01_1+data01_2
	and01 <- ifelse(and01==2,1,0)
	first01 <- array(0,dim=dim(data01_1))
	for (r in (dim(and01)[1]-30):dim(and01)[1]){	# to avoid first row if hemi='s' 
		first01[r,(which(and01[r,]>0)[1])] <- 1
	}
	fir366 <- apply(first01,2,sum)
return(fir366)
}

plot_climNind <- function(box366,fir366,jpeg=F,desFolder=desF,fName=fN,title=tit)
{
if(jpeg)	jpeg(paste(desFolder,fName,sep='/'),width=900,height=400,quality=100)

	#stat_box1 <- boxplot(box366,range=0,varwidth=T,border='lightblue',xaxt='n')
	stat_box1 <- boxplot(box366,varwidth=T,outline=F,border='lightblue',xaxt='n')
	# occurrence
	lines(fir366,ylim=c(0,10),type='h',xaxt='n',yaxt='n',bty='n',ylab='rainfal total (mm/3days on the last day)',xlab='julian Days',main=tit)
	#axis
	if(hemi=='s'){
		axis(1,	at=c(1,31,62,92,123,153,184,215,244,275,305,336,366),
			labels=c("01-jul","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec","31-jan","29-feb","31-mar","30-apr","31-may","30-jun")
		)
	}else{
		axis(1,	at=c(1,31,60,91,121,152,182,213,244,274,305,335,366),
			labels=c("01-jan","31-jan","29-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
		)
	}
	axis(4,range(0,5))
	mtext("first occurrences",side=4,line=1)
	
if(jpeg)	dev.off()
}

#### main
hemi<-'s'
graphics.off()

desF <- '/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/TestAgroIndex'
bas <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Baselines/ZWNK0XXX.AgMIP')#; fN <- 'ZWNK0XXX_agroInd.jpg'; tit <- 'Nkayi (ZIM) Baseline' 
fuMI <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Futures-mandv-29/ZWNKIAXF.AgMIP')#; fN <- 'ZWNKIAXF_agroInd.jpg'; tit <- 'Nkayi (ZIM) RCP 8.5, mid-century, GCM A (middle)'
fuCW <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Futures-mandv-29/ZWNKIFXF.AgMIP')#; fN <- 'ZWNKIFXF_agroInd.jpg'; tit <- 'Nkayi (ZIM) RCP 8.5, mid-century, GCM F (cool/wet)'
fuCD <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Futures-mandv-29/ZWNKIZXF.AgMIP')#; fN <- 'ZWNKIZXF_agroInd.jpg'; tit <- 'Nkayi (ZIM) RCP 8.5, mid-century, GCM Z (cool/dry)'
fuHD <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Futures-mandv-29/ZWNKIRXF.AgMIP')#; fN <- 'ZWNKIRXF_agroInd.jpg'; tit <- 'Nkayi (ZIM) RCP 8.5, mid-century, GCM R (hot/dry)'
fuHW <- read_AgMIPformat('/home/olivier/Desktop/Wine-shared/Projects/2015-2016_AgMIP2/ZIM-CLIP/ZIM-Futures-mandv-29/ZWNKIDXF.AgMIP')#; fN <- 'ZWNKIDXF_agroInd.jpg'; tit <- 'Nkayi (ZIM) RCP 8.5, mid-century, GCM D (hot/wet)'


#jpeg(paste(desF,'basN5gcms.jpg',sep='/'),width=480,height=480,quality=100)
bmp(paste(desF,'basN5gcms.bmp',sep='/'),res=100)

basN5gcms <- list(bas=bas,fMI=fuMI,fCW=fuCW,fCD=fuCD,fHD=fuHD,fWH=fuHW)
colorScale <- c('black','gray','green','lightblue','red','yellow')
frameTitle <- c('baseline','future middle','future cold/wet','future cold/dry','future hot/dry','future hot/wet')

par(mfcol=c(6,1),oma=c(0,0,0,0),mar=c(2,4,1,2))
# 3 days of rain, on last day
days3_01<-compute_3days_index(basN5gcms[[1]]$data,hemi)
# 10 days of rain, on first day
days10_01<-compute_10days_index(basN5gcms[[1]]$data,hemi)

first_bas<-compute_first(days3_01,days10_01,'and')
pretty_bas<-ifelse(first_bas==0,NA,first_bas)
yLimits <- c(0,4)
xLimits <- c(92,230)

rm(days3_01,days10_01,first_bas)

for(d in 1:length(basN5gcms)){
	plot(pretty_bas,type='h',ylim=yLimits,xlim=xLimits,bty='n',xaxt='n',ylab='# occurrences',xlab='',col=colorScale[1],lwd=2)

	if(d>1){
		# 3 days of rain, on last day
		days3_01<-compute_3days_index(basN5gcms[[d]]$data,hemi)
		# 10 days of rain, on first day
		days10_01<-compute_10days_index(basN5gcms[[d]]$data,hemi)

		first_fut<-compute_first(days3_01,days10_01,'and')
		pretty_fut<-ifelse(first_fut==0,NA,first_fut)
		lines(pretty_fut,type='h',ylim=yLimits,xlim=xLimits,bty='n',xaxt='n',ylab='# occurrences',xlab='',col=colorScale[d],lwd=2)
	}
	
	#axis
	if(hemi=='s'){
		axis(1,	at=c(1,31,62,92,123,153,184,215,244,275,305,336,366),
			labels=c("01-jul","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec","31-jan","29-feb","31-mar","30-apr","31-may","30-jun")
		)
	}else{
		axis(1,	at=c(1,31,60,91,121,152,182,213,244,274,305,335,366),
			labels=c("01-jan","31-jan","29-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
		)
	}
	if(d>1)	text(230,3,labels=frameTitle[1],pos=2,col=colorScale[1])
	text(230,2,labels=frameTitle[d],pos=2,col=colorScale[d])
}

dev.off()
browser()

