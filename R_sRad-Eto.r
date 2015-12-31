##
 # FILE R_sRad-Eto.r
 # AUTHOR olivier crespo
 # DATE 2015
 ###############################################################################

##
 # around compute_radn_1day
 ###############################################################################
compute_radn <- function(data,station,inland=NULL)
{	
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station",quote=FALSE);
		stop();
	}

	for (line in 1:dim(data$year)){

		radn<-compute_radn_1day(data$julDay[line],data$tmx[line],data$tmn[line],station$lat,station$alt,inland);
		
		if(line==1){
			sRad <- array(radn$solarRad[1],dim=1);
			eRad <- array(radn$extraRad[1],dim=1);
			aTra <- array(radn$atmosTra[1],dim=1);
		}else{
			sRad <- array(c(sRad,radn$solarRad[1]),dim=dim(sRad)+1);
			eRad <- array(c(eRad,radn$extraRad[1]),dim=dim(eRad)+1);
			aTra <- array(c(aTra,radn$atmosTra[1]),dim=dim(aTra)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"sRad"=sRad,"eRad"=eRad,"aTra"=aTra);
return(data);
}

##
 # from agriParameters.r
 # around compute_ETo_1Day
 ###############################################################################
compute_ETo <- function(data,fileHead,inland=NULL,arid=NULL)
{	station <- fileHead$station;

	# compute Rs, Ra, Tt
	data <- compute_radn(data,station,inland);

	# compute everything else
	for (line in 1:dim(data$year)){
		ET <- compute_ETo_1Day(data$tmx[line],data$tmn[line],station$alt,data$sRad[line],data$eRad[line],arid);

		if(line==1){
			RHs <- array(ET$RelHum[1],dim=1);
			ETo <- array(ET$ETo[1],dim=1);
		}else{
			RHs <- array(c(RHs,ET$RelHum[1]),dim=dim(RHs)+1);
			ETo <- array(c(ETo,ET$ETo[1]),dim=dim(ETo)+1);
		}
	}
	
## a and c aridity parameters routine
	meanRH <- sum(RHs,na.rm=TRUE)/(length(RHs)-length(RHs[is.na(RHs)]));
#	posteriori_a <- 4.5 * exp(-0.1 * meanRH);
	posteriori_c <- 0.724 - 0.004 * meanRH;
	doItAgain <- 0;
	if(arid>1 && arid <5){
		if (posteriori_c <= ET$cArray[arid-1])	doItAgain <- -1;
		if (posteriori_c >= ET$cArray[arid+1])	doItAgain <- +1;
	}

## Aridity Index (AI)
	# according to the definition provided by the United Nations Convention to Combat Desertification (UNCCD)
	# AI = ratio of mean annual precipitation to mean annual potential evapotranspiration
	meanAnnRain <- sum(data$ppt)/(as.numeric(format(fileHead$period$end,"%Y"))-as.numeric(format(fileHead$period$start,"%Y")));
	meanAnnETo <- sum(ETo)/(as.numeric(format(fileHead$period$end,"%Y"))-as.numeric(format(fileHead$period$start,"%Y")));
	AI <- meanAnnRain/meanAnnETo;
	if(is.na(AI)){
		AI <- "NA";
	}else{
		if (0<=AI && AI<0.05)	AI <- "Hyper-Arid";
		if (0.05<=AI && AI<0.2)	AI <- "Arid";
		if (0.2<=AI && AI<0.5)	AI <- "Semi-Arid";
		if (0.5<=AI && AI<0.65)	AI <- "Dry Sub-Humid";
		if (0.65<=AI)		AI <- "Humid";
	}
	
	data <- list(	"tmn"=data$tmn,
			"tmx"=data$tmx,
			"ppt"=data$ppt,
			"julDay"=data$julDay,
			"year"=data$year,
			"sRad"=data$sRad,
			"ETo"=ETo,
			"AI"=AI, "doItAgain"=doItAgain, "arid"=arid
		);

return(data);
}

 ###############################################################################
 #
 # BELOW : FOR ONE DAY ONLY, loops are available above
 #
 # 1. SOLAR RADIATION ESTIMATION (+ EXTRATERRESTRIAL RADIATION + ATMOSPHERIC TRANSMISSIVITY)
 # 2. ETo ESTIMATION (+ RELATIVE HUMIDITY)
 # supp. RADIATION AND ETo ESTIMATION original mark's code 
 #
 ###############################################################################

##
 # SOLAR RADIATION ESTIMATION
 # (+ EXTRATERRESTRIAL RADIATION + ATMOSPHERIC TRANSMISSIVITY)
 ###############################################################################
 # > see for most of the calculations
 # @article{richard_g._allen_crop_1998,
 #	title = {Crop evapotranspiration - Guidelines for computing crop water requirements},
 #	volume = {56},
 #	url = {http://www.fao.org/docrep/x0490e/x0490e00.htm#Contents},
 #	journal = {{FAO} Irrigation and drainage papers},
 #	author = {Richard G. Allen and Luis S. Pereira and Dirk Raes and Martin Smith},
 #	year = {1998}
 # },
 # > see for solar radiation# @article{rosalind_a._ball_evaluation_2004,
 # 	title = {Evaluation of Solar Radiation Prediction Models in North America},
 #	volume = {96},
 #	url = {https://www.agronomy.org/publications/aj/abstracts/96/2/391},
 #	doi = {10.2134/agronj2004.0391},
 #	number = {2},
 #	journal = {Agronomy Journal},
 #	author = {Rosalind A. Ball and Larry C. Purcell and Sean K. Carey},
 #	year = {2004},
 #	pages = {391--397}
 # },
 ###############################################################################
compute_radn_1day <- function(julDay,tmx,tmn,staLat,staAlt,inland)
{	
	Gsc <- 0.0820;					# solar constant = 0.0820 MJ.m^(-2).min^(-1)
	phi <- pi*staLat/180;				# latitude [rad] (N.B. South lat should be negative)
	J <- julDay;					# julian day of the year

	delta <- 0.409*sin((2*pi*J/365)-1.39);		# solar decimation [rad]
	Dr <- 1+0.033*cos(2*pi*J/365);			# inverse relative distance Earth-Sun
		
	Ws <- acos(-tan(phi)*tan(delta)); 		# sunset hour angle [rad]
		
	# Extraterrestrial radiation for daily periods [MJ.m^(-2).day^(-1)]
	Ra <- (24*60/pi)*Gsc*Dr*(Ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(Ws));
		
	Krs <- ifelse(inland,0.16,0.19);		# Krs in [0.1,1.2] for example 0.16 inland, 0.19 coastal
	# estimate of the atmospheric transmissivity
	Tt <- Krs *(1+2.7*10^(-5)*staAlt)*sqrt(abs(tmx-tmn));

	# Solar radiation at earth's surface [MJ.m^(-2).day^(-1)]
	Rs <- Ra*Tt;

return(list("solarRad"=Rs,"extraRad"=Ra,"atmosTra"=Tt));
}

##
 # ETo ESTIMATION (+ RELATIVE HUMIDITY)
 ###############################################################################
 # > see for most of the calculations
 # @article{richard_g._allen_crop_1998,
 # 	title = {Crop evapotranspiration - Guidelines for computing crop water requirements},
 #	volume = {56},
 #	url = {http://www.fao.org/docrep/x0490e/x0490e00.htm#Contents},
 #	journal = {{FAO} Irrigation and drainage papers},
 #	author = {Richard G. Allen and Luis S. Pereira and Dirk Raes and Martin Smith},
 #	year = {1998}
 # },
 # @techreport{delobel_review_2009,
 #	type = {{FAO} technical report},
 #	title = {Review of {ETo} calculation methods and software},
 #	author = {François Delobel},
 #	year = {2009}
 # },
 # @article{c._h._b._priestley_assessment_1972,
 # 	title = {On the Assessment of Surface Heat Flux and Evaporation Using {Large-Scale} Parameters},
 #	volume = {100},
 #	number = {2},
 #	journal = {Monthly Weather Review},
 #	author = {C. H. B. {PRIESTLEY} and R. J. {TAYLOR}},
 #	year = {1972},
 #	pages = {81--92}
 # }
 # @article{j._l._steiner_lysimetric_1991,
 #	title = {Lysimetric Evaluation of Daily Potential Evapotranspiration Models for Grain Sorghum},
 #	volume = {83},
 #	doi = {agronj1991.00021962008300010055x},
 #	number = {1},
 #	journal = {Agronomy Journal},
 #	author = {J. L. Steiner and T. A. Howell and A. D. Schneider},
 #	year = {1991},
 #	pages = {240--247}
 # }
 # @article{f._castellv_methods_1997,
 #	title = {Methods for estimating vapor pressure deficit at a regional scale depending on data availability},
 #	volume = {87},
 #	doi = {10.1016/S0168-1923(97)00034-8},
 #	number = {4},
 #	journal = {Agricultural and Forest Meteorology},
 #	author = {F. Castellví and P. J. Perez and C. O. Stockle and M. Ibañez},
 #	year = {1997},
 #	pages = {243--252}
 # }
 # good summary in here: http://www.apesimulator.it/help/models/evapotranspiration/
 ###############################################################################
compute_ETo_1Day <- function(tmx,tmn,staAlt,sRad,eRad,arid)
{
	# calculation of psychometric constant
	Tmean <- (tmx+tmn)/2;
	Po <- 101.3; alto <- 0; g <- 9.807; R <- 287; a1 <- 0.0065;
	Tko <- 273.15+Tmean;
	P <- Po*((Tko-a1*(staAlt-alto))/Tko)^(g/(a1*R));		# atmospheric pressure
	Cp <- 0.001013;							# specific heat at constant pressure, 1.013.10^(-3) [MJ.kg^(-1).°C^(-1)]
	epsilon <- 0.622;						# ratio molecular weight of water vapour/dry air = 0.622
	lambda <- 2.501-(0.002361*Tmean);				# latent heat of vaporization [MJ.kg^(-1)]
	psychCon <- Cp*P/(epsilon*lambda);				# psychrometric constant [kPa.°C^(-1)]

	# calculation of Slope of saturation vapour pressure curve
	Tdew <- (tmn*0.52) + (0.6*tmx) - (0.009*(tmx^2)) - 2;		# see {delobel_review_2009}
	eTmin <- 0.6108*exp(17.27*tmn/(tmn+237.3));			# min saturation vapour pressure [kPa]
	eTmax <- 0.6108*exp(17.27*tmx/(tmx+237.3));			# max saturation vapour pressure [kPa]
	ea <- 0.6108*exp(17.27*Tdew/(Tdew+237.3));			# actual vapour pressure [kPa]
	eTmean <- es <- (eTmax+eTmin)/2;				# mean saturation vapour pressure [kPa] - Using mean air temperature instead of daily minimum and maximum temperatures results in lower estimates for the mean saturation vapour pressure. The corresponding vapour pressure deficit (a parameter expressing the evaporating power of the atmosphere) will also be smaller and the result will be some underestimation of the reference crop evapotranspiration. Therefore, the mean saturation vapour pressure should be calculated as the mean between the saturation vapour pressure at both the daily maximum and minimum air temperature.
	slopeVap <- 2049*((eTmin/((tmn+237.3)^2))+(eTmax/((tmx+237.3)^2)));	# slope vapour pressure curve at air temperature T [kPa.°C^(-1)] (to be favoured according to {delobel_review_2009}), otherwise: slopeVap <- 4098*eTmean/((Tmean+237.3)^2);	# In the FAO Penman-Monteith equation, where ∆ occurs in the numerator and denominator, the slope of the vapour pressure curve is calculated using mean air temperature {richard_g._allen_crop_1998}

	# calculation of net radiation
	albedo <- 0.23;							# a green vegetation cover has an albedo of about 0.20-0.25
	Rns <- (1-albedo)*sRad;						# net shortwave radiation [MJ.m^(-2).day^(-1)]
	SteBolCon <- 4.903*10^(-9);					# Stefan-Boltzmann constant [4.903.10^(-9) MJ.K^(-4).m^(-2).day^(-1)]
	Rso <- (0.75+2*10^(-5)*staAlt)*eRad;				# clear-sky solar radiation [MJ.m^(-2).day^(-1)]
	Rnl_1 <- SteBolCon*((tmx+273.15)^4+(tmn+273.15)^4)/2;
	Rnl_2 <- 0.34 - 0.14*sqrt(ea);
	Rnl_3 <- (1.35*ifelse((sRad/Rso)>1,1,sRad/Rso))-0.35;
	Rnl <- Rnl_1 * Rnl_2 * Rnl_3;					# net longwave radiation [MJ.m^(-2).day^(-1)]
	Rn <- Rns -Rnl;							# net radiation at the crop surface [MJ.m^(-2).day^(-1)]

	# Priestley-Taylor coefficient (PTc) and aridity corrections leading to alpha
	PTc <- 1.26;							# Priestley-Taylor coefficient: Small adjustments may be required to PTc in the range 1.2-1.3, but default is 1.26 as the overall mean
	# a and c are arrays such that
	# a[1] and c[1] are the set for (al)most humid conditions,
	# a[5] and c[5] are the set for (al)most arid conditions,
	# a[3] and c[3] are the set for default conditions.
	a <- array(c(0,0.0009,0.0041,0.0184,0.1),dim=5);		# a is ranging from 0 (humid) to 0.1 (arid)
	c <- array(c(0.34,0.39,0.44,0.49,0.54),dim=5);			# c typically ranges between 0.45 and 0.50 from humid to arid
	k <- 0;								# k=0 for daily computations
#	Tc <- 2.24+0.49*(tmx+tmn);					# see {f._castellv_methods_1997}
#	eTc <- 0.6108*exp(17.27*Tc/(Tc+237.3));
#	VPDmax_1 <- eTmax - ea;						# see {f._castellv_methods_1997}
	VPDmax_fin <- (eTmax-eTmin)/(1-a[arid]*(eTmax-eTmin));
#	VPD_1 <- es-ea;
#	VPD_2 <- eTc - eTmin;						# see {f._castellv_methods_1997}
	VPD_fin <- c[arid]*VPDmax_fin+k;
#	alpha_1 <- PTc;							# see {c._h._b._priestley_assessment_1972}
#	alpha_2 <- (1+psychCon/slopeVap)/(1+0.6);			# see {c._h._b._priestley_assessment_1972}
	alpha_fin <- 1+(PTc-1)*1*VPD_fin;				# see {j._l._steiner_lysimetric_1991}

#=> PT	# Priestley-Taylor Potential Evapotranspiration
	G <- 0;								# soil heat flux density [MJ.m^(-2).day^(-1)] - As the magnitude of the day is relatively small, it may be ignored
	PT <- alpha_fin/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);	# mark's version improved for P, Rnl(_3) and slopeVap

#=> PM	# FAO Penman-Monteith equation for reference evapotranspiration [mm.day^(-1)]
#	# significant sensitivity to arid/humid condition and vegetation height through windspeed
#	# this relation has been produced for : sub-humid, low to moderate wind speed, short vegetation
#	windSpeed <- 2;							# wind speed at 2 m height [m.s^(-1)]  - 2 m/s is used as a temporary estimate - Due to the appearance of windSpeed in both the nominator and denominator of the FAO Penman-Monteith equation, ETo is not highly sensitive to normal ranges of wind speed - N.B. taller is the ground vegetation considered, greater is the sensitivity
#	PM <- (slopeVap*(Rn-G)/lambda+(900*psychCon*windSpeed*VPD_x/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		

#=> HS	# Hargreaves and Samani
#	a <- 0; b <- 1;							# unadjusted version
#	HS <- a+b*0.0023/lambda*(((tmx+tmn)/2)+17.8)*sqrt(abs(tmx-tmn))*eRad;

## AI requirements -> Relative Humidity (RH)
	RH <- 100 * ea / eTmean;

return(list("ETo"=PT,"RelHum"=RH,"cArray"=c));
}

##
 # RADIATION AND ETo ESTIMATION original mark's code
 ###############################################################################
 # for check only
 ###############################################################################
compute_mark <- function(tmin,tmax,lat,alt,jds)
{
     latr <- lat*pi/180
     Tave <- (tmin+tmax)/2
     Tdew <- (tmin*0.52) + (0.6*tmax) - (0.009*(tmax^2)) - 2
     ea <- 0.6108*exp((17.27*Tdew)/(Tdew+237.3))
     lh <- 2.501-(0.002361*Tave)

     P <- ifelse(alt > -90, 101.3*(((293-(0.0065*alt))/293)^5.26), 96)  # approximation without valid alt

     delta <- 0.4093 * sin((2*pi*jds/365)-1.39)
     ws <- acos(-tan(latr)*tan(delta))
     Rsolar <- (118.08/pi) * (1 + (0.033*cos(0.0172*jds))) * ((ws*sin(latr)*sin(delta)) + (cos(latr)*cos(delta)*sin(ws))) 

     alt <- ifelse(alt > -90, alt, 0)

     Rs <-  (1-0.23) * ks * Rsolar # assumes albedo 0.23

     fh <- 0.34 - (0.14 * sqrt(ea))
     fc <- (1.35*(ks * Rsolar/(0.75*Rsolar))) - 0.35
     Rl <- -1 * fc * fh * 4.903e-9 * ((tmax+273.15)^4 + (tmin+273.15)^4) / 2

     Rn <- (Rs + Rl) / lh

     gamma <- (0.001013 * P)/(0.622*lh)
     eTmax <- 0.6108*exp((17.27*tmax)/(tmax+237.3))
     eTmin <- 0.6108*exp((17.27*tmin)/(tmin+237.3))
     es <- (eTmax + eTmin)/2
     s <- 4098*es/((Tave+237.3)^2)

# Definition of VPD ???
#     VPD <- es - ea

# VPD calculated via a critical temperature (from Italian web site)
#     Tc <- 2.24 + (0.49 * (tmax + tmin))
#     eTc <- 0.6108*exp((17.27*Tc)/(Tc+237.3))
#     VPD <- eTc - eTmin

# Recommended VPD from agrometshell
     a <- 0.04  # should vary between 0 (humid) and 0.08 (very arid)
     VPDmax <- (eTmax - eTmin) / (1 - (a * (eTmax - eTmin)))
     VPD <- 0.475 * VPDmax

     PTc <- 1.30 # should be 1.26-1.30

     pet <- (1+((PTc-1)*VPD))*s*Rn/(s+gamma)
     fPTpet <- pet
return(pet)
}
