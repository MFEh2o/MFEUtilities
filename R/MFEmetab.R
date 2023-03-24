# MFE metabolism function

## Support Functions
#' Floor Mins
#' Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
#' @param dataIn A data frame
#' @param timeStep Time interval between DO measurements, minutes; defaults to 10 minutes
#' @return A vector
#' @export
floorMins <- function(dataIn, timeStep){
  #Pull out datetime column and name it x
  x <- dataIn$datetime
  nRows <- length(x)
  #Truncate each datetime to hour; convert to class numeric
  floorHour <- as.POSIXct(trunc(x[1:nRows],"hour"),tz="America/Chicago")
  floorNumeric <- as.numeric(floorHour)
  #Create sequence from floorNumeric to next hour by timeStep (in seconds)
  seqSec <- seq(0,3600,60*timeStep)
  #Create matrix where each row is floorNumeric + the elements of seqSec
  matSec <- matrix(rep(seqSec,nRows),nrow=nRows,byrow=T)
  matSec <- floorNumeric + matSec
  #Calculate abs(time difference) between each element of x and the timeStep intervals
  difs <- abs(as.numeric(x) - matSec)
  #Find the minimum absolute difference in each row and select the corresponding time from matSec
  whichMin <- apply(difs,1,which.min)
  rowNames <- as.numeric(rownames(data.frame(whichMin)))
  matIndex <- (whichMin-1)*nRows + rowNames
  matSecFlat <- matrix(matSec,ncol=1)
  outTime <- as.POSIXct(matSecFlat[matIndex],origin="1970-01-01",tz="America/Chicago")
  #Return outTime
  return(outTime)
}

#' Find Not Dup Rows
#' Function to find duplicate datetime stamps. Returns the indices of rows where the datetime is NOT a duplicate of the datetime in a previous row. 
#' @param dataIn a data frame
#' @return A vector, indices of rows where the datetime is NOT a duplicat of the datetime in a previous row.
#' @export
findNotDupRows <- function(dataIn){
  #dataIn<-eval(parse(text=dataInName))
  dataInName<-colnames(dataIn)[2]
  #Find duplicated time stamps
  dups <- duplicated(dataIn$datetime)
  #If no duplicated time stamps, notDupRows=all rows in dataIn
  if (all(dups==FALSE)){
    notDupRows <- c(1:dim(dataIn)[1])
  }else{
    #If at least one time stamp is duplicated, warn, and notDupRows is indexes
    #of rows where datetime is not duplicate of the datetime in a previous row
    notDupRows <- which(dups==FALSE)
    nDups <- dim(dataIn)[1]-length(notDupRows)
    print(paste("Warning:",nDups,"rows with duplicate time stamps in",dataInName,"will be removed"))
  }
  #Return dupRows
  return(notDupRows)
}

#' Calculate Z Mix Density
#' Using density instead of temp to indicate mixed layer, (though note that objects in this script are still labeled e.g. 'temps', not 'dens')
#' @param dataIn A density profile. First column is datetime, POSIXct. Second and subsequent columns are density (kg m-3) at depth. Column headers for these columns are e.g. temp0, temp0.5, temp1.0, temp2, temp2.3
#' @param thresh Threshold density change to indicate end of mixed layer. Units (kg m-3) m-1. Defaults to 0.075 (value Jordan Read is using in their project)
#' @return A data frame. First column is datetime, second column is zMix calculated at that datetime.
#' @export
calcZMixDens <- function(dataIn, thresh = 0.075){
  #Useful things
  nObs <- dim(dataIn)[1]
  nCols <- dim(dataIn)[2]
  
  #Extract vector of depths from column names of dataIn
  depths <- as.numeric(substr(colnames(dataIn)[2:nCols],5,10))
  # depths=c(0.1,2.0,3.0,4.0,10.0)
  
  #Set up structure to hold calculated metaDepth for each time point
  metaDepthOut <- data.frame(datetime=dataIn$datetime,zMix=rep(NA,nObs))
  
  #Calculate zMix for each time in dataIn
  for (i in 1:nObs){
    #If all temp data at this time point are NA, return NA for zMix at this time
    if (all(is.na(dataIn[i,2:nCols]))){
      metaDepthOut[i,2] <- NA
      next
    }
    
    #Get vectors of depths and temps, excluding cols where temp is NA
    temps <- unlist(dataIn[i,2:nCols])
    DEPTHS <- depths[!(is.na(temps))]
    TEMPS <- temps[!(is.na(temps))]
    
    #Find change in depth, change in temp, and change in temp per depth
    dz <- diff(DEPTHS)
    dT <- diff(TEMPS)
    dTdz <- dT/dz
    
    #If no rate of change exceeds thresh, return max depth for zMix at this time
    if (all(dTdz<thresh)){
      metaDepthOut[i,2] <- max(depths)
      next
    }
    
    #Identify first depth where rate of change exceeds thresh
    whichFirstBigChange <- min(which(dTdz >= thresh))
    metaDepth <- mean(c(DEPTHS[whichFirstBigChange],DEPTHS[(whichFirstBigChange+1)]))
    
    #Save result to metaDepthOut
    metaDepthOut[i,2] <- metaDepth
  }
  
  #Return metaDepthOut
  return(metaDepthOut)
}



#Inputs
# dataIn:     A data.frame with two columns, first is "datetime", second is data
# maxLength:  Maximum length of NA string that you are willing to interpolate across.
#             NOTE that this is given in minutes
# timeStep:   The time step of the data

#' Fill Holes
#' Linearly interpolate values for strings (up to specified length) of missing data. Created by CTS, 31 July 2009.
#' @param dataIn: A data frame with two columns, first is "datetime", second is data
#' @param maxLength: Maximum length of NA string that you are willing to interpolate across. NOTE that this is given in minutes.
#' @param timeStep The time step of the data.
#' @return A data frame with same format as dataIn, but with values interpolated.
#' @export
fillHoles <- function(dataIn, maxLength, timeStep){
  
  #Number of rows in dataIn
  nObs <- dim(dataIn)[1]
  
  #Express maxLength as number of time steps instead of number of minutes
  maxLength <- maxLength/timeStep
  
  #Temporarily replace NAs in data with -99999
  whichNA <- which(is.na(dataIn[,2]))
  if(length(whichNA)){
    dataIn[whichNA,2] <- -99999
  }
  #Identify strings of NA (-99999) values
  rleOut <- rle(dataIn[,2])
  which9 <- which(rleOut$values == -99999)
  
  #If no NA strings in data, return dataIn
  if (length(which9) == 0){
    return(dataIn)
  }else{
    #Otherwise, continue
    
    #Interpolate valus for each string of NA values
    for (i in 1:length(which9)){
      
      #Determine start and end index of string i, and calculate length of string
      if (which9[i]==1){
        stringStart <- 1
      } else{
        stringStart <- 1 + sum(rleOut$lengths[1:(which9[i]-1)])
      }
      
      stringEnd <- sum(rleOut$lengths[1:which9[i]])
      stringLength <- stringEnd-stringStart+1
      
      #Skip to next i if:
      #  -length of string exceeds maxLength,
      #  -stringStart is the first obs in dataIn
      #  -stringEnd is the last obs in dataIn
      if (stringLength > maxLength | stringStart==1 | stringEnd==nObs) next else{
        
        #Linearly interpolate missing values
        interp <- approx(x=c(dataIn[stringStart-1,"datetime"],dataIn[stringEnd+1,"datetime"]),
                         y=c(dataIn[stringStart-1,2],dataIn[stringEnd+1,2]),
                         xout=dataIn[stringStart:stringEnd,"datetime"],
                         method="linear")
        dataIn[stringStart:stringEnd,2] <- interp$y
      }
    }
    
    #Replace any remaining -99999 with NA
    dataIn[which(dataIn[,2]==-99999),2] <- NA
    
    #Return result
    return(dataIn)
  }
}

#' Metab Predix Version 4
#' Return DOHat, residuals, atmFlux, and NLL given par estimates. Created by CTS, 18 August 2009.
#' @param parsIn A vector of parameters.
#' @param dataIn A data frame
#' @return A data frame
#' @export 
metabPredix_v4 <- function (parsIn, dataIn) {
  ## Inputs
  #Unpack parameters and exponentiate to force positive,
  iota <- exp(parsIn[1])
  rho <- exp(parsIn[2])
  DOInit <- exp(parsIn[3])
  
  #Label useful things
  nObs <- dim(dataIn)[1]
  kO2 <- dataIn$kO2
  DOSat <- dataIn$DOSat
  zMix <- dataIn$zMix
  irr <- dataIn$irr
  DOObs <- dataIn$DOObs
  fluxDummy <- dataIn$fluxDummy
  
  ##
  #Calculate predictions and residuals
  
  #Set up output
  DOHat <- rep(NA,nObs)
  atmFlux <- rep(NA,nObs)  
  #Initialize DOHat
  DOHat[1] <- DOInit
  
  #Calculate atmFlux and predicted DO for each time point
  #Fluxes out of lake have negative sign
  for (i in 1:(nObs-1)) {
    atmFlux[i] <- fluxDummy[i] * -kO2[i] * (DOHat[i] - DOSat[i]) / zMix[i]  
    DOHat[i+1] <- DOHat[i] + iota*irr[i] - rho + atmFlux[i]
  }
  
  #Compare observed and predicted DO; calculate residuals and NLL
  #Exclude from calculation any cases where DOObs=NA
  if (any(is.na(DOObs)))
  {
    NAObs <- which(is.na(DOObs))
    res <- DOObs[-NAObs] - DOHat[-NAObs]
  } else
    
  {
    res <- DOObs - DOHat
  }
  
  nRes <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nRes
  NLL <- 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
  
  #Set up output structure
  dataOut <- list(atmFlux=atmFlux,DOHat=DOHat,res=res,NLL=NLL)
  
  #Return dataOut
  return(dataOut)
}

#' Metab Loss, Version 4
#' Estimating Metabolism from DO data. This function calculates the likelihoods; use in conjunction with an optimization routine to minimize NLL. Use this for one day's data at a time. Created by CTS, 29 January 2009. Version 4 created August 2009.
#' @param dataIn A data frame, which includes the following columns: DOObs: DO (mg/L); DOSat: DO at saturation (mg/L); irr: irradiance (PAR) (mmol m-2 s-1) >>>> note that GLEON data may actually be in umol; check; kO2: piston velocity for O2, m (timeStep)-1; zMix: depth of the mixed layer (m); fluxDummy: 0 if zMix is above DO sensor depth (prevents atm flux); 1 otherwise.
#' @param parsIn A vector of parameters. First element is `iota` (primary productivity per unit of PAR; units are (mg L-1 timeStep-1) / (mmol m-2 s-1)). Second element is `rho` (nighttime respiration; units are (mg L-1 (timeStep)-1)). Third element is DOInit (initial DOHat value; units are mg L-1). TimeStep is the number of minutes between observations. `parsIn` is given in log units.
#' @return NLL, which is a scalar (I think? -KG)
#' @export
metabLoss_v4 <- function (parsIn, dataIn) {
  ##Inputs
  #Unpack parameters and exponentiate to force positive
  iota <- exp(parsIn[1])
  rho <- exp(parsIn[2])
  DOInit <- exp(parsIn[3])
  
  #Label useful things
  nObs <- dim(dataIn)[1]
  kO2 <- dataIn$kO2
  DOSat <- dataIn$DOSat
  zMix <- dataIn$zMix
  irr <- dataIn$irr
  DOObs <- dataIn$DOObs
  fluxDummy <- dataIn$fluxDummy
  
  
  ##
  #Calculate predictions and residuals
  
  #Set up output
  DOHat <- rep(NA,nObs)
  atmFlux <- rep(NA,nObs)  
  #Initialize DOHat
  DOHat[1] <- DOInit
  
  #Calculate atmFlux and predicted DO for each time point
  #Fluxes out of lake have negative sign
  for (i in 1:(nObs-1)) {
    atmFlux[i] <- fluxDummy[i] * -kO2[i] * (DOHat[i] - DOSat[i]) / zMix[i]  
    DOHat[i+1] <- DOHat[i] + iota*irr[i] - rho + atmFlux[i]
  }
  
  #Compare observed and predicted DO; calculate residuals and NLL
  #Exclude from calculation any cases where DOObs=NA
  if (any(is.na(DOObs))){
    NAObs <- which(is.na(DOObs))
    res <- DOObs[-NAObs] - DOHat[-NAObs]
  } else{
    res <- DOObs - DOHat
  }
  
  nRes <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nRes
  NLL <- 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
  
  #Return NLL
  return(NLL)
}


#' Bootstrap Metabolism
#' Bootstrapping function for metabolism model; JAZ 2014-11-3; modified from Winslow and GLEON fellows 
#' @param parGuess A guessed parameter to start with? (Not sure this is right -KG)
#' @param dataTemp A data frame
#' @param n Number of bootstrap iterations; default is 1000.
#' @param ar1.resids Logical; whether or not to maintain the ar1 component of the residuals.
#' @param timeStep timeStep of data
#' @return A data frame
#' @export
bootstrap.metab <- function(parGuess, dataTemp, n=1000, ar1.resids=FALSE, timeStep){
  
  n.obs = length(dataTemp$DOObs)
  
  doHat  = metabPredix_v4(optimTemp$par,dataTemp)$DOHat
  resids = dataTemp$DOObs - doHat
  
  #If we are maintaining the ar1 component of the residuals, 
  # we must estimate ar1 coeff and the ar1 residual standard deviation
  if(ar1.resids){
    ar1.lm    = lm(resids[1:n.obs-1] ~ resids[2:n.obs]-1)
    ar1.coeff = ar1.lm$coefficients
    ar1.sd    = sd(ar1.lm$residuals)
  }
  
  #Pre-allocate the result data frame
  result <- data.frame(boot.iter = 1:n,
                       iota = rep(NA,n),
                       rho = rep(NA,n),
                       DOInit = rep(NA,n),
                       covergence = rep(NA,n),
                       nll = rep(NA,n),
                       GPP = rep(NA,n))
  
  for(i in 1:n){
    #Randomize the residuals using one of two methods
    if(ar1.resids){ #residual randomization keeping the ar1 data structure
      simRes = rep(NA, n.obs)
      simRes[1] = sample(resids[!is.na(resids)],1)
      for(j in 2:n.obs){
        simRes[j] = ar1.coeff*simRes[j-1] + rnorm(n=1, sd=ar1.sd)
      }
      
    }else{ #Raw residual randomization
      #Randomize residuals without replacement
      simRes = sample(resids[!is.na(resids)], length(resids), replace=FALSE) 
    }
    
    doSim = doHat + simRes
    
    #Run optim again with new simulated DO signal
    dataBoot<-dataTemp
    dataBoot$DOObs<-doSim
    optimTemp <- optim(parGuess,metabLoss_v4,dataIn=dataBoot)
    
    result[i,2:3] <- exp(optimTemp$par[1:2])*(1440/timeStep) #iota and rho to units of mg O2 L-1 day-1 
    result[i,4] <- exp(optimTemp$par[3]) #initial DO estimate 
    result[i,5] <- optimTemp$convergence  #did model converge or not (0=yes, 1=no)
    result[i,6] <- optimTemp$value #value of nll 
    result[i,7] <- (result[i,2]/(60*60*24))*sum(dataTemp$irr)*timeStep*60 #GPP in units of mg O2 L-1 d-1 
  }
  
  return(result)
}


#' mfeMetab
#' Functionality for generating lake metabolism estimates from MFE database. Putting Solomon metabolism code into a function and adding some options for different K models. Also allows for pulling input data directly from MFE sensor database. WARNING! This function makes some modifications to your R workspace, which isn't best practice for R package functions. I don't have time to fix it now. It sets the system time zone, and it also saves your workspace to an .Rdata file. Use caution. -KG
#' SEJ with code from CTS, JAZ, CRO, and CJT
#' @param lakeID Lake to estimate metabolism for
#' @param minDate First date to estimate metabolism for
#' @param maxDate Last date to estimate metabolism for
#' @param outName Prefix for output files 
#' @param dirDump Directory where outputs should be dumped, e.g. 'C:/GLEON/Acton/Results'. No default value. 
#' @param maxZMix Back up zmix if temp profile data is unavailable; defaults to 4, but should pick a value that makes sense.
#' @param k Model for piston velocity - options are: "cole&caraco", a constant k600 specified by the desired value, or "read" (coming soon)
#' @param fluxDummyToggle Use convention where atmospheric exchange is shut off during microstratification conditions.
#' @param bootstrapping `yes` or `no`. Warning! Will take a while to run if `yes`. Defaults to `no`.
#' @param lat Latitude of lake, decimal degrees, north positive; defaults to UNDERC (46.15)
#' @param elev Elevation of lake surface, m above sea level; defaults to UNDERC (518)
#' @param windHeight Height above lake surface at which wind speed is measured, m; defaults to UNDERC (2)
#' @param timeStep Time interval between DO measurements, minutes; defaults to 10 minutes
#' @param sensorDepth Depth of DO sensor, m; defaults to UNDERC (0.7?)
#' @param outName Text to use in lableing outputs, e.g. 'Acton2008'. Character. No default.
#' @export
mfeMetab <- function(lakeID, minDate, maxDate, outName, dirDump, maxZMix = 8,
                     k = "cole&caraco", fluxDummyToggle = TRUE, 
                     bootstrap = 'no', lat = 46.16, elev = 535, windHeight = 2,
                     timeStep = 10, sensorDepth = 0.7){
  	
	library(LakeMetabolizer)

	#For troubleshooting:
  #maxZMix=8;k="cole&caraco";fluxDummyToggle=TRUE;bootstrap='no';lat=46.16;elev=535;windHeight=2;timeStep=10;sensorDepth=0.7

  # Set environment tz variable to CDT
  Sys.setenv(tz = "America/Chicago")
  
  ########################################
  # Read and organize data from database - assumes database functions have been sourced.
  # DO
  if(lakeID%in%c("EL","WL")){
    rawDOmd <- sensordbTable("DO_CORR", lakeID = lakeID, minDate = minDate, maxDate = maxDate)
    rawDOysi <- sensordbTable("YSI_CORR", lakeID=lakeID, minDate = minDate, maxDate = maxDate)
    rawDO <- rbind(rawDOmd[,c("dateTime","cleanedDO_mg_L")], rawDOysi[,c("dateTime","cleanedDO_mg_L")])
    dataDO <- aggregate(x = as.numeric(rawDO$cleanedDO_mg_L), by = list(rawDO$dateTime), FUN = mean,
                        na.rm = TRUE)
    colnames(dataDO) <- c("datetime", "DO")
  }else{
    rawDO<-sensordbTable("DO_CORR", lakeID = lakeID, minDate = minDate, maxDate = maxDate)
    dataDO<-rawDO[,c("dateTime","cleanedDO_mg_L")]
    colnames(dataDO)<-c("datetime","DO")
  }
  #Water temp at depth of DO sensor
  if(lakeID%in%c("EL","WL")){
    rawSensorTemp<-rbind(rawDOmd[,c("dateTime","cleanedTemp_C")],rawDOysi[,c("dateTime","cleanedTemp_C")])
    dataSensorTemp<-aggregate(x=as.numeric(rawSensorTemp$cleanedTemp_C),by=list(rawSensorTemp$dateTime),
                              FUN=mean,na.rm=TRUE)
    colnames(dataSensorTemp)<-c("datetime","TEMP")
  }else{
    dataSensorTemp<-rawDO[,c("dateTime","cleanedTemp_C")]
    colnames(dataSensorTemp)<-c("datetime","TEMP")
  }
  #Temp profile
  tempChain<-sensordbTable("HOBO_TCHAIN_CORR",lakeID=lakeID,minDate=minDate,maxDate=maxDate)
  tempChain2<-tempChain[,c("dateTime","depth_m","cleanedTemp_C")]
  colnames(tempChain2)=c('datetime','depth_m','temp')
  dataTempProfile<-reshape(tempChain2,timevar="depth_m",idvar="datetime",direction="wide",sep="")
  
  metDataEL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="EL",minDate=minDate,maxDate=maxDate)
  metDataWL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="WL",minDate=minDate,maxDate=maxDate)
  metDataFE<-sensordbTable("HOBO_METSTATION_CORR",lakeID="FE",minDate=minDate,maxDate=maxDate)
  metData=rbind(metDataEL,metDataWL, metDataFE)
  
  #PAR
  #Divide PAR by 1000 to convert from measured units (umol m-2 s-1) to model units (mmol m-2 s-1)
  #made choice to average PAR readings from WL & EL/FE
  dataPAR<-aggregate(x=as.numeric(metData$cleanedPAR_uE_m2_s),by=list(metData$dateTime),
                     FUN=mean,na.rm=TRUE)
  colnames(dataPAR)=c("datetime","PAR")
  dataPAR$PAR <- dataPAR$PAR/1000
  #Wind speed
  dataWind<-aggregate(x=as.numeric(metData$cleanedWindSpeed_m_s),by=list(metData$dateTime),
                      FUN=mean,na.rm=TRUE)
  colnames(dataWind)=c("datetime","WS")
  
  ##
  #Display some info about time grain of measurements
  
  #Print first five time readings for each variable
  firstFiveTimes <- data.frame(DO=dataDO$datetime[1:5],PAR=dataPAR$datetime[1:5],windSpeed=dataWind$datetime[1:5],sensorTemp=dataSensorTemp$datetime[1:5],tempProfile=dataTempProfile$datetime[1:5])
  print('First five time readings for each variable'); print(firstFiveTimes)
  
  #Calculate first differences of time readings, display unique values
  cat('\n','First differences of datetime', '\n')
  difTimesDO <- diff(dataDO$datetime); print(table(difTimesDO))
  difTimesPAR <- diff(dataPAR$datetime); print(table(difTimesPAR))
  difTimesWindSpeed <- diff(dataWind$datetime); print(table(difTimesWindSpeed))
  difTimesSensorTemp <- diff(dataSensorTemp$datetime); print(table(difTimesSensorTemp))
  difTimesTempProfile <- diff(dataTempProfile$datetime); print(table(difTimesTempProfile))
  
  ##
  #Remove rows with duplicate datetime stamps (and warn)
  notDupRows <- findNotDupRows(dataDO)
  dataDO <- dataDO[notDupRows,]
  notDupRows <- findNotDupRows(dataPAR)
  dataPAR <- dataPAR[notDupRows,]
  notDupRows <- findNotDupRows(dataWind)
  dataWind <- dataWind[notDupRows,]
  notDupRows <- findNotDupRows(dataSensorTemp)
  dataSensorTemp <- dataSensorTemp[notDupRows,]
  notDupRows <- findNotDupRows(dataTempProfile)
  dataTempProfile <- dataTempProfile[notDupRows,]

  dataDO$datetime <- floorMins(dataDO,timeStep=timeStep)
  dataPAR$datetime <- floorMins(dataPAR,timeStep=timeStep)
  dataWind$datetime <- floorMins(dataWind,timeStep=timeStep)
  dataSensorTemp$datetime <- floorMins(dataSensorTemp,timeStep=timeStep)
  dataTempProfile$datetime <- floorMins(dataTempProfile,timeStep=timeStep)
  
  #Repeat check for dup rows in case any introduced during floorMins
  notDupRows <- findNotDupRows(dataDO)
  dataDO <- dataDO[notDupRows,]
  notDupRows <- findNotDupRows(dataPAR)
  dataPAR <- dataPAR[notDupRows,]
  notDupRows <- findNotDupRows(dataWind)
  dataWind <- dataWind[notDupRows,]
  notDupRows <- findNotDupRows(dataSensorTemp)
  dataSensorTemp <- dataSensorTemp[notDupRows,]
  notDupRows <- findNotDupRows(dataTempProfile)
  dataTempProfile <- dataTempProfile[notDupRows,]
  
  #Find the latest first time point and the earliest last time point of all the data
  startTime <- max(min(dataDO$datetime),min(dataPAR$datetime),min(dataWind$datetime),min(dataSensorTemp$datetime),min(dataTempProfile$datetime))
  endTime <- min(max(dataDO$datetime),max(dataPAR$datetime),max(dataWind$datetime),max(dataSensorTemp$datetime),max(dataTempProfile$datetime))
  
  #Data.frame with one column "datetime" which is sequence of times at time interval of timeStep, from startTime to endTime
  completeTimes <- data.frame(datetime=seq(startTime,endTime,paste(timeStep,"mins")))
  
  #Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
  dataDO <- merge(completeTimes,dataDO,by="datetime",all.x=T)
  dataPAR <- merge(completeTimes,dataPAR,by="datetime",all.x=T)
  dataWind <- merge(completeTimes,dataWind,by="datetime",all.x=T)
  dataSensorTemp <- merge(completeTimes,dataSensorTemp,by="datetime",all.x=T)
  dataTempProfile <- merge(completeTimes,dataTempProfile,by="datetime",all.x=T)

  ########################################
  #Calculate sunrise, sunset
  
  #Days of year for which to calculate sunrise and sunset
  daysVec <- seq.POSIXt(trunc(startTime,"day"),trunc(endTime,"day"),"1 day")
  day <- as.numeric(format(daysVec,format="%j"))
  
  #Factors to convert degrees to radians and vice versa
  degToRad <- 2*pi/360
  radToDeg <- 180/pi
  
  #Day angle "gamma" (radians). Iqbal 1983 Eq. 1.2.2
  dayAngle <- 2*pi*(day-1)/365
  
  #Declination of the sun "delta" (radians). Iqbal 1983 Eq. 1.3.1
  dec <- 0.006918 - 0.399912*cos(dayAngle) + 0.070257*sin(dayAngle) - 0.006758*cos(2*dayAngle) +  0.000907*sin(2*dayAngle) - 0.002697*cos(3*dayAngle) + 0.00148*sin(3*dayAngle)
  
  #Sunrise hour angle "omega" (degrees). Iqbal 1983 Eq. 1.5.4
  latRad <- lat*degToRad
  sunriseHourAngle <- acos(-tan(latRad)*tan(dec))*radToDeg
  
  #Sunrise and sunset times (decimal hours, relative to solar time) Iqbal 1983 Ex. 1.5.1 - +5 is to put it in America/Chicago
  sunrise <- 12 - sunriseHourAngle/15+5
  sunset <- 12 + sunriseHourAngle/15+5
  # As number of seconds from midnight
  sunrise <- sunrise/24*86400
  sunset <- sunset/24*86400
  # As number of seconds from beginning of year
  sunrise <- sunrise+(day-1)*86400
  sunset <- sunset+(day-1)*86400
  # Convert to POSIXct and round to nearest minute
  yr <- format(daysVec,format="%Y")
  origin <- paste(yr,"01","01",sep="-")
  sunrise <- round(as.POSIXct(sunrise,origin=origin,tz="America/Chicago"),"mins")
  sunset <- round(as.POSIXct(sunset,origin=origin,tz="America/Chicago"),"mins")
  
  # One final note: I found out a while ago that the metabolism code doesn't take daylight 
  # savings time into account.  All the datasets that Chris was getting for GLEON were in 
  # normal time, not daylight savings time.  If you're using CDT times with the pelagic data 
  # you've been running through the codes, you should check the sunrise and sunset times the 
  # code is calculating.  They'll probably be offset by one hour, which would affect the GPP 
  # and R results.  If the data you're inputting is exclusively in daylight savings time (like 
  # our data from the summer), you can just add these two lines of code into script:
  # #
  sunrise <- sunrise + 3600
  sunset <- sunset + 3600
  # #
  # Comment the shit out of those lines so you don't forget about them later.  
  # Those lines would go right before:
  # #Create data.frame with sunrise, sunset times for each day
  # sun <- data.frame(day=daysVec,sunrise,sunset)
  
  # NOT QUITE SURE ABOUT THIS YET...
  #sunrise <- sunrise + 3600
  #sunset <- sunset + 3600
  
  
  #Create data.frame with sunrise, sunset times for each day
  sun <- data.frame(day=daysVec,sunrise,sunset)
  
  
  ########################################
  #Re-trim data sets so that they start at or after first sunrise, and end at last time before last sunrise
  # i.e. lop off partial day at end
  
  #Trim
  startTrim <- min(sun$sunrise)
  endTrim <- max(sun$sunrise)
  dataDO <- dataDO[dataDO$datetime >= startTrim & dataDO$datetime < endTrim,]
  dataPAR <- dataPAR[dataPAR$datetime >= startTrim & dataPAR$datetime < endTrim,]
  dataWind<- dataWind[dataWind$datetime >= startTrim & dataWind$datetime < endTrim,]
  dataSensorTemp <- dataSensorTemp[dataSensorTemp$datetime >= startTrim & dataSensorTemp$datetime < endTrim,]
  dataTempProfile <- dataTempProfile[dataTempProfile$datetime >= startTrim & dataTempProfile$datetime < endTrim,]
  completeTimes <- data.frame(datetime=completeTimes[completeTimes$datetime >= startTrim & completeTimes$datetime < endTrim,],stringsAsFactors=FALSE)
  
  #(Useful later) Vector giving which solar day each time in completeTimes belongs to
  solarDaysBreaks <- unique(sun$sunrise[sun$sunrise <= endTrim])
  solarDaysVec <- cut.POSIXt(completeTimes$datetime,breaks=solarDaysBreaks)
  
  
  ########################################
  #Fill gaps in data
  
  ##
  #DO - do not fill gaps
  
  ##
  #PAR - linearly interpolate gaps up to 60 min long
  dataPAR <- fillHoles(dataPAR,maxLength=60,timeStep=timeStep)
  
  ##
  #sensorTemp - linearly interpolate gaps up to 60 min long
  dataSensorTemp <- fillHoles(dataSensorTemp,maxLength=200,timeStep=timeStep)
  
  ##
  #windSpeed - fill with daily average as long as at least 80% of data are available
  
  #Loop over days
  for (i in 1:length(unique(solarDaysVec))){
    cat(i, " ")
    #Extract data between sunrise on day i and sunrise on day i+1
    timeSlice <- c(sun$sunrise[i], sun$sunrise[i+1])
    dataTemp <- dataWind[dataWind$datetime>=timeSlice[1] & dataWind$datetime<timeSlice[2],]
    
    #Determine total number of observations, and number that are NA
    nTot <- length(dataTemp$WS)
    nNA <- length(which(is.na(dataTemp$WS)))
    
    #If >20% of obs are NA, skip to next i
    if(nrow(dataTemp)>0){
      if (nNA/nTot > 0.20){
        next
      }else{
        #Calculate mean windSpeed and sub in for NA values
        meanSpeed <- mean(dataTemp$WS,na.rm=T)
        naRows <- as.numeric(row.names(dataTemp[is.na(dataTemp$WS),]))
        dataWind$WS[naRows] <- meanSpeed
      }
    }else{
      next
    }
  }
  
  ##
  #tempProfile - linearly interpolate gaps up to 60 min long 
  
  nCols <- dim(dataTempProfile)[2]
  
  #Loop over the columns of dataTempProfile
  for (i in 2:nCols)
  {
    dataTemp <- dataTempProfile[,c(1,i)]
    dataTemp[,2]=as.numeric(dataTemp[,2])
    #***** set maxLength to enable long interpolations or not
    dataTempFilled <- fillHoles(dataTemp,maxLength=1000000,timeStep=timeStep)	
    dataTempProfile[,i] <- dataTempFilled[,2]
  }
  
  ########################################
  #Calculate zMix and fluxDummy
  
  #If temperature measured at only one depth, use maxZMix as zMix at every time
  if (ncol(dataTempProfile) <= 2)
  {
    dataZMix <- data.frame(datetime=dataTempProfile$datetime,
                           zMix=rep(maxZMix,length(dataTempProfile$datetime)))
  } else
    
    #Otherwise calculate zMix from data
  {
    #Convert tempProfile data to density
    #Density of water (kg m-3) as function of temp from McCutcheon (1999)
    #Note there is a different formula if salinity is appreciable; formula below ignores that
    dataDensProfile <- dataTempProfile 
    dataDensProfile[,-1] <- 1000*(1-((dataDensProfile[,-1]+288.9414)/(508929.2*(dataDensProfile[,-1]+68.12963)))*(dataDensProfile[,-1]-3.9863)^2)
    
    #Calc zMix
    dataZMix <- calcZMixDens(dataDensProfile)
  }
  
  #Plot zMix
  maxDepth <- max(as.numeric(substr(colnames(dataTempProfile)[2:nCols],5,10)))
  pdf(file=paste(outName,'zMix.pdf'))
  plot(zMix~datetime,data=dataZMix,ylim=c(maxDepth,0))
  dev.off()
  
  #if using fluxDummy, then...
  if(fluxDummyToggle){
    #Identify when to shut off atmospheric flux
    # If zMix > sensorDepth, then sensor is in mixed layer and fluxDummy = 1
    # If zMix <= sensorDepth, then there is stratification at or above sensor and fluxDummy = 0 -- shut off atmosphere at this time step
    fluxDummy <- as.numeric(dataZMix$zMix>sensorDepth)
  }else{
    fluxDummy<-rep(1,nrow(dataTempProfile))
  }
  
  ########################################
  #Merge data for convenience
  
  #Merge
  data1 <- merge(dataDO,dataPAR,by="datetime",all=T)
  data1 <- merge(data1,dataWind,by="datetime",all=T)
  data1 <- merge(data1,dataSensorTemp,by="datetime",all=T)
  data1 <- merge(data1,dataZMix,by="datetime",all=T)
  
  
  ########################################
  #Report on lengths of NA strings in data
  
  #For each variable in data1, find the length of each run of NA in the data
  
  #Have to temporarily sub in -99999 for NA to get rle() to work as desired
  data1Temp <- as.matrix(data1[,2:6])
  whichNA <- which(is.na(data1Temp))
  data1Temp[whichNA] <- -99999
  
  cat('\n','Lengths of NA strings after fillHoles etc.','\n')
  
  #DO
  rleOut <- rle(data1Temp[,"DO"])
  whichNA <- which(rleOut$values==-99999)
  print('Lengths of DO NA strings')
  print(sort(rleOut$lengths[whichNA]))
  #PAR
  rleOut <- rle(data1Temp[,"PAR"])
  whichNA <- which(rleOut$values==-99999)
  print('Lengths of PAR NA strings')
  print(sort(rleOut$lengths[whichNA]))
  #windSpeed
  rleOut <- rle(data1Temp[,"WS"])
  whichNA <- which(rleOut$values==-99999)
  print('Lengths of windSpeed NA strings')
  print(sort(rleOut$lengths[whichNA]))
  #sensorTemp
  rleOut <- rle(data1Temp[,"TEMP"])
  whichNA <- which(rleOut$values==-99999)
  print('Lengths of sensorTemp NA strings')
  print(sort(rleOut$lengths[whichNA]))
  #zMix
  rleOut <- rle(data1Temp[,"zMix"])
  whichNA <- which(rleOut$values==-99999)
  print('Lengths of zMix NA strings')
  print(sort(rleOut$lengths[whichNA]))
  
  rm(data1Temp)
  
  
  ########################################
  #Calculate DOSat and kO2 at each time step
  
  ##
  #Calculate average atmospheric pressure at elevation of lake
  #Using the 'barometric formula' - see e.g. U.S. Standard Atmosphere 1976 or
  # Jacobs 1999 Atmospheric Chemistry, Eqn 2.9
  #Values of Rstar, g0, M are according to US Standard Atmosphere 1976; could use SI instead
  
  #Constants
  Pb <- 101325        #static pressure, pascals
  Tb <- 288.15        #standard temp, K
  Lb <- -0.0065       #standard temp lapse rate, K m-1
  h <- elev           #elevation above sea level, m
  hb <- 0             #elevation at bottom of atmospheric layer 0, m (note layer 0 extends to 11000 masl)
  Rstar <-  8.31432   #universal gas constant, N m mol-1 K-1 (equiv to J K-1 mol-1)  SI: 8.314472
  g0 <- 9.80665       #acceleration of gravity, m s-1
  M <- 0.0289644      #molar mass of Earth's air, kg mol-1
  
  #Pressure, in Pa (pascals)
  P <- Pb * (Tb/(Tb+Lb*(h-hb)))^(g0*M/(Rstar*Lb))
  # In mmHg
  atmPres <- P*0.00750061683
  
  
  ##
  #Calculate DO saturation
  #Use eqn from Weiss 1970 Deep Sea Res. 17:721-735; simplified since salinity=0
  # ln DO = A1 + A2 100/T + A3 ln T/100 + A4 T/100
  
  #Convert sensorTemp to Kelvin
  sensorTempK <- dataSensorTemp[,2] + 273.15
  
  #Weiss equation
  A1 <- -173.4292;  A2 <- 249.6339;  A3 <- 143.3483;  A4 <- -21.8492
  DOSat <- exp(((A1 + (A2*100/sensorTempK) + A3*log(sensorTempK/100) + A4*(sensorTempK/100))))
  
  #Correction for local average atmospheric pressure
  u <- 10^(8.10765 - (1750.286/(235+dataSensorTemp[,2])))
  DOSat <- (DOSat*((atmPres-u)/(760-u)))   #ml/L
  DOSat <- DOSat/1000                      #L/L
  
  #Convert using standard temperature and pressure. 
  #Similar to calculating saturation DO at STP in ml/L, converting to mg?L (at STP),
  #and then doing the above temperature and pressure conversions.
  R <- 0.082057  #L atm deg-1 mol-1
  O2molWt <- 15.999*2
  convFactor <- O2molWt*(1/R)*(1/273.15)*(760/760) #g/L
  DOSat <- DOSat*convFactor*1000                   #mg/L
  
  ##
  #Calculate kO2 depending on what model is specified
  if(k=="cole&caraco"){
    wp <- 0.15                       #exponent of wind profile power relationship, Smith 1985 Plant, Cell & Environment 8:387-398
    wind10 <- (10/windHeight)^wp * dataWind[,2]
    k600 <- 2.07 + 0.215*wind10^1.7  #k600 in cm hr-1 per Cole and Caraco 1998;
    k600 <- k600*24/100              #k600 in m day-1
    schmidt <- 1800.6 - 120.1*dataSensorTemp[,2] + 3.7818*dataSensorTemp[,2]^2 - 0.047608*dataSensorTemp[,2]^3
    kO2 <- k600*(schmidt/600)^-0.5   #Jahne et al. 87. exp could also be -.67
    kO2 <- kO2*(timeStep/1440)       #change kO2 to units of m/(timeStep*min)
  }else if(k=="read"){
    #### need to work on this sometime -> look at "metabFUnc_Mesocosm_ReadKModel.R
  }else if(is.numeric(k)){
    k600=rep(k,nrow(data1))
    schmidt <- 1800.6 - 120.1*dataSensorTemp[,2] + 3.7818*dataSensorTemp[,2]^2 - 0.047608*dataSensorTemp[,2]^3
    kO2 <- k600*(schmidt/600)^-0.5   #Jahne et al. 87. exp could also be -.67
    kO2 <- kO2*(timeStep/1440)       #change kO2 to units of m/(timeStep*min)
  }
  
  ########################################
  #Fit model to find parameter estimates for each day
  
  ##
  #Set up
  
  #Organize input data
  data2 <- data.frame(datetime=data1$datetime, DOObs=data1$DO, DOSat=DOSat, irr=data1$PAR, kO2=kO2, zMix=data1$zMix, fluxDummy=fluxDummy)
  
  #Set up data.frame to store output of optimizations
  nDays <- dim(sun)[1] - 1  #Not sure if this indexing works appropriately for all lakes
  dateRange <- sun$day[c(1,nDays)]
  outDays <- seq(dateRange[1],dateRange[2],"1 day")
  optimOut <- data.frame(solarDay=outDays,nll=rep(NA,nDays), iotaEst=rep(NA,nDays), 
                         rhoEst=rep(NA,nDays), DOInitEst=rep(NA,nDays), optimCode=rep(NA,nDays),
                         R2=rep(NA,nDays),iotaSd=rep(NA,nDays),rhoSd=rep(NA,nDays),GPPSd=rep(NA,nDays))
  
  #Calculate appropriate starting guesses for iota and rho parameters
  #  This takes as reasonable guesses
  #    iota = 1 (mg L-1 d-1)/(mmol m-2 s-1)
  #    rho  = 0.5 mg L-1 d-1
  #  And converts them to appropriate units given time step of model
  iotaGuess <- 3*timeStep/1440 
  rhoGuess <- 0.5*timeStep/1440
  
  #Remove some stuff
  rm(DOSat, kO2, fluxDummy)

  ##
  #Useful stuff for plotting
  
  #Limits for y-axis for drivers
  irrLims <- range(data2$irr,na.rm=T)
  zMixLims <- c(max(data2$zMix,na.rm=T),0)
  atmFluxLims <- c(-10*max(data2$kO2,na.rm=T)*min(data2$zMix,na.rm=T),10*max(data2$kO2,na.rm=T)*min(data2$zMix,na.rm=T))
  
  #Set up pdf device
  pdf(file=paste(outName,'daily fits.pdf'),width=11,height=8.5)
  layout(rbind(matrix(c(1:14),nrow=2,byrow=F),matrix(c(15:28),nrow=2,byrow=F)),heights=c(1,1.5,1,1.5))
  par(mar=c(1,2,0,0)+0.1)
  
  #Save workspace
  save(list=ls(),file=paste(outName,'.RData',sep=""))
  
  ##
  #Run optimization for each day
  
  for (i in 1:nDays){
    #Print occasional progress report on i
    if (i %in% seq(1,nDays,10)) {print(paste("Starting day",i))}
    
    #Extract data between sunrise on day i and sunrise on day i+1
    timeSlice <- c(sun$sunrise[i], sun$sunrise[i+1])
    dataTemp <- data2[data2$datetime>=timeSlice[1] & data2$datetime<timeSlice[2],]
    
    #If we have less than 75% of the day, more than 20% of DOObs is missing, or if any NA in DOSat, irr, kO2, or zMix, 
    # return NA for optimization results and plot blank plots
    nTot <- length(dataTemp$DOObs)
    nNA <- length(which(is.na(dataTemp$DOObs)))
    if ((nTot<(24*60/timeStep*0.75)) | (nNA/nTot > 0.30) |  any(is.na(dataTemp[,3:6]))){
      optimOut[i,2:6] <- NA
      frame(); frame()
      next
    } else
      
      #Otherwise, fit model and make plots
    {
      
      #For guess of initial DOHat, use first obs unless that is NA, in which case use min obs
      if (is.na(dataTemp$DOObs[1])==F){
        DOInit <- dataTemp$DOObs[1]
      }else{
          DOInit <- min(dataTemp$DOObs,na.rm=T)
          }
      
      #Find parameter values by minimizing nll
      parGuess <- log(c(iotaGuess,rhoGuess,DOInit))
      optimTemp <- optim(parGuess,metabLoss_v4,dataIn=dataTemp)
      
      #Save min nll
      optimOut[i,2] <- optimTemp$value
      #Save parameter estimates
      #  Multiply by 1440/timeStep to get from units of timeStep^-1 to units of day^-1
      optimOut[i,3:4] <- exp(optimTemp$par[1:2])*(1440/timeStep)
      optimOut[i,5] <- exp(optimTemp$par[3]) 
      #Save code indicating whether nlm completed successfully
      optimOut[i,6] <- optimTemp$convergence
      
      #Calculate atmFlux and DOHat given max likelihood parameter estimates
      predix <- metabPredix_v4(optimTemp$par,dataTemp)
      DOHat <- predix$DOHat
      atmFlux <- predix$atmFlux
      res <- predix$res
      
      #Calculate SST, SSE, and R2, and save R2
      SST <- sum(dataTemp$DOObs^2,na.rm=T)
      SSE <- sum(res^2,na.rm=T)
      R2 <- (SST-SSE)/SST
      optimOut[i,"R2"] <- R2
      
      #bootstrapping 
      if(tolower(bootstrap)=='yes'){
        bootResult<-bootstrap.metab(parGuess = parGuess,dataTemp = dataTemp,n = 100, timeStep = timeStep)
        optimOut[i,9] = sd(bootResult$rho)
        optimOut[i,8] = sd(bootResult$iota)
        optimOut[i,10] = sd(bootResult$GPP)
      }
      
      #Plot irradiance (orange points), zMix (dashed line), atmFlux (hollow black points)
      #y-axis tick labels are for atmFlux; positive values are flux into lake and negative values are flux out of lake
      par(mar=c(1,2,0,0)+0.1)
      plot(dataTemp$irr~dataTemp$datetime, ylim=irrLims, axes=F, xlab="", ylab="", pch=18, col="dark orange")
      axis.POSIXct(1,dataTemp$datetime,labels=F); box()
      text(x=min(dataTemp$datetime),y=irrLims[2],labels=format(dataTemp$datetime[1],format="%d-%b"),adj=c(0,1))
      par(new=T); plot(dataTemp$zMix~dataTemp$datetime, ylim=zMixLims, type="l", lty=2, axes=F, xlab="", ylab="")
      par(new=T); plot(atmFlux~dataTemp$datetime, ylim=atmFluxLims, axes=F, xlab="", ylab=""); axis(2)
      
      #Plot observed and predicted DO
      yLims <- range(c(DOHat,dataTemp$DOObs),na.rm=T)
      par(mar=c(2,2,0,0)+0.1)
      plot(DOHat ~ dataTemp$datetime, ylim=yLims, type="l", axes=F, xlab="", ylab="")
      axis.POSIXct(1,dataTemp$datetime,format="%H:%M")
      axis(2)
      box()
      points(dataTemp$DOObs ~ dataTemp$datetime)
      meanDOSat <- round(mean(dataTemp$DOSat,na.rm=T),1)
      text(x=min(dataTemp$datetime),y=yLims[2],labels=paste('DOSat',meanDOSat),adj=c(0,1))
      
    }
    
  }  #end loop over nDays
  
  
  #Close pdf graphics device
  dev.off()
  
  #Dump optimOut to dirDump
  write.table(optimOut,paste(outName,'optimOut.txt'))
  
  
  #Calculate model-fitting version of GPP from iotaEst and total daily PAR
  #First calculate total mmol photons m-2 for each time step
  solarFlux <- data2$irr*timeStep*60  #mmol m-2 timeStep-1
  #Convert iota units
  iotaNewUnits <- optimOut$iotaEst/(60*60*24) #(mg L-1 s-1) / (mmol m-2 s-1)
  #Aggregate solarFlux - sum by day
  aggSolarFlux <- aggregate(solarFlux,by=list(solarDay=solarDaysVec),sum,na.rm=T)
  aggSolarFlux$solarDay <- as.POSIXct(trunc.POSIXt(as.POSIXct(aggSolarFlux$solarDay,tz="America/Chicago"),"days"),tz="America/Chicago")
  aggSolarFlux <- merge(aggSolarFlux,data.frame(solarDay=optimOut$solarDay,stringsAsFactors=FALSE),all.y=T)
  totalSolarFlux <- aggSolarFlux[,2] #mmol m-2 d-1
  
  #Calc GPP from fitted iota
  GPPFit <- iotaNewUnits*totalSolarFlux #mg L-1 d-1
  
  GPPFitOut <- data.frame(solarDay=optimOut$solarDay,GPPFit,stringsAsFactors=FALSE)

  maxZmix<-aggregate(dataZMix[,2],by=list(strftime(strptime(dataZMix[,1],"%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")),FUN=max,na.rm=TRUE) #applies the 'mean()' function to Zmix data by the given date - i.e. averages all Zmix data for a given date and stores in aveZmix 
  
  colnames(maxZmix)<-c("solarDay","zMix")  #make column headers standard for merging later on 
  
  colnames(GPPFitOut)<-c("solarDay","GPP") #make column headers standard 
  GPPFitOut[,1]<-as.character(GPPFitOut[,1])
  
  GPPFitOut<-merge(GPPFitOut,maxZmix,by.x="solarDay",all=T) #merges the two data vectors of metabolism and Zmix by given date. i.e. matches up data based on date
  
  write.table(GPPFitOut,paste(outName,"GPPFitOut.txt"))  
  
  optimOut=optimOut[as.character(optimOut$solarDay)%in%GPPFitOut$solarDay,]
  GPPFitOut=GPPFitOut[GPPFitOut$solarDay%in%as.character(optimOut$solarDay),]
  
  return(list(optimOut=optimOut,GPPFit=GPPFitOut))
}
