#' Sensor DB Table
#' Retrieving a table from the sensor database. Created by JAZ, 2014-12-10; updated 2017-03-08
#' @param table The data table to be returned. *Not* case-sensitive.
#' @param dbname Name of the database file. Default is whatever you've set to the variable `sensor_db` in your script.
#' @param lakeID Vector of lakeID's that you want; default returns all lakeID's.
#' @param depthClass Vector of depthClass values that you want; default returns all depthClasses (i.e. PML, hypo, etc...)
#' @param minDepth_m Numeric value of minimum sensor depth
#' @param maxDepth_m Numeric value of maximum sensor depth
#' @param minDate Character string of minimum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)
#' @param maxDate Character string of maximum sample date in standard unambiguous format (i.e. YYYY-MM-DD, YYYY-MM-DD HH:MM, or YYYY-MM-DD HH:MM:SS)
#' @param dateFormat Character string of date format if not in standard unambiguous format (i.e. '%y/%m/%d')
#' @return Database table, pulled from the sensor database, filtered by the specified parameters if applicable.
#' @export
sensordbTable<-function(table,fpath=sensor_dbdir,
                        dbname=sensor_db,lakeID=c(),minDepth_m=c(),maxDepth_m=c(),
                        minDate=c(),maxDate=c(),dateFormat=c()){
  #set file path to the location of the database (defaults to Jake's database location)
  table=as.character(table)
  library(RSQLite)
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection
  #query an entire table
  table<-dbGetQuery(con,paste('SELECT * FROM', table)) #note that capitalization doesn't matter so LAKES=lakes
  if(!is.null(lakeID)){
    table<-table[table$lakeID%in%lakeID,]
  }
  if(!is.null(table$depth_m)){
    if(!is.null(minDepth_m)){
      table<-table[table$depth_m>=minDepth_m,]
    }
    if(!is.null(maxDepth_m)){
      table<-table[table$depth_m<=maxDepth_m,]
    }
  }
  
  if(!is.null(table$dateTime)){
    table$dateTime<-as.POSIXct(table$dateTime,tz="America/Chicago") # changing from character to POSIXct
    if(!is.null(minDate)){
      if(!"POSIXct"%in%class(minDate)){
        if(is.null(dateFormat)){
          minDate <- as.POSIXct(minDate,tz="America/Chicago")
        }else{
          minDate <- as.POSIXct(minDate, format=dateFormat,tz="America/Chicago")
        }
      }
      table<-table[table$dateTime>minDate,]
    }
    if(!is.null(maxDate)){
      if(!"POSIXct"%in%class(maxDate)){
        if(is.null(dateFormat)){
          maxDate <- as.POSIXct(maxDate,tz="America/Chicago")
        }else{
          maxDate <- as.POSIXct(maxDate, format=dateFormat,tz="America/Chicago")
        }
      }
      table<-table[table$dateTime<maxDate,]
    }
  }
  
  ###alter column classes within tables
  dateFix=c('date' )
  dateTimeFix=c('dateTime')
  numericFix=c('precip_mm', 'cleanedPrecip_mm', 'temp_C', 'DO_mg_L', 'spcond_uS_cm', 'pH', 'pH_mV', 'DO_pctSat', 'batt_V', "cleanedLight_lux",
               'cleanedTemp_C', 'cleanedDO_mg_L', 'press_kPa', 'cleanedPress_kPa', 'atmPressure_kPa', 'PAR_uE_m2_s', 'windSpeed_m_s', 'depth_m',
               'windGust_m_s', 'windDir_deg', 'RH_pct', 'dewPoint_C', 'cleanedAtmPressure_kPa', 'cleanedPAR_uE_m2_s', "light_lux",
               'cleanedWindSpeed_m_s', 'cleanedWindGust_m_s', 'cleanedWindDir_deg', 'cleanedRH_pct', 'cleanedDewPoint_C')
  characterFix = c('location', 'changesCodePress', 'changesCodePAR', 'changesCodeWindSpeed', 'changesCodeWindGust',"changesCodeLight",
                   'changesCodeWindDir', 'changesCodeTemp', 'changesCodeRH', 'changesCodeDewPoint', 'lakeID', 'changesCodeDO',
                   "changesCodePrecip")
  integerFix=c('flag')
  factorFix=c('replicate')
  
  for(i in which(colnames(table) %in% dateFix)) {
    table[,i] <- as.Date(table[,i],tz="America/Chicago")
  }
  
  for(i in which(colnames(table) %in% dateTimeFix)) {
    table[,i] <- as.POSIXct(table[,i],tz="America/Chicago")
  }
  for(i in which(colnames(table) %in% characterFix)) {
    table[,i] <- as.character(table[,i])
  }
  
  for(i in which(colnames(table) %in% numericFix)) {
    table[,i] <- as.numeric(table[,i])
  }
  
  for(i in which(colnames(table) %in% integerFix)) {
    table[,i] <- as.integer(table[,i])
  }
  
  for(i in which(colnames(table) %in% factorFix)) {
    table[,i] <- as.factor(table[,i])
  }
  
  return(table)
}

#' Sensor DB Table List
#' Listing table names in the MFE sensor database. Created by JAZ, 2015-11-30
#' @param fpath File path of database file location. Defaults to whatever you set as `sensor_dbdir` in your script.
#' @param dbname Name of the database file. Default is whatever you've set to the variable `sensor_db` in
#' @return Vector of database table names
#' @export
#' 
sensordbTableList<-function(fpath=sensor_dbdir,
                            dbname=sensor_db){
  #set file path to the location of the database (defaults to Jake's database location)
  library(RSQLite)
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection
  #list tables in database
  tables=dbListTables(con)
  return(tables)
}