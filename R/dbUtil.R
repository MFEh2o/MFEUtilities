####################################################
# Functions used to interact with the MFE database #
####################################################
# JAZ; 2015-11-30 
# SEJ; 2018-09-14
# This version added to the MFE_Utilities package by Kaija Gahm on 19 July 2021.

# # load required packages
# #***********************
# library(RSQLite)
# library(dplyr)

#' Throw an error if trying to connect to a database file that doesn't exist. Avoids creating an 0B database file, which is the default behavior for dbConnect() otherwise. 
#' @param path Directory where the database is stored
#' @param db File name of the database file you're trying to connect to
#' @return If the database file doesn't exist, throws an error.
#' @export
lookForDB <- function(path, db){
  if(!file.exists(file.path(path, db))){
    stop(paste0("Can't find the database file you're attempting to access.\nfpath: ",
                fpath, "\ndbname: ", dbname, "\nFull path that failed: ",
                file.path(fpath, dbname),
                "\nDouble-check that you have the right database file name and location."))
  }
}

#' List names of the tables in the MFE database
#' 
#' @param fpath By default, the `dbdir` variable, which is defined in the global environment (usually at the beginning of each analysis script)
#' @param dbname A character string containing the name of the .db file you want to access. By default, the globally-defined `db` variable is used.
#' @return A vector of table names
#' @examples
#' #dbdir <- here("currentDB")
#' #db <- "MFEdb.db" # though note that it's best practice to specify 
#' # a database version with date attached.
#' #dbTableList(dbdir, db) # in this case, equivalent to dbTableList() 
#' # or dbTableList(fpath = dbdir, dbname = db)
#' @export

# listing table names in MFE database 
dbTableList<-function(fpath = dbdir, 
                      dbname = db){
  # Create SQLite driver object
  drv <- RSQLite::SQLite()
  
  # If database exists, connect to the database; open a connection object `con`
  lookForDB(fpath, dbname) # check whether database exists, and if not, throw an error.
  con <- DBI::dbConnect(drv, dbname = file.path(fpath, dbname)) 
  
  #List tables in database
  tables <- DBI::dbListTables(con) 
  
  # Disconnect from the database
  DBI::dbDisconnect(con)
  
  # Return the vector of table names
  return(tables)
}

#' List column names in a MFE database table 
#' 
#' @param table The database table to explore. **Not** case-sensitive.
#' @param cols A character vector of columns to summarize
#' @param fpath By default, the `dbdir` variable, which is defined in the global environment (usually at the beginning of each analysis script)
#' @param dbname A character string containing the name of the .db file you want to access. By default, the globally-defined `db` variable is used.
#' @return A summary of the chosen columns
#' @export
#' 
dbTableSummary<-function(table, cols=c("lakeID","depthClass"),
                         fpath = dbdir, dbname = db){
  table <- as.character(table)
  # Create driver object
  drv <- RSQLite::SQLite()
  # Open database connection
  lookForDB(fpath, dbname) # check whether database exists; if not, throw an error
  con <- DBI::dbConnect(drv, dbname = file.path(fpath, dbname)) 
  # Query an entire table
  table <- DBI::dbGetQuery(con, paste("SELECT * FROM", table)) 
  
  ###alter column classes within tables
  dateFix <- c("dateSet", "dateSample", "dateRun" )
  dateTimeFix <- c("dateTimeSet", "dateTimeSample")
  numericFix <- c("benthicBacterialProductionVolume_ugC_L_h","benthicBacterialProductionArea_mgC_m2_h","incubationDuration_h","BacterialProduction_ugC_L_h","depthTop","depthBottom","bodyLength","headWidth","dryMass","chl","abs440","g440","DOC","TN_DOC","dietItemCount","dietItemBodyLength","dietItemHeadWidth","otherLength","dietItemRangeLower","dietItemRangeHigher","dryMass_bodylength","dryMass_headwidth","dryMass_other","totalDryMass","fishLength","fishWeight","mortality","removed","otolithSample","tissueSampled","dietSampled","gonadRemoved","leftEyeRemoved","photo","gonadWeight","gonadSqueze","annulusNumber","paramValue","interpretationNumber","effort","CH4PeakArea","CO2PeakArea","CH4ppm","CO2ppm","CH4_uM","CO2_uM","sampleWt","d13C","d15N","d2H","d18O","percentC","percentN","percentH","surfaceArea","maxDepth","lat","long","temp","DOmgL","DOsat","SpC","pH","ORP","PAR","parameterValue","lakeLevel_m","wellLevel_m","wellLevelCorrected_m","hydraulicHead_m","wellHeightAboveGround_m","waterTable_m","filterVol","sampleAmount","POC","PON","benthicRespiration_mgC_m2_h","benthicNPP_mgC_m2_h","benthicGPP_mgC_m2_h","ppb","rhodReleaseVolume","wetMass","ashedMass","percentOrganic","waterHeight","waterHeight_m","tPOCdepGreater35_mgC_m2_d","tPOCdepLess35_mgC_m2_d","count","abundance_num_m3","biomass_gDryMass_m3","slope","intercept","length","width","mass","eggs","production","prodSD","seasonalSD","production_m3","prodSD_m3","seasonalSD_m3","production_eggRatio","wtEmpty","wtFull","wtSubsample")
  integerFix <- c("flag")
  factorFix <- c("replicate")
  for(i in which(colnames(table) %in% dateFix)) {
    table[,i] <- as.Date(table[,i],tz="America/Chicago")
  }
  for(i in which(colnames(table) %in% dateTimeFix)) {
    table[,i] <- as.POSIXct(table[,i],tz="America/Chicago")
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
  
  # Print a summary of the table
  summary <- list()
  for(i in 1:length(cols)){
    print(paste(cols[i]," - ",typeof(table[,cols[i]]),":",sep=""),quote=FALSE)
    print(sort(unique(table[,cols[i]])),quote=FALSE)
  }
  # Disconnect from the database
  DBI::dbDisconnect(con)
}

#' Import an MFE database table, and (optionally) do a little bit of querying.
#' 
#' @param table The database table you want to import. **Not** case-sensitive.
#' @param lakeID (optional) lakeID's to subset.
#' @param depthClass (optional) depthClasses to subset.
#' @param fpath By default, the `dbdir` variable, which is defined in the global environment (usually at the beginning of each analysis script)
#' @param dbname A character string containing the name of the .db file you want to access. By default, the globally-defined `db` variable is used.
#' @return The chosen database table
#' @export
#' 
# Importing database tables
dbTable <- function(table, lakeID = c(), depthClass = c(),
                  fpath = dbdir, dbname = db){
  table <- as.character(table)
  drv <- RSQLite::SQLite() #create driver object
  # Check that the db exists
  lookForDB(fpath, dbname) # check whether database exists; if not, throw an error
  # Open a database connection
  con <- DBI::dbConnect(drv,dbname=file.path(fpath,dbname))
  
  # Query an entire table
  table<-DBI::dbGetQuery(con,paste("SELECT * FROM", table)) #note that capitalization doesn't matter so LAKES=lakes
  if(!is.null(lakeID)){
    table<-table[table$lakeID%in%lakeID,]
  }
  if(!is.null(depthClass)){
    table<-table[table$depthClass%in%depthClass,]
  }
  ###alter column classes within tables
  dateFix=c("dateSet", "dateSample", "dateRun" )
  dateTimeFix=c("dateTimeSet", "dateTimeSample")
  numericFix=c("benthicBacterialProductionVolume_ugC_L_h","benthicBacterialProductionArea_mgC_m2_h","incubationDuration_h","BacterialProduction_ugC_L_h","depthTop","depthBottom","bodyLength","headWidth","dryMass","chl","abs440","g440","DOC","TN_DOC","dietItemCount","dietItemBodyLength","dietItemHeadWidth","otherLength","dietItemRangeLower","dietItemRangeHigher","dryMass_bodylength","dryMass_headwidth","dryMass_other","totalDryMass","fishLength","fishWeight","mortality","removed","otolithSample","tissueSampled","dietSampled","gonadRemoved","leftEyeRemoved","photo","gonadWeight","gonadSqueze","annulusNumber","paramValue","interpretationNumber","effort","CH4PeakArea","CO2PeakArea","CH4ppm","CO2ppm","CH4_uM","CO2_uM","sampleWt","d13C","d15N","d2H","d18O","percentC","percentN","percentH","surfaceArea","maxDepth","lat","long","temp","DOmgL","DOsat","SpC","pH","ORP","PAR","parameterValue","lakeLevel_m","wellLevel_m","wellLevelCorrected_m","hydraulicHead_m","wellHeightAboveGround_m","waterTable_m","filterVol","sampleAmount","POC","PON","benthicRespiration_mgC_m2_h","benthicNPP_mgC_m2_h","benthicGPP_mgC_m2_h","ppb","rhodReleaseVolume","wetMass","ashedMass","percentOrganic","waterHeight","waterHeight_m","tPOCdepGreater35_mgC_m2_d","tPOCdepLess35_mgC_m2_d","count","abundance_num_m3","biomass_gDryMass_m3","slope","intercept","length","width","mass","eggs","production","prodSD","seasonalSD","production_m3","prodSD_m3","seasonalSD_m3","production_eggRatio","wtEmpty","wtFull","wtSubsample")
  integerFix=c("flag")
  factorFix=c("replicate")
  
  for(i in which(colnames(table) %in% dateFix)) {
    table[,i] <- as.Date(table[,i],tz="America/Chicago")
  }
  
  for(i in which(colnames(table) %in% dateTimeFix)) {
    table[,i] <- as.POSIXct(table[,i],tz="America/Chicago",optional=TRUE)
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
  
  ### Fix NA strings
  table <- table %>% #change "" and "NA" to actual NA (written as <NA> in character/factor vectors to distinguish from "NA") 
    dplyr::mutate(dplyr::across(where(is.character), dplyr::na_if, ""),
           dplyr::across(where(is.character), dplyr::na_if, "NA"))
  
  # Disconnect from the database
  DBI::dbDisconnect(con)
  return(table)
}
