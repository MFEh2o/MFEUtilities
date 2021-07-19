####################################################
# Functions used to interact with the MFE database #
####################################################
# JAZ; 2015-11-30 
# SEJ; 2018-09-14

# load required packages
#***********************
library(RSQLite)
library(dplyr)

# defining functions for interaction with MFE database
#*****************************************************

# listing table names in MFE database 
dbTableList<-function(fpath=dbdir,dbname=db){
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection    
  #list tables in database
  tables=dbListTables(con) 
  dbDisconnect(con)
  return(tables)
}

# listing column names in a MFE database table 
dbTableSummary<-function(table,cols=c("lakeID","depthClass"),fpath=dbdir,dbname=db){
  table=as.character(table)
  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection
  #query an entire table
  table<-dbGetQuery(con,paste("SELECT * FROM", table)) #note that capitalization doesn't matter so LAKES=lakes
  
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
  
  #summarize table
  summary=list()
  for(i in 1:length(cols)){
    print(paste(cols[i]," - ",typeof(table[,cols[i]]),":",sep=""),quote=FALSE)
    print(sort(unique(table[,cols[i]])),quote=FALSE)
  }
  dbDisconnect(con)
}

# Importing database tables
dbTable<-function(table,lakeID=c(),depthClass=c(),fpath=dbdir,dbname=db){
  #set file path to the location of the database (defaults to my database location)
  table=as.character(table)

  drv=SQLite() #create driver object
  con=dbConnect(drv,dbname=file.path(fpath,dbname)) #open database connection
  #query an entire table
  table<-dbGetQuery(con,paste("SELECT * FROM", table)) #note that capitalization doesn't matter so LAKES=lakes
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
    mutate_if(is.character, list(~na_if(., ""))) %>% # 
    mutate_if(is.character, list(~na_if(., "NA")))
  
  dbDisconnect(con)
  return(table)
}
