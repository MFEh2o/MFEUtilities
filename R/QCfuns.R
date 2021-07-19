#QCfuns.R
#QC checks for everyday use
#RNN 2020-12-03
# KG note: as of 2021, many of these functions are probably superseded by the standard MFE database check script. But I'm keeping them here and documenting them just in case.

#' Check for missing/extra columns
#' 
#' @param old A data frame (the old/original version)
#' @param new A data frame (the new version)
#' @return Gives warnings/messages to tell about missing or extra columns.
#' @export

checkCols <- function(old, new){
  missing <-colnames(old)[!colnames(old) %in% colnames(new)]
  extra <- colnames(new)[!colnames(new) %in% colnames(old)]
  
  if(all(colnames(new)==colnames(old))==TRUE){
    message("Column names and order match")
  }else{
    message("Column names and order DO NOT match")
    warning(paste0(c("Missing: ", missing)), quote = F)
    warning(paste0(c("Extra: ", extra)), quote = F)
  }
}

#' Check that all metadataID's show up in the METADATA table. Requires that you have previously connected to the database using dbTable().
#' 
#' @param new A data frame (the old/original version)
#' @return Gives warnings/messages about metadataID's and whether they show up in the METADATA table.
#' @export
#' 
checkMet<- function(new, old = dbTable("METADATA")$metadataID){
  new <-new$metadataID
  newMet <- unique(new)
  
  if(all(unique(new) %in% old)){
    message("All metadata in METADATA", quote = F)
  }else{
    print("Good metadata:", quote = F)
    print(newMet[newMet %in% old], quote = F)
    print("Bad metadata:", quote = F)
    print(newMet[!newMet %in% old], quote = F)
  }
}

#' Parse sampleID's. KG NOTE: The <<- assignment here is not good practice; should assign to variables within the function and then have an object get returned. But I don't want to mess up existing workflows, so I'm leaving it like this for now.
#' 
#' @param table A data frame
#' @param part Which part of the sampleID do you want to retrieve?
#' @param fish TRUE/FALSE is this a fish sampleID?
#' @return Parsed sampleID portion.
#' @export
#' 
#parse sampleIDs
parseSampleID<- function(table, part, fish = FALSE){
  sID <- table$sampleID # get the sampleID's
  x<<- case_when(part == "lakeID" ~ word(sID,1,1,sep="_"),
              part == "siteName" ~ word(sID,2,2,sep="_"),
              part == "siteID" ~ word(sID,1,2,sep="_"),
              part == "dateID" ~ word(sID,3,3,sep="_"),
              part == "timeID" ~ word(sID,4,4,sep="_"),
              
              part == "gear" & fish == TRUE ~ word(sID,5,5,sep="_"),
              part == "metadataID" & fish == TRUE ~ word(sID,6,6,sep="_"),
              
              part == "depthClass" ~ word(sID,5,5,sep="_"),
              part == "depthBottom" ~ word(sID,6,6,sep="_"),
              part == "metadataID" ~ word(sID,7,7,sep="_"),
              TRUE ~ "help")
}

#' Attempt to recreate sampleID's from component parts. KG note: I'm not super clear on how this function works. It gets the component parts from the sampleID itself, rather than taking them from other columns, so under what circumstances would the sampleID not be able to be re-created?
#' 
#' @param table A data frame
#' @param fish TRUE/FALSE is this a fish sampleID?
#' @return Messages telling you whether the sampleID's can be re-created.
#' @export
#' 
recreateSampleIDs<- function(table, fish = FALSE){
  # Get each portion of the sampleID, using parseSampleID above. 
  siteID <-parseSampleID(table, "siteID", fish = fish)
  dateID <-parseSampleID(table, "dateID", fish = fish)
  timeID <-parseSampleID(table, "timeID", fish = fish)
  if(fish == TRUE){
    gear<-parseSampleID(table, "gear", fish = fish)
  }else{
    depthClass<- parseSampleID(table, "depthClass", fish = fish)
    depthBottom<- parseSampleID(table, "depthBottom", fish = fish)
  }
  metadataID<-parseSampleID(table, "metadataID", fish = fish)
  
  if(fish == TRUE){
    compiledID<- paste(siteID, dateID, timeID, gear, metadataID, sep = "_")
  }else(
    compiledID<- paste(siteID, dateID, timeID, depthClass, depthBottom, metadataID, sep = "_")
  )
  
  if(all(table$sampleID == compiledID)==TRUE){
    print("Yay! All sampleIDs can be recreated from component parts", quote = F)
  }else{
    help<- table$sampleID[!table$sampleID == compiledID]
    print(paste0("Error: ",length(help)," sampleIDs could not be recreated from component parts:"))
    print(help)
  }
}

#' Check that all INFO ID's are in SAMPLES
#' 
#' @param info A data frame, e.g. FISH_INFO.
#' @param sample A data frame, e.g. FISH_SAMPLES.
#' @return Messages about whether or not all the ID's from INFO are also present in SAMPLES.
#' @export
#' 
#check that all INFO ID's are in SAMPLES
checkINFO<-function(info, sample){
  infoIDs<-unique(info$sampleID)
  sampleIDs<-unique(sample$sampleID)
  
  missing<- infoIDs[!infoIDs %in% sampleIDs]
  check<-info[info$sampleID %in% missing,]
  
  if(length(missing)==0){
    print("All sampleIDs from INFO are in SAMPLES")
  }else{
    print(paste0("Error: ", length(missing), " unique sampleID(s) from INFO are not in SAMPLES:"))
    print(check$sampleID)
  }
}

# XXX ---------------------------------------------------------------------
# KG note: I've stopped documenting here. If you want to document the rest of these functions, follow the format shown above, and refer to [this resource](https://www.google.com/search?q=r+package+documentation+how-do&oq=r+package+documentation+how-do&aqs=chrome..69i57j33i22i29i30l5.2756j0j7&sourceid=chrome&ie=UTF-8) for instructions on documenting package functions.

#check for duplicates
checkDuplicates<- function(table, column){
  check<- table[,column]
  dup<- check[duplicated(check)==TRUE]
  dups<- subset(table,table[,column] %in% dup)
  
  if(length(dup)==0){
    print("No duplicate IDs in this column")
  }else{
    print(paste0("Error: ", length(dup), " unique ID(s) duplicated in this column:"))
    print(dups[,column])
  }
}

#find wherever something in a sampleID wherever it exists in the database
findID<-function(something){
  for (i in 1:length(dbTableList())){
    table<-dbTable(dbTableList()[i])
    if(any(grepl(something,table$sampleID))){
      print(paste0(dbTableList()[i], " in sampleID"))
    }
    if(any(grepl(something,table$siteID) & !dbTableList()[i]=="SAMPLES")){
      print(paste0(dbTableList()[i], " in siteID"))
    }
    if(any(grepl(something,table$metadataID) & !dbTableList()[i]=="SAMPLES")){
      print(paste0(dbTableList()[i], " in metadataID"))
    }
  }
}


#Change a string or substring in multiple columns and change the updateID
changeSomething<- function(something, with, tableName, columns, dateYYYYMMDD, updateID){
  table<-dbTable(tableName)
  tableFix<-dbTable(tableName)
  for (i in 1:length(columns)){
    tableFix[,columns[i]]<-gsub(something,with,tableFix[,columns[i]])
  }
  
  for (i in 1:nrow(tableFix)){
    if(any(tableFix[i,]==table[i,])==FALSE){
      tableFix$updateID[i]<-updateID
    }
  }
  write.csv(tableFix, paste0("Misc fixes/output/",tableName,".",dateYYYYMMDD, ".csv"), row.names = F)
}


