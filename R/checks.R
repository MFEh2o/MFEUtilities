# Check functions
# Originally created by Kaija Gahm on 7 January 2020
# Inspired by GH issue 107. This is a draft. Will be expanded as needed.

# Libraries ---------------------------------------------------------------
library(dplyr)

# Check that object is a data frame ---------------------------------------
dfCheck <- function(obj){
  if(!is.data.frame(obj)){
    stop(paste0("`", deparse(substitute(obj)), 
                "` is not a data frame."))
  }
}


# Check that data frame has certain columns -------------------------------
colsCheck <- function(obj, cols){
  # Check whether the object is a data frame
  dfCheck(obj)
  
  # Check whether the object has the cols
  if(!all(cols %in% names(obj))){
    ## get the names of the missing cols
    missingCols <- cols[!(cols %in% names(obj))]
    
    ## print informative error message
    stop(paste0("The following columns are missing from `", 
                deparse(substitute(obj)), "` : ", 
                paste(missingCols, collapse = ", ")))
  }
}

# Check sampleID date -----------------------------------------------------
## This function checks the date in the sampleID against the date in the dateSample column
dateCheck <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a sampleID and a dateSample column
  colsCheck(df,
            cols = c("sampleID", "dateSample"))
  
  # Grab the date portion of the sampleID
  sampleIDDates <- stringr::str_extract(df$sampleID,
                                        pattern = "(?<=\\_)\\d{8}(?=\\_)") %>%
    lubridate::ymd()
  
  # Grab the dateSample dates
  dateSampleDates <- df$dateSample
  
  # Return logic
  if(all(dateSampleDates == sampleIDDates)){
    message("All dates match between `sampleID` and `dateSample`.")
  }else{
    mismatchIDs <- which(sampleIDDates != dateSampleDates)
    message(paste0(length(mismatchIDs), " date mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}

# Check sampleID time -----------------------------------------------------
timeCheck <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a sampleID and a dateTimeSample column
  colsCheck(df,
            cols = c("sampleID", "dateTimeSample"))
  
  # Grab the time portion of the sampleID
  sampleIDTimes <- stringr::str_extract(df$sampleID,
                                        pattern = "(?<=\\_\\d{8}\\_)\\d{3,4}(?=\\_)")
  
  # Grab the dateTimeSample time and convert it to character
  dateTimeSampleTimes <- df$dateTimeSample %>%
    as.character() %>%
    substr(., 12, 19) %>%
    substr(., 1, 5) %>%
    stringr::str_replace(., ":", "")
  
  # Return logic
  if(all(dateTimeSampleTimes == sampleIDTimes)){
    message("All times match between `sampleID` and `dateTimeSample`.")
  }else{
    mismatchIDs <- which(sampleIDTimes != dateTimeSampleTimes)
    message(paste0(length(mismatchIDs), " time mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}


# Check dateSample and dateTimeSample -------------------------------------
dateSampleCheck <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a dateSample and a dateTimeSample column
  colsCheck(df,
            cols = c("dateSample", "dateTimeSample"))
  
  # Get the date portion of dateSample
  dateSampleDates <- df$dateSample %>%
    lubridate::ymd()
  
  # Get the date portion of dateTimeSample
  dateTimeSampleDates <- df$dateTimeSample %>%
    lubridate::date() %>%
    lubridate::ymd()
  
  # Return logic
  if(all(dateSampleDates == dateTimeSampleDates)){
    message("All dates match between `dateSample` and `dateTimeSample`.")
  }else{
    mismatchIDs <- which(dateSampleDates != dateTimeSampleDates)
    message(paste0(length(mismatchIDs), " date mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}

# Check dateSet and dateTimeSet -------------------------------------------
dateSetCheck <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a dateSet and a dateTimeSet column
  colsCheck(df,
            cols = c("dateSet", "dateTimeSet"))
  
  # Get the date portion of dateSet
  dateSetDates <- df$dateSet %>%
    lubridate::ymd()
  
  # Get the date portion of dateTimeSet
  dateTimeSetDates <- df$dateTimeSet %>%
    lubridate::date() %>%
    lubridate::ymd()
  
  # Return logic
  if(all(dateSetDates == dateTimeSetDates)){
    message("All dates match between `dateSet` and `dateTimeSet`.")
  }else{
    mismatchIDs <- which(dateSetDates != dateTimeSetDates)
    message(paste0(length(mismatchIDs), " date mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}

# Check whether time spans make sense -------------------------------------
# Checks whether dateTimeSample is later than dateTimeSet
timeTravel <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a dateSet and a dateTimeSet column
  colsCheck(df,
            cols = c("dateTimeSet", "dateTimeSample"))
  
  # Get dateTimeSet
  dateTimeSet <- df$dateTimeSet %>%
    lubridate::parse_date_time(., orders = c("ymd_HMS", "ymd", "ymd_HM"))
  
  # Get dateTimeSample
  dateTimeSample <- df$dateTimeSample %>%
    lubridate::parse_date_time(., orders = c("ymd_HMS", "ymd", "ymd_HM"))
  
  # Return logic
  inds_timeTravel <- which(dateTimeSet > dateTimeSample)
  
  if(length(inds_timeTravel) == 0){
    message("No time travel detected.")
  }else{
    message(paste0(length(inds_timeTravel), " instances of time travel detected. Returning row indices."))
    return(inds_timeTravel)
  }
}

# Checks whether dateTimeSample is equal to dateTimeSet
sameTime <- function(df){
  # Verify that df is a data frame
  dfCheck(df)
  
  # Verify that df has a dateSet and a dateTimeSet column
  colsCheck(df,
            cols = c("dateTimeSet", "dateTimeSample"))
  
  # Get dateTimeSet
  dateTimeSet <- df$dateTimeSet %>%
    lubridate::parse_date_time(., orders = c("ymd_HMS", "ymd", "ymd_HM"))
  
  # Get dateTimeSample
  dateTimeSample <- df$dateTimeSample %>%
    lubridate::parse_date_time(., orders = c("ymd_HMS", "ymd", "ymd_HM"))
  
  # Return logic
  inds_sameTime <- which(dateTimeSet == dateTimeSample)
  
  if(length(inds_sameTime) == 0){
    message("No same-time rows detected.")
  }else{
    message(paste0(length(inds_sameTime), " same-time rows detected. Returning row indices."))
    return(inds_sameTime)
  }
}