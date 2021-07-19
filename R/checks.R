# General database check functions
# Kaija Gahm, with some input from Randi Notte
# January 2021

#' Check whether something is a data frame
#' 
#' @param obj An object that you want to check for df-ness
#' @return Throws an error if `obj` is not a data frame.
#' @export
dfCheck <- function(obj){
  if(!is.data.frame(obj)){
    stop(paste0("`", deparse(substitute(obj)), 
                "` is not a data frame."))
  }
}

#' Check that a data frame has certain columns
#' 
#' `colsCheck` checks whether a data frame contains the specified columns. You can check for as many columns as you want, and the function will throw an informative error if *any* of them are missing (and it will tell you which ones) are missing. It will also check that `obj` is a data frame.
#' 
#' @param obj An object that you want to check for cols
#' @param cols A vector of column names you want to check for
#' @return Throws an informative error if any of `cols` are missing from `obj`. Additionally throws an error if `obj` is not a data frame (see `dfCheck()`)
#' @export
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

#' Check `sampleID` date against `dateSample`
#' 
#' `dateCheck` checks whether all the dates in the `sampleID` column match up with the dates in the `dateSample` column. If they don't, returns numeric indices of the rows that contain mismatches. This function additionally verifies that `df` is a data frame and that it has the columns `sampleID` and `dateSample`.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
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
    message(paste0(length(mismatchIDs), 
                   " date mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}

#' Check `sampleID` time against the time portion of `dateTimeSample`
#' 
#' `timeCheck` checks whether all the times in the `sampleID` column match up with the times in the `dateTimeSample` column. If they don't, returns numeric indices of the rows that contain mismatches. This function also verifies that `df` is a data frame and that it has the columns `sampleID` and `dateTimeSample`.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
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
    message(paste0(length(mismatchIDs), 
                   " time mismatches found. Returning row indices."))
    return(mismatchIDs)
  }
}

#' Check that dates match between `dateSample` and `dateTimeSample`
#' 
#' `dateSampleCheck` checks whether all the dates match between `dateSample` and `dateTimeSample`. If they don't, returns numeric indices of the rows that contain mismatches. This function also verifies that `df` is a data frame and that it has the columns `dateSample` and `dateTimeSample`.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
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



#' Check that dates match between `dateSet` and `dateTimeSet`
#' 
#' `dateSetCheck` checks whether all the dates match between `dateSet` and `dateTimeSet`. If they don't, returns numeric indices of the rows that contain mismatches. This function also verifies that `df` is a data frame and that it has the columns `dateSet` and `dateTimeSet`.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
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

#' Check that sample-set time spans are logical
#' 
#' `timeTravel` checks whether there are any rows where `dateTimeSet` is later than `dateTimeSample`, which wouldn't make sense. The function also verifies that `df` is a data frame and has `dateTimeSet` and `dateTimeSample` columns.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
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

#' Check for rows where `dateTimeSet` and `dateTimeSample` are the same
#' 
#' `sameTime` checks whether there are any rows where `dateTimeSet` is equal to `dateTimeSample`. The function also verifies that `df` is a data frame and has `dateTimeSet` and `dateTimeSample` columns.
#' 
#' @param df A data frame to check
#' @return ID's of rows that have mismatched dates.
#' @export
sameTime <- function(df, excludeZero = F){
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
  
  if(excludeZero){
    # if requested, exclude any where both times are 00:00
    zeroes <- which(dateTimeSet == dateTimeSample & 
                      grepl("00:00", dateTimeSet) & 
                      grepl("00:00", dateTimeSample))
    inds_sameTime <- inds_sameTime[!(inds_sameTime %in% zeroes)]
  }
  
  if(length(inds_sameTime) == 0 & excludeZero == F){
    message("No same-time rows detected.")
  }else if(length(inds_sameTime) == 0 & excludeZero == T){
    message("No same-time rows detected. Excluded ", length(zeroes), " rows where set and sample times were both 00:00.")
  }else{
    message(paste0(length(inds_sameTime), " same-time rows detected. Returning row indices."))
    return(inds_sameTime)
  }
}
