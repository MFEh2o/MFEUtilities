#' Get data in all database tables that matches specific criteria. For example, pull all data that has one of a set of metadataID's, projectID', etc. First, we connect to the database and list out all the names of the tables contained in it. Then, for each table in that list, we look through its column names to see if they contain the desired column. If they do, we filter that table to get rows associated with the metadataID(s) in question. Then we store those in a list. Then, for each database table name in the list, we query the database to get all rows of that table with the desired values in the target column. 
#' 
#' @param column The column that you want to filter on, in quotes. For example, "metadataID", or "sampleID"
#' @param values A character vector of 1 or more values that you'd like to filter for. 
#' @param con An SQLite() database connection, created with `dbConnect()`.
#' @return Returns a named list. Each element of the list is a data frame (a subset of a database table), named after the database table it comes from. Each element contains only the rows that match your criteria. For example, if `column` was "metadataID" and `values` was c("foo", "bar"), then each element will contain only rows where the metadataID is either "foo" or "bar".
#' @examples
#' #library(RSQLite) 
#' #con <- dbConnect(SQLite(), here("currentDB", "MFEdb.db")) # establish a 
#' #database connection
#' #getData(column = "metadataID", values = c("DOC.20110601", "Color.20110601"), 
#' #con) # get all rows in any database table that have either of the two 
#' #metadataID's shown.
#' #getData(column = "sampleID", values = 
#' #c("RB_DeepHole_20110613_1130_point_5_ZoopSurv.Sample.20110517", 
#' #"RB_DeepHole_20110613_1130_point_4.5_ZoopSurv.Sample.20110517", 
#' #"RB_DeepHole_20110613_1130_point_4_ZoopSurv.Sample.20110517"), con) # Get 
#' #all rows from all tables where these sampleID's show up.
#' # You can also pass a vector to the `values` argument. 
#' #ids <- c("RB_DeepHole_20110613_1130_point_5_ZoopSurv.Sample.20110517", 
#' #"RB_DeepHole_20110613_1130_point_4.5_ZoopSurv.Sample.20110517", 
#' #"RB_DeepHole_20110613_1130_point_4_ZoopSurv.Sample.20110517")
#' #getData(column = "sampleID", values = ids, con)
#' @export

getData <- function(column, values, con){
  # Get the names of all the db tables
  tableNames <- DBI::dbListTables(con)
  
  # Get the column names of all the db tables
  l <- lapply(tableNames, function(x) DBI::dbListFields(con, x))
  
  # Which tables contain the desired `column`?
  l2 <- lapply(l, function(x) column %in% x) %>% unlist() 
  
  # Focus on the tables that have the desired `column`.
  tableNamesReduced <- tableNames[l2]
  
  # For each of remaining table, grab all rows that have one of the desired `values` in the target `column`.
  rows <- lapply(tableNamesReduced, function(x){
    query <- paste0("SELECT * FROM ", x, " WHERE ", column, " in (",
                    paste0(sprintf("'%s'", values), collapse = ", "), ")")
    rows <- DBI::dbGetQuery(con, query)
    return(rows)
  }) # now we have a list of db table subsets.
  
  # Name the list elements with the table names
  names(rows) <- tableNamesReduced
  
  # Keep only the subsets that have at least 1 row.
  rows <- rows %>%
    purrr::keep(~ nrow(.x) > 0)
  
  # Return the named list!
  return(rows)
}