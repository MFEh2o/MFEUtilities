# A 'reverse regex' function designed to find e.g. unique date/time formats, unique tag formats, etc. Originally written as part of the fish entry tool, but I'm putting it here because why not.

#' Reverse Regex
#' 
#' A function to find all unique 'general' formats in a vector. Useful for e.g. tag formats, date formats, etc.
#' @param vec A character vector that you want to parse into all general formats.
#' @param unique Logical; whether to return one result per element of the vector (unique = F, default) or return just all unique formats (unique = T).
#' @return If unique = F (default), returns a vector with the same length as `vec`, containing general formats where '0' stands in for digits 0-9; 'a' stands in for any lowercase letter, and 'A' stands in for any uppercase letter. Non-alphanumeric characters are left as is. If unique = T, this function returns instead a vector of all unique formats, which may not have the same length as `vec`.
#' @examples
#' rr(c("09-07-2021", "9/7/21", "2021-09-07"))
#' @export
rr <- function(vec, unique = F){
  # Check that `vec` is a character vector that has at least some non-missing entries
  checkmate::assertCharacter(vec, all.missing = F)
  
  # Classify each character as uppercase (A), lowercase (a), or digit (0). Leave punctuation marks as they are (because for my use case, different punctuation = different format)
  classified <- vec %>%
    stringr::str_replace_all(., "[0-9]", "0") %>%
    stringr::str_replace_all(., "[A-Z]", "A") %>%
    stringr::str_replace_all(., "[a-z]", "a")
  
  # Two possible returns: all unique formats, or all formats
  if(unique == T){
    # Get all unique formats
    unique_formats <- unique(classified)
    
    # Return unique formats
    return(unique_formats)
  }else{
    return(classified)
  }
}