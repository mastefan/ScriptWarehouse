#' diameter_ratio
#' 
#' This tool estimates the relationship between DBH and DCH for the trees 
#' samples for where this is available.
#' 
#' @param dbh a vector containing DBH values
#' @param dch a vector containing DCH values
#' @param core.height a vector containing coring heights for a given 
#' DCH value
#' @param age a vector containing the counted age of a given sample
#' 
#' Note that dbh, dch and core.height values should have a matched order

diameter_ratio <- function(dbh=NULL,
                           dch=NULL,
                           core.height=NULL,
                           age=NULL,
                           sample.length=NULL){

  # check data presence
  if(is.null(dbh|dch|core.height|age|sample.length)){
    cat("\nNo data provided...")
  }
  
  # estimate missing length
  missing <- dch/2-sample.length
  
  # estimate average ring width
  width <- age/(sample.length+missing)
  
  # calculate ratio by tree
  ratio <- dbh/dch
  
  # get linear coefficient of ratio by core height
  model <- lm(width ~ ratio)
  
  # output coefficient
  return(model)
}