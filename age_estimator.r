#' age_estimator
#' 
#' Estimates the age of trees based on DBH and age using coefficient 
#' from the diameter_ratio function

age_estimator <- function(dbh=NULL,
                          dch=NULL,
                          core.height=NULL,
                          age=NULL,
                          sample.length=NULL){

  # check data presence
  if(is.null(dbh|dch|core.height|age|sample.length)){
    cat("\nNo data provided...")
  }
  
  # combine data into a dataframe
  data <- data.frame(dbh,dch,core.height,age,sample.length,
                     model,
                     stringsAsFactors=FALSE)
  
  # run a for loop over dataframe to perform calculations
  for(i in data){
    
    if(!is.null(data$dch)){
      
      missing <- data$dbh/2-data$sample.length # missing distance
      
      width <- data$age/(data$sample.length+missing) # age/corrected length
      
      width <- predict(model, newdata=data.frame(x=width)) # predict dch
      
    } else {
      
      missing <- data$dch/2-data$sample.length # missing distance
      width <- data$age/(data$sample.length+missing) # age/corrected length
      
    }
    
    age <- data$age+(missing*width) # estimating age by
    
    return(age) # output
  }
}