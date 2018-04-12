#' summarizer
#' 
#' Gives summary statistics of inputted Daymet data.
#' Includes: mean, median & sd
#' 
#' @param data An R object containing the data to be summarized

summarizer <- function(data = NULL){
  # input checks
  if(is.null(data)){
    stop('Please provide data as R object.')
  }
  
  # convert to dataframe if data is raster
  if(is.raster(data)){
    data <- raster::as.data.frame(data)
  }
  
  # get column names
  c_names <- colnames(data, do.NULL = FALSE)
  
  # calculate summary stats
  s_mean <- sapply(data, FUN = mean, na.rm = TRUE)
  s_median <- sapply(data, FUN = median, na.rm = TRUE)
  s_sd <- sapply(data, FUN = sd, na.rm = TRUE)
  
  # create output dataframe
  result <- rbind(s_mean, s_median, s_sd)
  
  # define row names
  row.names(result) <- c("mean","median","sd")
  
  # return result
  return(result)
}