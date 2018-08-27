#' raster2random
#' 
#' Takes a named list of raster bricks (dependent variable first)
#' and converts it to a long-format dataframe for use with 
#' the randomForest function
#' 
#' @param in.data Named list of input rasters. The independent variable
#'  should be first on the list.
#' @param na.rm logical Should rows where independent data = NA be removed?

raster2random <- function(in.data = NULL, na.rm = FALSE){
  
  # check that the inputs are a list
  if(class(in.data) != "list"){
    stop("input data must be given as a list")
  }
  
  # create an output list
  out <- list()
  
  # loop through items on list
  for(i in 1:length(names(in.data))){
    # convert raster brick to dataframe
    o1 <- raster::as.data.frame(in.data[[i]])
    
    # add pixel number identifier to dataframe
    o2 <- data.frame(pix = rownames(o1), o1)
    
    # convert dataframe to long format
    o3 <- reshape2::melt(o2, id = "pix")
    
    # write value strings
    out[i] <- list(o3$value)
    
  }
  
  # convert to dataframe
  out <- as.data.frame(out)
  
  # assign rownames to dataframe
  ## month.year.pixel
  rownames(out) <- paste0(o3[,2], ".", o3[,1])
  
  if(na.rm == TRUE){
    # filter out rows where the first value column is NA
    out <- dplyr::filter(out, !is.na(names(out)[1]))
  }
  
  # assign column names
  colnames(out) <- names(in.data)
  
  # return result
  return(out)
}