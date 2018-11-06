#' raster2random - Stage 2
#' 
#' Takes a named list of raster bricks and converts it to a long-format 
#' dataframe for use with the randomForest function
#' 
#' Stage 2: Merges dependent variable with desired independent columns
#' 
#' @param in.dep DEPENDENT varia ble data. Provide a raster brick.
#' @param dep.name Vector for naming the dependent variable column. Defaults to "avhrrp".
#' @param in.ind INDEPENDENT variable data. Provide output from raster2random_s1. 
#' Can be a single dataframe or a named list of dataframes.
#' #@param months Vector of months from independent variable data to be included. 
#' #To specify different months for different variables provide a list of 
#' vectors of the same length as in.ind.
#' @param na.rm Should instances where DEPENDENT = NA be removed?
#' @param no.drift Have drift years been removed from in.dep data? T/F

raster2random_s2 <- function(in.dep = NULL, dep.name = "avhrrp",
                             in.ind = NULL, #months = 1:12, 
                             na.rm = TRUE, no.drift = TRUE){
  
  ## ensure that INDEPENDENT data is formatted as a list
  #if(class(in.ind) != "list") { in.ind <- list(independent = in.ind) }
  
  ## ensure that months are formatted as a list
  #if(class(months) != "list") { months <- list(independent = months)}
  
  ## get data for given months from given INDEPENDENT dataframes
  #ind <- list()
  #for(i in 1:length(months)){
  #  for(j in 1:length(in.ind)){
  #    ind[[j]] <- in.ind[[j]][,months[[i]]]
  #  }
  #}

  # convert selected INDEPENDENT data to a dataframe
  #ind <- as.data.frame(ind, stringsAsFactors = FALSE)
  #ind <- as.data.frame(in.ind, stringsAsFactors = FALSE)
  
  # prepare dependent data
  dep <- raster::as.data.frame(in.dep)
  dates <- seq(from = 1989, to = 2014, by = 1)
  if(no.drift == TRUE){
    nd_p_band <- c(1:3, 7:10, 13:26)
    dates <- dates[nd_p_band]
  }
  colnames(dep) <- dates
  dep <- data.frame(pix = 1:nrow(dep), dep)
  dep <- reshape2::melt(dep, id = "pix")
  colnames(dep) <- c("pix", "year", dep.name)
  
  # join DEPENDENT and INDEPENDENT data
  out <- cbind(dep, ind, stringsAsFactors = FALSE)
  
  # assign row names
  rownames(out) <- paste0(out$pix, out$year)
  
  # remove pix and year columns
  out <- out[,3:ncol(out)]
  
  if(na.rm == TRUE){
    # filter out rows where the first value column is NA
    out <- tibble::rownames_to_column(out)
    out <- dplyr::filter(out, !is.na(out[,2]))
    out <- tibble::column_to_rownames(out)
  }
  
  # return result
  return(out)
  
}
