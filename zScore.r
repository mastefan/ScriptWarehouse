#' Z score calculator
#' 
#' Does not include option for parallel processing
#' 
#' This script is designed to calculate the Z score of pixels in a stack of 
#' rasters. This formula is derived from Rodriguez-Galiano et al. 2016, 
#' Biogeosciences. Z scores show the difference from a multi-image mean 
#' in a given raster at a given pixel.
#' 
#' The z score values for a given year were defined as the difference from 
#' the multi-year mean, normalized by the standard deviation across years.
#' 
#' Rodriguez-Galiano calculated z scores excluding the targeted year in 
#' the multi-year mean to enhance inter-annual variation.
#' 
#' @param file A path to file containing stacked phenology data 
#' (do not provide a raster object)
#' @param na.rm Should NA values be retained or removed? (logical, default = TRUE)
#' @param ncores Number of cores to be used for processing 
#' (ignored if parallel = FALSE, numeric, default = 2)
#' 
#' Note that parallel processing is curreltly only functional for Windows.
#' This can be modified to work on MacOS or Unix by modifying the cluster type.
#' 
#' Note that the parallel processing has been very buggy and may hang.

zScore <- function(file=NULL, na.rm = TRUE, ncores=2){
  
  # stop if no data is provided
  if(is.null(file)){
    stop('Please specify a file path.')
  }
  
  # load data
  data <- suppressWarnings(raster::brick(file))

  # calculate SD of stack
  cat('\n calculating standard deviation')
  sd <- raster::calc(x = data, fun = sd, na.rm = na.rm)

  # calculate mean for each layer (mean of all but band i)
  set <- 1:raster::nbands(data)
  mn <- data
  for (i in set){
    cat(sprintf('\n calculating mean for band %s', i))
    subset <- set[-i]
    wrk <- raster::stack(x = data, bands = subset)
    mn_i <- raster::calc(x = wrk, fun = mean, na.rm = na.rm)
    mn[[i]] <- mn_i
  }
    
  # calculate the z score
  cat('\n calculating z scores')
  z <- raster::overlay(x = data, y = mn, 
                       fun = function(data, mn){ data - mn }, 
                       na.rm = na.rm)

  # normalize the z score
  cat('\n normalizing z scores')
  z <- raster::overlay(x = z, y = sd, 
                       fun = function(z, sd){ z / sd },
                       na.rm = na.rm)

  # name bands in output
  names(z) <- names(data)
  
  # peoplespeak
  cat("\n c'est fini!\n")
  
  #return result
  return(z)
}
