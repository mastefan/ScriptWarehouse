#' Z score calculator
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
#' @param parallel Should parallel processing be used
#'  to increase processing speed? (logical, default = TRUE)
#' @param ncores Number of cores to be used for processing 
#' (ignored if parallel = FALSE, numeric, default = 2)
#' 
#' Note that parallel processing is curreltly only functional for Windows.
#' This can be modified to work on MacOS or Unix by modifying the cluster type.
#' 
#' Note that the parallel processing has been very buggy and may hang.

zScore <- function(file=NULL, na.rm = TRUE, parallel = TRUE, ncores=2){
  
  # stop if no data is provided
  if(is.null(file)){
    stop('Please specify a file path.')
  }
  
  # begin multi-core processing if parallel = TRUE
  if(parallel == TRUE){
    raster::beginCluster(ncores, type='SOCK')
    cat(sprintf('\n executing zScore using a cluster size of %s', ncores))
  } else {
    cat('\n executing zScore without parallel processing')
  }
  
  # load data
  data <- suppressWarnings(raster::brick(file))

  # calculate SD of stack
  cat('\n calculating standard deviation')
  if(parallel == TRUE){
    sd <- raster::clusterR(x = data, fun = function(data){
               raster::calc(x = data, fun = sd, na.rm = na.rm)})
  } else {
    sd <- raster::calc(x = data, fun = sd, na.rm = na.rm)
  }
  
  # calculate mean for each layer (mean of all but band i)
  set <- 1:raster::nbands(data)
  mn <- data
  for (i in set){
    cat(sprintf('\n calculating mean for band %s', i))
    subset <- set[-i]
    wrk <- raster::stack(x = data, bands = subset)
    if(parallel == TRUE){
      mn_i <- raster::clusterR(x = wrk, fun = function(wrk){
        raster::calc(x = wrk, fun = mean, na.rm = na.rm)})
    } else {
      mn_i <- raster::calc(x = wrk, fun = mean, na.rm = na.rm)
    }
    mn[[i]] <- mn_i
  }
    
  # calculate the z score
  cat('\n calculating z scores')
  if(parallel == TRUE){
    fun1 <- function(x,y){x-y}
    z <- data
    for(i in set){
      wrk <- raster::stack(data[[i]],mn[[i]])
      z_i <- raster::clusterR(x = wrk, fun = raster::overlay, args = list(fun = fun1, na.rm = na.rm))
      z[[i]] <- z_i
    }
  } else {
    z <- raster::overlay(x = data, y = mn, 
                         fun = function(data, mn){ data - mn }, 
                         na.rm = na.rm)
  }
  
  # normalize the z score
  cat('\n normalizing z scores')
  if(parallel == TRUE){
    fun2 <- function(x,y){x/y}
    for(i in set){
      wrk <- raster::stack(z[[i]], sd)
      z_i <- raster::clusterR(x = wrk, fun = raster::overlay, args = list(fun = fun2, na.rm = na.rm))
      z[[i]] <- z_i
    }
  } else {
    z <- raster::overlay(x = z, y = sd, 
                         fun = function(z, sd){ z / sd },
                         na.rm = na.rm)
  }
  
  # end multi-core processing
  if(parallel == TRUE){
    raster::endCluster() 
  }
  
  # peoplespeak
  cat("\n c'est fini")
  
  #return result
  return(z)
}
