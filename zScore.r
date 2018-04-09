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
#' @param file A path to file containing stacked phenology data.
#' @param ncores numeric. Number of cores to be used for processing

zScore <- function(file=NULL,ncores=1){
  
  # set up for multi-core processing
  beginCluster(ncores,type='SOCK')
  
  # stop if no data is provided
  if(is.null(file)){
    stop('Please specify a file path.')
  }
  
  # load data
  data <- suppressWarnings(raster::brick(file))
  
  # create output raster object using characteristics of input file
  result <- raster::brick(template = data)
  
  # calculate the mean
  mn <- raster::stackApply(x = data,
                           fun = mean,
                           na.rm = TRUE)
  
  # calculate the sd
  sd <- raster::calc(data,
                     fun = sd,
                     na.rm = TRUE)
  
  # get number of bands to create vector to pass to for loop
  band_num <- raster::nbands(data)
  
  # calculate Z-score for each layer in data
  for (i in 1:band_num){
    
    # calculate the mean of all years excluding i
    mn <- raster::calc(x = data[[-i]],
                       fun = mean,
                       na.rm = TRUE)
    
    # calculate the z score
    z <- raster::calc(x = data,
                      fun = function(x) {data[[i]] - mn[[i]]},
                      na.rm = TRUE)
    
    # normalize the z score
    z_n <- raster::calc(x = z,
                        fun = function(x) {z[[i]] / sd},
                        na.rm = TRUE)
    
    # write the z score to the appropriate band in result
    result[[i]] <- z_n
  }
  
  #return result
  return(result)
  
  # end multi-core processing
  endCluster()
}