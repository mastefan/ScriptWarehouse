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
#' @param data (raster objects?) (files?)
#' @param ncores numeric. Number of cores to be used for processing

zScore <- function(data=NULL,ncores=1){
  
  # stop if no data is provided
  if(is.null(data)){
    stop('Please specify data source.')
  }
  
  # load data
  
  # 
  # 
  
  # set up for multi-core processing
  beginCluster(ncores,type='SOCK')
  
  # multi-year mean (for all years)
  mym <- raster::calc(data, fun=mean, na.rm=TRUE)
  
  # multi-year mean (excluding target year)
  test <- array(1:27,c(3,3,3))
  layers <- length(test[,,3])
  
  for(i in layers){
    a <- mean(data[-i])
    return(a)
  }
  
  # multi-year sd
  mys <- raster::calc(data, fun=sd, na.rm=TRUE)
  
  # calculate z score
  z <- raster::calc(data, fun=function(x){data-mym}) #does this calculate by pixel by year? i think so...
  
  # normalize
  z.n <- raster::calc(z, fun=function(x){z/mys})
  
  #return result
  return(z.n)
  
  # end multi-core processing
  endCluster()
}