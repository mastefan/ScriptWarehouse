#' calc_dd
#' 
#' @param tmean daily average temperature data for one year (365 days)
#' @param threshold temperature threshold (use same units as data)
#' @param index options are "grow" or "chill"
#' @param accumulate should degree days be accumulated? 
#' Options are "sum" or "daily".
#' @param start starting day (1 - 365)
#' @param end ending day (1 - 365)
#' 
#' Note that na.rm is set to TRUE

calc_dd <- function(tmean = NULL, threshold = NULL, index = NULL, start = 1, end = 365, accumulate = NULL){
  
  # input checks
  if(is.null(tmean)) { stop("please provide tmin and tmean data") }
  if(is.null(threshold)) { stop("please establish a threshold") }
  if(is.null(index)) { stop("please specify the index to be calculated") }
  
  # length checks
  if(raster::nlayers(tmean) < 365){ stop("Please provide daily tmean data") }
  if(raster::nlayers(tmean) > 365){ stop("Please provide a single year of data") }
  
  # load specified data bands
  if(class(tmean) != "RasterBrick"){
    tmean <- raster::stack(x = tmean, bands = start:end)
  } else {
    tmean <- raster::stack(x = tmean, layers = start:end)
  }
  
  # growing degree days function
  if(index == "grow"){
    # set function
    f <- function(x,y){
      if(x > y){
        x <- x - y
      } else {
        x <- 0
      }
    }
  }
  
  # chilling degree days
  if(index == "chill"){
    # set function
    f <- function(x,y){
      if(x < y){
        x <- y - x
      } else {
        x <- 0
      }
    }
  }
  
  # create threshold raster
  thresh <- raster::raster(tmean[[1]])
  thresh <- raster::setValues(thresh, values = threshold)
  
  # run function
  result <- raster::overlay(x = tmean, y = thresh, 
                            fun = Vectorize(f), recycle = TRUE)
  
  # accumulate if told to do so
  if(accumulate == "sum"){
    result <- raster::calc(x = result, fun = sum, na.rm = TRUE)
  }
  if(accumulate == "daily"){
    # function to calculate accumulated sum
    sum_f <- function(x){
      y <- x
      for(i in 1:length(x)){
        y[i] <- sum(x[1:i])
      }
      return(y)
    }
    
    # run function within calc
    result <- raster::calc(result, fun = sum_f, forcefun = TRUE)
    
  }
  
  # return result
  return(result)
}