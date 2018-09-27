#' calc_dd
#' 
#' @param tmean daily average temperature data for one year (365 days)
#' @param threshold temperature threshold (use same units as data)
#' @param index options are "grow" or "chill"
#' @param accumulate should degree days be accumulated? TRUE / FALSE
#' @param start starting day (1 - 365)
#' @param end ending day (1 - 365)
#' 
#' Note that na.rm is set to TRUE

calc_dd <- function(tmean = NULL, threshold = NULL, index = NULL, start = 1, end = 365, accumulate = FALSE){
  
  # input checks
  if(is.null(tmean)) { stop("please provide tmin and tmean data") }
  if(is.null(threshold)) { stop("please establish a threshold") }
  if(is.null(index)) { stop("please specify the index to be calculated") }
  
  # load data
  if(class(tmean) != "RasterBrick"){ tmean <- raster::brick(tmean, values = TRUE) }
  if(raster::nlayers(tmean) < 365){ stop("Please provide daily tmean data") }
  if(raster::nlayers(tmean) > 365){ stop("Please provide a single year of data") }

  # growing degree days function
  if(index == "grow"){
    # set function
    f <- function(x,y){
      for(i in 1:length(x)){
        if(x[i] > y){
          x[i] <- x[i] - y
        } else {
          x[i] <- 0
        }
      }
      return(x)
    }
  }
  
  # chilling degree days
  if(index == "chill"){
    # set function
    f <- function(x,y){
      for(i in 1:length(x)){
        if(x[i] < y){
          x[i] <- y - x[i]
        } else {
          x[i] <- 0
        }
      }
      return(x)
    }
  }
  
  # create threshold raster
  thresh <- raster::raster(tmean[[1]])
  thresh <- raster::setValues(thresh, values = threshold)
  
  # run function
  result <- raster::overlay(x = tmean, y = thresh, 
                            fun = Vectorize(f), recycle = TRUE)
  
  # accumulate if told to do so
  if(accumulate == TRUE){
    # create new stacks
    a <- raster::brick(raster::stack(x = result, layers = start:end), values = TRUE)
    
    # function to calculate accumulated sum
    sum_f <- function(x){
      y <- x
      for(i in 1:length(x)){
        y[i] <- sum(x[1:i])
      }
      return(y)
    }
    
    # run function within calc
    b <- raster::calc(a, fun = sum_f, forcefun = TRUE)
    
    # format results
    names(b) <- start:end
    result <- b
  }
  
  # return result
  return(result)
}