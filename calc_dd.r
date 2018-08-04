#' calc_dd
#' 
#' @param tmean
#' @param threshold
#' @param index options are "grow" or "chill"
#' @param accumulate T/F
#' @param start
#' @param end
#' 

calc_dd <- function(tmean = NULL, threshold = NULL, index = NULL, start = 1, end = 365){
  
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
  
  # accumulate
  if(accumulate == TRUE){
    # create new stack
    daily <- raster::stack(x = result, layers = start:end)
    
    # calculate daily backward-looking sum
    for(i in 1:raster::nlayers(wrk)){
      daily[[i]] <- raster::calc(x = raster::stack(daily, bands = 1:i), fun = sum, na.rm = TRUE)
    }
    
    # calculate total sum
    total <- raster::calc(x = daily, fun = sum, na.rm = TRUE)
    
    # combine results
    names(daily) <- 1:365
    names(total) <- "total"
    result <- raster::brick(daily, total)
  }
  
  # return result
  return(result)
}