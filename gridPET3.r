#' gridPET3
#' 
#' Calculates pixel-wise SPEI values for Daymet data
#' 
#' Fastest working version! Use this one!
#' 
#' Uses raster::overlay
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param latitude File path or raster object. Output from gridPETlat.
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param na.rm Logical. Passed to SPEI package functions: thornthwaite

gridPET3 <- function(tmean = NULL, latitude = NULL,
                     start.year = 1988, end.year = 2014,
                     na.rm = TRUE){
  
  # input check
  if(is.null(tmean | latitude)){
    stop('Please include tmean and latitude data')
  }
  if(class(start.year) != "numeric" | class(end.year) != "numeric"){
    stop('start.year and end.year should be numeric')
  }
  
  # load data
  if(class(tmean) != "RasterBrick"){
    tmean <- raster::brick(tmean)
  }
  if(class(latitude) != "RasterLayer"){
    latitude <- raster::brick(latitude)
  }
  
  # dates
  dates <- seq(as.Date(sprintf("%s-01-15", start.year)),
               as.Date(sprintf("%s-12-15", end.year)),
               by = "month")
  tmean <- raster::setZ(tmean, dates, name = "time")
  names(tmean) <- zoo::as.yearmon(raster::getZ(tmean))
  
  # potential evapotranspiration
  pet_f <- function(x,y){
    return(as.numeric(SPEI::thornthwaite(Tave = x,lat = y, na.rm = na.rm)))
  }
  cat("calculating PET...")
  pet <- raster::overlay(x = tmean, y = latitude, fun = Vectorize(pet_f), 
                         recycle = TRUE, progress = "text")
  
  # format output
  pet <- raster::setZ(pet, dates, name = "time")
  names(pet) <- zoo::as.yearmon(raster::getZ(tmean))
  
  # return result
  cat("c'est fini...")
  return(pet)
  
}