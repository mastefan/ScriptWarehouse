#' gridSPEI3
#' 
#' Calculates pixel-wise SPEI values for Daymet data.
#' Note that before SPEI can be calculated, an intermediary calculation 
#' must be done (int = PET - Precipitation).
#' 
#' Fastest working version! Use this one!
#' 
#' Uses raster::overlay
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param prcp File path or raster object. Monthly total precipitation.
#' @param pet File path or raster object. Monthly potential evapotranspiration.
#' @param scale Numeric. At which scale should SPEI be calculated 
#' (number of months back from 1, which is current month)?
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param na.rm Logical. Passed to SPEI package functions: spei.

gridSPEI3 <- function(tmean = NULL, prcp = NULL, pet = NULL, 
                      scale = 1, 
                      start.year = 1988, end.year = 2014, 
                      na.rm = TRUE){
  
  # input check
  if(is.null(tmean) | is.null(prcp) | is.null(pet)){
    stop('Please include tmean, prcp and pet data')
  }
  if(class(start.year) != "numeric" | class(end.year) != "numeric"){
    stop('start.year and end.year should be numeric')
  }
  if(class(scale) != "numeric"){
    stop('scale should be numeric')
  }
  
  # load data
  if(class(tmean) != "RasterBrick"){
    tmean <- raster::brick(tmean)
  }
  if(class(prcp)  != "RasterBrick"){
    prcp <- raster::brick(prcp)
  }
  if(class(pet) != "RasterBrick"){
    pet <- raster::brick(pet)
  }
  
  # dates
  dates <- seq(as.Date(sprintf("%s-01-15", start.year)),
               as.Date(sprintf("%s-12-15", end.year)),
               by = "month")
  tmean <- raster::setZ(tmean, dates, name = "time")
  names(tmean) <- zoo::as.yearmon(raster::getZ(tmean))
  prcp <- raster::setZ(prcp, dates, name = "time")
  names(prcp) <- zoo::as.yearmon(raster::getZ(prcp))
  
  # intermediary
  int_f <- function(pe,pr){
    return(pe - pr)
  }
  cat("calculating PET - PRCP...")
  int <- raster::overlay(x = pet, y = prcp, fun = Vectorize(int_f), progress = "text")
  
  # spei
  spei_f <- function(x){
    return(as.numeric(SPEI::spei(data  = x, scale = scale, na.rm = na.rm)$fitted))
  }
  spei <- raster::calc(x = int, fun = spei_f)
  
  # creating outputs
  names(spei) <- names(pet)
  result <- spei
  
  # return result
  cat("c'est fini...")
  return(result)
  
}