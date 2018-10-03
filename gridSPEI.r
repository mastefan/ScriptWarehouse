#' gridSPEI
#' 
#' Calculates pixel-wise SPEI values for Daymet data
#' 
#' Uses for loops and pixel indexing
#' This function is ~4x faster than gridSPEI2 for a small dataset
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param prcp File path or raster object. Monthly total precipitation.
#' @param pet File path or raster object. Monthly potential evapotranspiration.
#' @param scale Numeric. At which scale should SPEI be calculated 
#' (number of months back from 1, which is current month)?
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param na.rm Logical. Passed to SPEI package functions: spei.

gridSPEI <- function(tmean = NULL, prcp = NULL, pet = NULL, 
                     scale = 1, 
                     start.year = 1988, end.year = 2014, 
                     na.rm = TRUE){
  # input check
  if(is.null(tmean) | is.null(prcp)){
    stop('Please include tmean and prcp data')
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
  
  # spei
  spei <- raster::brick(tmean, values = FALSE)
  cat("calculating SPEI...\n")
  pb2 = utils::txtProgressBar(min = 0, max = raster::ncell(tmean),
                              initial = 0, style = 3)
  for(i in 1:raster::ncell(tmean)){
    dat <- (as.vector(pet[i]) - as.vector(prcp[i]))
    dat <- ts(dat, start = c(start.year, 1), 
              end = c(end.year, 12), 
              frequency = 12)
    spei_i <- SPEI::spei(data = dat,
                         scale = scale,
                         na.rm = na.rm)
    spei[i] <- as.vector(spei_i$fitted)
    utils::setTxtProgressBar(pb2,i)
  }
  
  # creating output
  result <- spei
  result <- raster::setZ(spei, dates, name = "time")
  names(result) <- zoo::as.yearmon(raster::getZ(tmean))
  
  # return result
  cat("\nc'est fini...")
  return(result)

}