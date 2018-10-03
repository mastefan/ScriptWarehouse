#' gridPET
#' 
#' Calculates pixel-wise PET values for Daymet data
#' 
#' Uses for loops and pixel indexing
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param latitude File path or raster object. Output from gridPETlat.
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param na.rm Logical. Passed to SPEI package functions: thornthwaite

gridPET <- function(tmean = NULL, latitude = NULL,
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
  if(class(latitude) != "Raster"){
    latitude <- raster::brick(latitude)
  }
  
  # dates
  dates <- seq(as.Date(sprintf("%s-01-15", start.year)),
               as.Date(sprintf("%s-12-15", end.year)),
               by = "month")
  tmean <- raster::setZ(tmean, dates, name = "time")
  names(tmean) <- zoo::as.yearmon(raster::getZ(tmean))
  
  # potential evapotranspiration
  pet <- raster::brick(tmean, values = FALSE)
  cat("calculating PET...\n")
  pb1 = utils::txtProgressBar(min = 0, max = raster::ncell(tmean),
                              initial = 0, style = 3)
  for(i in 1:raster::ncell(tmean)){
    pet_i <- SPEI::thornthwaite(Tave = as.vector(tmean[i]),
                                lat = latitude[i],
                                na.rm = na.rm)
    pet[i] <- as.vector(pet_i)
    utils::setTxtProgressBar(pb1,i)
  }
  
  # format output
  pet <- raster::setZ(pet, dates, name = "time")
  names(pet) <- zoo::as.yearmon(raster::getZ(tmean))
  
  # return result
  cat("\nc'est fini...")
  return(pet)
  
}
