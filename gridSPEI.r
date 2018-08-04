#' gridSPEI
#' 
#' Calculates pixel-wise SPEI values for Daymet data
#' 
#' Uses for loops and pixel indexing
#' This function is ~4x faster than gridSPEI2 for a small dataset
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param prcp File path or raster object. Monthly total precipitation.
#' @param scale Numeric. At which scale should SPEI be calculated 
#' (number of months back from 1, which is current month)?
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param na.rm Logical. Passed to SPEI package functions: thornthwaite & spei.
#' @param pet.out Logical. Should PET be included in the output?

gridSPEI <- function(tmean = NULL, prcp = NULL, 
                     scale = 1, 
                     start.year = 1988, end.year = 2014, 
                     na.rm = TRUE,
                     pet.out = FALSE){
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
  if(class(tmean) != "RasterBrick"){tmean <- raster::brick(tmean, values = TRUE)}
  if(class(prcp)  != "RasterBrick"){prcp  <- raster::brick(prcp,  values = TRUE)}
  
  # dates
  dates <- seq(as.Date(sprintf("%s-01-15", start.year)),
               as.Date(sprintf("%s-12-15", end.year)),
               by = "month")
  tmean <- raster::setZ(tmean, dates, name = "time")
  names(tmean) <- zoo::as.yearmon(raster::getZ(tmean))
  prcp <- raster::setZ(prcp, dates, name = "time")
  names(prcp) <- zoo::as.yearmon(raster::getZ(prcp))
  
  # latitude
  ref <- raster::projectExtent(tmean, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  int <- raster::projectRaster(from = tmean, to = ref)
  latitude <- raster::setValues(tmean[[1]], sp::coordinates(int)[,"y"])
  
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
  
  # spei
  spei <- raster::brick(tmean, values = FALSE)
  cat("\ncalculating SPEI...\n")
  pb2 = utils::txtProgressBar(min = 0, max = raster::ncell(tmean),
                              initial = 0, style = 3)
  for(i in 1:raster::ncell(tmean)){
    spei_i <- SPEI::spei(data = (as.vector(pet[i]) - as.vector(prcp[i])),
                          scale = scale,
                          na.rm = na.rm)
    spei[i] <- as.vector(spei_i[[2]])
    utils::setTxtProgressBar(pb2,i)
  }
  
  # creating output
  if(pet.out == TRUE){
    names(pet) <- sprintf("PET.%s",names(tmean))
    names(spei) <- sprintf("SPEI.%s",names(tmean))
    result <- raster::stack(pet, spei)
  } else {
    result <- raster::setZ(spei, dates, name = "time")
    names(result) <- zoo::as.yearmon(raster::getZ(tmean))
    result <- spei
  }
  # perhaps it would be useful to include metadata with other outputs from SPEI later...
  
  # return result
  cat("\nc'est fini...")
  return(result)
}