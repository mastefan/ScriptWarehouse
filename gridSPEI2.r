#' gridSPEI2
#' 
#' Calculates pixel-wise SPEI values for Daymet data
#' 
#' Uses raster:: functions
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param prcp File path or raster object. Monthly total precipitation.
#' @param scale Numeric. At which scale should SPEI be calculated 
#' (number of months back from 1, which is current month)?
#' @param start.year Numeric. Default = 1988.
#' @param end.year Numeric. Default = 2014.
#' @param pet.out Logical. Should PET be included in the output?
#' 
#' Note that na.rm is set to TRUE within thornthwaite & spei functions.

gridSPEI2 <- function(tmean = NULL, prcp = NULL, 
                      scale = 1, 
                      start.year = 1988, end.year = 2014,
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
  
  # people speak
  cat("preparing for calculations...")
  
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
  cat("calculating PET...")
  pet_f <- function(x,y){
    as.numeric(SPEI::thornthwaite(Tave = as.vector(x),lat = y, na.rm = TRUE))
  }
  pet <- raster::overlay(x = tmean, y = latitude, fun = Vectorize(pet_f))
  
  # pet - prcp
  int_f <- function(x,y){
    as.numeric(x) - as.numeric(y)
  }
  int <- raster::overlay(x = pet, y = prcp, fun = Vectorize(int_f))
  
  # spei
  cat("calculating SPEI...")
  spei_f <- function(x,y){
    SPEI::spei(data = x,
               scale = y,
               na.rm = TRUE)[[2]]
  }

  scale_r <- raster::raster(tmean[[1]])
  scale_r <- raster::setValues(scale_r, values = scale)
  
  spei <- raster::overlay(x = int, y = scale_r, fun = Vectorize(spei_f), 
                          recycle = TRUE, unstack = FALSE)
  
  # creating output
  if(pet.out == TRUE){
    names(pet) <- sprintf("PET.%s",names(tmean))
    names(spei) <- sprintf("SPEI.%s",names(tmean))
    result <- raster::stack(pet, spei)
  } else {
    names(spei) <- names(tmean)
    result <- spei
  }
  # perhaps it would be useful to include metadata with other outputs from SPEI later...
  
  # return result
  cat("c'est fini...")
  return(result)
}