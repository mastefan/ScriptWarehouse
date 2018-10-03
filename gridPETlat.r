#' gridPETlat
#' 
#' Calculates pixel-wise latitude (decimal degrees) for a given input
#' 
#' Uses for loops and pixel indexing
#' 
#' @param tmean File path or raster object. Monthly average temperature.

gridPETlat <- function(tmean = NULL){
  
  # input check
  if(is.null(tmean)){
    stop('Please include tmean data')
  }
  
  # load data
  if(class(tmean) != "RasterBrick"){
    tmean <- raster::stack(tmean, 1)
  }
  
  # manually set reference projection
  ref <- raster::projectExtent(tmean, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # project tmean input to reference projection
  int <- raster::projectRaster(from = tmean, to = ref)
  
  # set values of result
  latitude <- raster::setValues(tmean[[1]], sp::coordinates(int)[,"y"])
  
  # return result
  return(latitude)
  
}