#' gridPETlat3
#' 
#' Calculates pixel-wise latitude (decimal degrees) for a given input
#' 
#' @param tmean File path or raster object. Monthly average temperature.
#' @param parallel Should execution use parallel processing? This can increase 
#' processing time substantially on multi-core systems. Options are: FALSE 
#' (parallel processing should not be used), TRUE (parallel processing should 
#' be used with standard parameters), numeric of length one (parallel processing 
#' should be used, specifically using n cores).

gridPETlat3 <- function(tmean = NULL, parallel = FALSE){
  
  # input check
  if(is.null(tmean)){
    stop('Please include tmean data')
  }
  
  # load data
  tmean <- raster::stack(tmean, 1)
  
  # manually set reference projection
  ref <- raster::projectExtent(tmean, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # project tmean input to reference projection
  if(parallel == FALSE){
    cat("using standard processing...")
    int <- raster::projectRaster(from = tmean, to = ref)
  } 
  if(parallel == TRUE){
    cat("using parallel processing and standard cluster count...\n")
    raster::beginCluster()
    int <- raster::projectRaster(from = tmean, to = ref)
    raster::endCluster()
  }
  if(is.numeric(parallel)){
    cat("using parallel processing and specified cluster count...\n")
    raster::beginCluster(n = parallel)
    int <- raster::projectRaster(from = tmean, to = ref)
    raster::endCluster()
  }
  
  # set values of result
  result <- raster::setValues(tmean[[1]], sp::coordinates(int)[,"y"])
  
  # return result
  cat("c'est fini...")
  return(result)
  
}