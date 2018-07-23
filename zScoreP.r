#' Z score calculator with parallel processing

zScoreP <- function(file=NULL, na.rm = TRUE, ncores=NULL, test = FALSE){
  # stop if no data is provided
  if(is.null(file)){
    stop('Please specify a file path.')
  }
  
  # begin parallel processing
  if(!is.null(ncores)){
    raster::beginCluster(n = ncores, type = 'SOCK')
    cat(sprintf('\n executing zScore using a cluster size of %s', ncores))
  } else {
    raster::beginCluster(type = 'SOCK')
  }
  
  # load data
  if(test == FALSE){
    data <- suppressWarnings(raster::brick(file))
  } else {
    data <- raster::brick(raster::stack(file, bands = 1:5))
  }
  
  # calculate sd
  cat('\n calculating standard deviation')
  sd <- raster::clusterR(x = data, fun = raster::calc,
                         args = list(x = data, fun = sd, na.rm = na.rm))

  # calculate mean for each layer (mean of all but band i)
  set <- 1:raster::nbands(data)
  mn <- data
  for (i in set){
    cat(sprintf('\n calculating mean for band %s', i))
    subset <- set[-i]
    wrk <- raster::stack(x = data, bands = subset)
    mn_i <- raster::clusterR(x = wrk, fun = function(wrk){
         raster::calc(x = wrk, fun = mean, na.rm = na.rm)})
  }
  
  # calculate z scores
  cat('\n calculating z scores')
  z <- raster::clusterR(x = data, fun = raster::overlay, 
                        args=list(function(data, mn){data-mn},
                                  na.rm = na.rm), 
                        export = "mn")
  
  # normalize z scores
  cat('\n normalizing z scores')
  z <- raster::clusterR(x = z, fun = raster::overlay,
                        args = list(function(z,sd){z / sd}, na.rm = na.rm),
                        export = "sd")
  
  # return result
  return(z)
}