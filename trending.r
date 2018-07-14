#' trending
#' 
#' Returns the per-pixel slope of a raster
#' Optionally masks the result to include 
#' only significant results at a user-specified
#' threshold.
#' @param file An r object or a character file name
#' @param change Logical, if TRUE the slope will be multiplied by the number of bands
#' @param threshold Significance threshold (e.g. 0.05)
#'
#'https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

trending <- function(file = NULL, change = FALSE, threshold = NULL){
  # input check
  if(is.null(file)){
    stop('Please specify the data source')
  }
  
  # load data
  if(class(file) != "RasterBrick"){
    dat <- suppressWarnings(raster::brick(file, values = TRUE))
  } else {
    dat <- file
  }
  
  # create time variable
  time <- 1:raster::nlayers(dat)
  
  # calculate linear trend
  fun_t <- function(x) {
    if (is.na(x[1])){
      NA 
    } else {
      lm(x ~ time)$coefficients[2] }
    }
  trend <- raster::calc(dat, fun_t)
  
  # calculate change over time
  if(change == TRUE){
    trend <- trend * nlayers(dat)
  }
  
  # if threshold unspecified, return trend
  if(is.null(threshold)){    
    return(trend)
  }
  
  # if threshold specified, return only pixels above threshold
  if(!is.null(threshold)){
    # calculate p-values
    fun_p <- function(x) { 
      if (is.na(x[1])){
        NA 
      } else {
        m = lm(x ~ time); summary(m)$coefficients[8]
      }
    }
    
    p_val <- raster::calc(dat, fun_p)
    
    # reclassify p-values at desired threshold
    m <- c(0, threshold, 1, threshold, 1, 0)
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    p_mask <- raster::reclassify(p_val, rclmat)
    fun <- function(x) { x[x<1] <- NA; return(x) }
    p_mask_na <- raster::calc(p_mask, fun)
    trend_sig <- raster::mask(trend, p_mask_na)
    
    # return significant trends
    return(trend_sig)
  }
  }
  