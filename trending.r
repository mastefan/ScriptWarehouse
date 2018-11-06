#' trending
#' 
#' Returns the per-pixel slope of a raster
#' Optionally masks the result to include 
#' only significant results at a user-specified
#' threshold.
#' @param file An r object or file path to load using raster::brick
#' @param method Either "lm" to use a standard linear model or "yp" to use prewhitened sen slope
#' @param threshold Significance threshold (e.g. 0.05)
#'
#'https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

trending <- function(file = NULL, method = NULL, threshold = NULL){

    # input check
  if(is.null(file)){
    stop('Please specify the data source')
  }
  if(is.null(method)){
    stop('Please specify modelling method')
  }
  
  # load data
  if(class(file) != "RasterBrick"){
    dat <- suppressWarnings(raster::brick(file, values = TRUE))
  } else {
    dat <- file
  }
  
  # function to calculate trend: yue-pilon prewhiteneing, sen slope & mann-kendall significance tests
  # drift years are removed in this stage
  if(method == "yp"){
    fun_t <- function(dat){
      if(any(is.na(dat))){
        NA
      } else {
        wrk <- ts(data = as.numeric(dat), start = 1989, end = 2014, frequency = 1)
        for(i in c(1992, 1993, 1994, 1999, 2000)){
          wrk[time(wrk) == i] <- NA
        }
        m <- zyp::zyp.trend.vector(y = wrk, x = time(wrk), method = "yuepilon")
        unname(m["trend"])
      }
    }
  }
  if(method == "lm"){
    fun_t <- function(dat){
      if(any(is.na(dat))){
        NA
      } else {
        wrk <- ts(data = as.numeric(dat), start = 1989, end = 2014, frequency = 1)
        for(i in c(1992, 1993, 1994, 1999, 2000)){
          wrk[time(wrk) == i] <- NA
        }
        m <- lm(wrk ~ time(wrk))
        unname(coefficients(m)[2])
      }
    }
  }
  
  # calculate trend
  trend <- raster::calc(x = dat, fun = fun_t)
  
  # if threshold unspecified, return trend
  if(is.null(threshold)){    
    return(trend)
  }
  
  # if threshold specified, return only pixels above threshold
  if(!is.null(threshold)){
    # function to extract significance
    if(method == "yp"){
      fun_p <- function(dat){
        if(any(is.na(dat))){
          NA
        } else {
          wrk <- ts(data = as.numeric(dat), start = 1989, end = 2014, frequency = 1)
          for(i in c(1992, 1993, 1994, 1999, 2000)){
            wrk[time(wrk) == i] <- NA
          }
          m <- zyp::zyp.trend.vector(y = wrk, x = time(wrk), method = "yuepilon")
          unname(m["sig"])
        }
      }
    }
    if(method == "lm"){
      fun_p <- function(dat){
        if(any(is.na(dat))){
          NA
        } else {
          wrk <- ts(data = as.numeric(dat), start = 1989, end = 2014, frequency = 1)
          for(i in c(1992, 1993, 1994, 1999, 2000)){
            wrk[time(wrk) == i] <- NA
          }
          m <- lm(wrk ~ time(wrk))
          unname(summary(m)$coefficients[8])
        }
      }
    }
    
    # calculate p-values
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
  