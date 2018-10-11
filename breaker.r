#' breaker
#' 
#' Identifies breakpoints in input data
#' 
#' @param dat Numeric vector of input data (time ordered). If a ts object is 
#' provided then other parameters are ignored.
#' @param breaks Single numeric specifying the number of breakpointss to seek
#'  in a dataset.
#' @param start As in ts. For annual data, a numeric year can be provided.
#' @param end As in ts. For annual data, a numeric year can be provided.
#' @param frequency As in ts. For annual data, specify 1.

breaker <- function(dat = NULL,
                    breaks = NULL,
                    start = NULL, end = NULL, 
                    frequency = NULL){
  
  # input checks
  if(is.null(dat)){stop("please specify data source")}
  if(class(dat) != "ts"){
   # check that other parameters are specifid if dat is not a time series 
  }
  
  # convert to a time series if necessary
  if(class(dat) != "ts"){
    dat <- ts(data = dat, start = start, end = end, frequency = frequency)
  }
  
  # omit NA's
  dat <- na.omit(dat)
  
  # create vector of breakdates with series start and end dates attached
  if(is.null(breaks)){
    dx <- strucchange::breakdates(strucchange::breakpoints(dat~time(dat), 
                                                           het.err = TRUE))
  } else {
    dx <- strucchange::breakdates(strucchange::breakpoints(dat~time(dat), 
                                                           breaks = breaks, 
                                                           het.err = TRUE))
  }
  
  # get vector of time from ts
  tm <- as.numeric(time(dat))
  
  # add start and end of time series to breakpoints vector
  suppressWarnings(
    if(is.na(dx)){
      # no breakpoints
      dx <- c(min(tm-1), max(tm))
    } else {
      # roll breakpoints into date series
      dx <- c((min(tm)-1), dx, max(tm))
    }
  )
  
  # create bins from dates
  bins <- cut(x = tm, breaks = dx, labels = FALSE)
  
  # correct lowered start year (necessary to make cut function include left-most value)
  dx[1] <- dx[1]+1
  
  # calculate trends for each segment
  trends <- list()
  for(i in 1:(length(dx)-1)){
    wrk <- window(dat, start = dx[i], end = dx[i+1])
    trends[i] <- list(zyp::zyp.trend.vector(y = wrk, method = "yuepilon"))
  }
  
  # convert nested list to dataframe
  out <- as.data.frame(trends)
  
  # create and assign nice bin labels
  segs <- rep(NA, times = (length(dx)-1))
  for(i in 1:length(segs)){
    if(i == 1){
      segs[i] <- paste0(dx[i], "-", dx[i+1])
    } else {
      segs[i] <- paste0((dx[i]+1), "-", dx[i+1])
    }
  }
  colnames(out) <- segs
  
  # return result
  return(out)
  
}