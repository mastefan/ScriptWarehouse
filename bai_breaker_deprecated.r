bai_breaker <- function(data, plot = FALSE, breaks = NULL){

  # remove NAs if any are present
  if(any(is.na(data))){
    a <- !is.na(data)
    b <- data[a==TRUE]
    data <- ts(b, start = min(as.numeric(names(b))), end = max(as.numeric(names(b))))
  }
  
  # create vector of breakdates with series start and end dates attached
  if(is.null(breaks)){
    dx <- strucchange::breakdates(strucchange::breakpoints(data~time(data), 
                                                           het.err = TRUE))
  } else {
    dx <- strucchange::breakdates(strucchange::breakpoints(data~time(data), breaks = breaks, 
                                                           het.err = TRUE))
  }
  
  years <- as.numeric(time(data))
  
  suppressWarnings(
    if(is.na(dx)){
      # no breakpoints
      dx <- c(min(years-1), max(years))
    } else {
      # roll breakpoints into date series
      dx <- c((min(years)-1), dx, max(years))
    }
  )
  
  # create bins from dates
  bins <- cut(x = as.numeric(time(data)), breaks = dx, labels = FALSE)
  
  # correct lowered start year (necessary to make cut function include left-most value)
  dx[1] <- dx[1]+1
  
  # calculate trends for each segment
  trends <- list()
  for(i in 1:(length(dx)-1)){
    wrk <- window(data, start = dx[i], end = dx[i+1])
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
  
  if(plot == FALSE){
    # if plot = FALSE, produce a dataframe output
    return(out)
  }
  
  if(plot == TRUE){
    # if plot = TRUE, produce a plot
    
    # interpolate values along line segments
    a <- list()
    c <- list()
    for(i in 1:(length(dx)-1)){
      if(i == 1){
        wrk <- window(data, start = dx[i], end = dx[i+1])
      } else {
        wrk <- window(data, start = (dx[i]+1), end = dx[i+1])
      }
      b <- rep(NA, times = length(wrk))
      for(j in 1:length(b)){
        b[j] <- (out[2,i]*j)+out[11,i]
      }
      a[i] <- list(b)
      c[i] <- list(rep(segs[i], times = length(b)))
    }
  
  # create dataframes interpretable by ggplot2
  t_lines <- data.frame(yrs = seq(dx[1], dx[length(dx)], by = 1), val = unlist(a), seg = unlist(c))
  data <- data.frame(yrs = seq(dx[1], dx[length(dx)], by = 1), val = data)
  
  # plot!
  suppressWarnings(
    ggplot2::ggplot(data, ggplot2::aes(x = yrs, y = val)) + 
      ggplot2::theme_light() + 
      ggplot2::geom_line(inherit.aes = TRUE) + 
      ggplot2::geom_line(data = t_lines, ggplot2::aes(x = yrs, y = val, col = seg), lwd = 0.8) + 
      ggplot2::labs(x = "Year", y = "BAI (mm^2/year)")
  )
  }
}