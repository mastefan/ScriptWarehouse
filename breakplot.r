#' breakplot
#' 
#' @param bre Output from breaker
#' @param dat Input to breaker
#' @param start As in ts.
#' @param end As in ts.
#' @param frequency As in ts.

breakplot <- function(bre = NULL, dat = NULL, 
                      start = NULL, end = NULL, 
                      frequency = NULL){
  
  # convert dat to a time series if necessary
  if(class(dat) != "ts"){
    dat <- ts(data = dat, start = start, end = end, frequency = frequency)
  }
  
  # omit NAs
  dat <- na.omit(dat)
  
  # backtrack and recreate dx
  cn <- strsplit(x = colnames(bre), split = "-")
  if(length(cn) < 2){
    dx <- c(cn[[1]][1])
  } else {
    dx <- list()
    for(i in 2:length(cn)){
      if(i == 2){
        dx[[i-1]]   <- as.numeric(cn[[i-1]][i-1])
        dx[[i]] <- as.numeric(cn[[i-1]][i])
      }
      if(i > 2){
        dx[[i]]   <- as.numeric(cn[[i-1]][2])
      }
    }
  }
  dx <- as.numeric(c(unlist(dx), unlist(cn)[length(unlist(cn))]))
  
  # backtrack and recreate segs
  segs <- colnames(bre)
  
  # interpolate values along line segments
  a <- list()
  c <- list()
  for(i in 1:(length(dx)-1)){
    if(i == 1){
      wrk <- window(dat, start = dx[i], end = dx[i+1])
    } else {
      wrk <- window(dat, start = (dx[i]+1), end = dx[i+1])
    }
    b <- rep(NA, times = length(wrk))
    for(j in 1:length(b)){
      b[j] <- (bre[2,i]*j)+bre[11,i]
    }
    a[i] <- list(b)
    c[i] <- list(rep(segs[i], times = length(b)))
  }
  
  # create dataframes interpretable by ggplot2
  t_lines <- data.frame(yrs = time(dat), val = unlist(a), seg = unlist(c))
  dat <- data.frame(yrs = as.numeric(time(dat)), val = as.numeric(dat))
  
  # plot!
  pt <- suppressMessages(
    ggplot2::ggplot(data = dat, ggplot2::aes(x = yrs, y = val)) + 
      ggplot2::theme_light() + 
      ggplot2::geom_line(inherit.aes = TRUE) + 
      ggplot2::geom_line(data = t_lines, ggplot2::aes(x = yrs, y = val, col = seg), lwd = 0.8) + 
      ggplot2::labs(x = "Year", y = "BAI (mm^2/year)"))
  pt$labels$colour <- "Trend Lines"
  
  return(pt)
}