function (rwl, diam = NULL) 
{
  
  # input checks
  if (!is.data.frame(rwl)) 
    stop("'rwl' must be a data.frame")
  if (!is.null(diam)) {
    if (ncol(rwl) != nrow(diam)) 
      stop("dimension problem: ", "'ncol(rw)' != 'nrow(diam)'")
    if (!all(diam[, 1] %in% names(rwl))) 
      stop("series ids in 'diam' and 'rwl' do not match")
    diam.vec <- diam[, 2]
  }
  
  # create output variable
  out <- rwl
  
  # number of years (rows) of data
  n.vec <- seq_len(nrow(rwl))
  
  # loop through samples
  for (i in seq_len(ncol(rwl))) {
    
    # get measurements for the i'th sample
    dat <- rwl[[i]]
    
    # get measurement for the i'th sample with NA's omitted
    dat2 <- na.omit(dat)
    
    # estimate the stem diameter if it is not provided
    if (is.null(diam)) { d <- sum(dat2) * 2 } else { d <- diam.vec[i] }
    
    # stem radius minus a sequence from 0 to the outer-most measurement as a cumulative sum
    r0 <- d/2 - c(0, cumsum(rev(dat2)))
    
    # basal area increment
    # pi * measurement difference between years ^ 2
    # pi is negative because differences are negative, so making pi negative makes the output positive
    bai <- -pi * rev(diff(r0 * r0))
    
    # create a vector of non-na values for setting values to appropriate positions in the output
    na <- attributes(dat2)$na.action
    no.na <- n.vec[!n.vec %in% na]
    
    # set BAI for a given year to the appropriate position in the output
    out[no.na, i] <- bai
  }
  
  # return the output
  out
}