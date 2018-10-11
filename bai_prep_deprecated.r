#' BAI prep
#' 
#' Takes a dataframe from dplR::bai.out and returns the mean BAI 
#' of a given plot as a time series. Can also constrain the time 
#' series to a given window.
#' 
#' @param in.data Dataframe from dplR::bai.out
#' @param in.plot Character "in quotes", indicating which plot should be extracted 
#' (not case sensitive).
#' @param window Numeric vector of length = 2, with the first value indicating the 
#' desired start year and the second value indicating the desired end year 
#' (e.g. c(1909, 2008))

bai_prep <- function(in.data, in.plot, biweight = FALSE, window = NULL){

  if(biweight == TRUE){
    
    # use biweight mean
    a = dplyr::select(in.data, grep(x = colnames(in.data), 
                                    ignore.case = TRUE, 
                                    pattern = in.plot))
    a <- dplR::chron(a, prefix = "", biweight = TRUE, prewhiten = TRUE)
    b <- data.frame(yr = as.numeric(rownames(a)), std = a$std, samp.depth = a$samp.depth)
    c <- dplyr::filter(b, samp.depth > 0)
    
    if(!is.null(window)){
      c <- dplyr::filter(c, dplyr::between(c$yr, window[1], window[2]))
    }
    
    d <- ts(data = c$std, st = min(c$yr))
    
  } else {
    
    # use arithmetic mean
    a = dplyr::select(in.data, grep(x = colnames(in.data), 
                                    ignore.case = TRUE, 
                                    pattern = in.plot))
    a <- rowMeans(a, na.rm = TRUE)
    d <- ts(a, start = min(as.numeric(rownames(in.data))), 
            end = max(as.numeric(rownames(in.data))))
    
    if(!is.null(window)){
      d <- window(d, window[1], window[2])
    }
    
  }

  return(d)

}