#' raster2random - Stage 1
#' 
#' Takes a named list of raster bricks and converts it to a long-format 
#' dataframe for use with the randomForest function
#' 
#' @param in.data Named list of input rasters. The independent variable
#'  should be first on the list.
#' @param na.rm logical Should rows where independent data = NA be removed?

raster2random <- function(in.data = NULL, na.rm = FALSE){
  
  # check that the inputs are a list
  if(class(in.data) != "list"){
    stop("input data must be given as a list")
  }
  
  # name bands of input rasters
  for(i in 1:length(in.data)){
    if(raster::nlayers(in.data[[i]]) == 26){
      dates <- seq(from = 1989, to = 2014, by = 1)
      names(in.data[[i]]) <- dates
    }
    if(raster::nlayers(in.data[[i]]) == 312){
      dates <- zoo::as.yearmon(seq(from = as.Date("1989-01-15"),
                                   to = as.Date("2014-12-15"),
                                   by = "month"))
      names(in.data[[i]]) <- dates
    }
  }
  
  # create an output list
  out <- list()
  
  ## loop through items on list
  for(i in 1:length(in.data)){
    # annual data
    if(raster::nlayers(in.data[[i]]) == 26){
      o <- raster::as.data.frame(in.data[[i]])
      o <- data.frame(pix = 1:nrow(o), o)
      o <- reshape2::melt(o, id = "pix")
      colnames(o) <- c("pix","time",names(in.data[i]))
      out[i] <- list(o)
    }
    # monthly data
    if(raster::nlayers(in.data[[i]]) == 312){
      o <- raster::as.data.frame(in.data[[i]])
      colnames(o) <- paste(names(in.data[i]), rep(month.abb, 26), sep = ".")
      o <- sapply(unique(names(o)), function(x) unname(unlist(o[,names(o)==x])))
      out[i] <- list(o)
    }
  }
  
  # convert to dataframe
  out <- as.data.frame(out)
  
  if(na.rm == TRUE){
    # filter out rows where the first value column is NA
    out <- dplyr::filter(out, !is.na(out[,1]))
  }
  
  
  # assign rownames to dataframe
  ## pixel X year
  rownames(out) <- paste0(out$pix, out$time)
  
  # remove pix and time columns
  out <- out[,3:ncol(out)]
  
  # return result
  return(out)
}