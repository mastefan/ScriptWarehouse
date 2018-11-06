#' raster2random - Stage 1
#' 
#' Takes a named list of raster bricks and converts it to a long-format 
#' dataframe for use with the randomForest function
#' 
#' Stage 1: Creates a dataframe of a given climate variable
#' 
#' @param in.data Named list of input rasters. Can handle annual or 
#' monthly data.
#' @param months Numeric vector of months (columns) to include. Ignored if 
#' data is at an annual timestep.
#' 
#' Note that band naming is done automatically, and can account for data 
#' where orbital drift years have been removed

raster2random_s1 <- function(in.data = NULL, months = 1:12){
  
  # check that the inputs are a list
  if(class(in.data) != "list"){
    stop("input data must be given as a list")
  }
  
  # name bands of input rasters
  for(i in 1:length(in.data)){
    # annual
    if(raster::nlayers(in.data[[i]]) == 26){
      dates <- seq(from = 1989, to = 2014, by = 1)
      names(in.data[[i]]) <- dates
    }
    # annual no drift
    if(raster::nlayers(in.data[[i]]) == 21){
        dates <- seq(from = 1989, to = 2014, by = 1)
        nd_p_band <- c(1:3, 7:10, 13:26)
        dates <- dates[nd_p_band]
        names(in.data[[i]]) <- dates
      }
    # monthly
    if(raster::nlayers(in.data[[i]]) == 312){
      dates <- zoo::as.yearmon(seq(from = as.Date("1989-01-15"),
                                   to = as.Date("2014-12-15"),
                                   by = "month"))
      names(in.data[[i]]) <- dates
    }
    # monthly no drift
    if(raster::nlayers(in.data[[i]]) == 264){
      nd_name <- c(1988:1991, 1995:1998, 2001:2014)
      dates <- paste(month.abb, rep(nd_name, each = 12), sep = ".")
      names(in.data[[i]]) <- dates
    }
  }
  
  # create an output list
  out <- list()
  
  ## loop through items on list
  for(i in 1:length(in.data)){
    # annual data
    if(raster::nlayers(in.data[[i]]) < 28){
      o <- raster::as.data.frame(in.data[[i]])
      o <- data.frame(pix = 1:nrow(o), o, stringsAsFactors = FALSE)
      o <- reshape2::melt(o, id = "pix")
      out[i] <- list(data.frame(o[,3]))
      colnames(out[[i]]) <- (names(in.data)[i])
      #names(out[i]) <- names(in.data[i])
    }
    # monthly
    if(raster::nlayers(in.data[[i]]) > 28){
      o <- raster::as.data.frame(in.data[[i]])
      colnames(o) <- paste(names(in.data[i]), 
                           rep(month.abb, (raster::nlayers(in.data[[i]])/12), sep = "."))
      o <- o[,months]
      o <- sapply(unique(names(o)), function(x) unname(unlist(o[,names(o)==x])))
      out[i] <- list(o)
    }
  }
  
  # convert to dataframe
  out <- as.data.frame(out)
  
  ## assign rownames to dataframe
  ### pixel X year
  #rownames(out) <- paste0(out$pix, out$time)
  
  ## remove pix and time columns
  #out <- out[,3:ncol(out)]
  
  # return result
  return(out)
}