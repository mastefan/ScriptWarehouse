#' Calculates the number of days that meet a condition and returns
#' a spatially-explicit result.
#' 
#' @param data string containing the name of a file
#'  containing the daily Daymet data to be evaluated
#' @param startday numeric day of year at which counting should begin. 
#' Default = 0 (January 1)
#' @param endday numeric day of year at which counting should end. 
#' Default = 365 (December 31)
#' @param criteria logical expression (e.g. ">") to be evaluated
#' @param value numeric the value that the criteria is to be evaluated
#'  against
#' @param internal TRUE / FALSE (if FALSE, write the output to file)
#' using the Daymet file format protocol.
#' 
#' @description This tool counts the number of days in a given time period
#' that meet a given set of criteria. This can be used to extract indices 
#' such as Growing Degree Days (tmin > 0), or days with precipitation 
#' (prcp != 0).

calc_nd <- function(data = NULL,
                    startday = 0,
                    endday = 365,
                    criteria = NULL,
                    value = NULL,
                    internal = FALSE){
  
  # perform input checks
  if(is.null(data) | is.null(criteria) | is.null(value)){
    stop('Please specify data, criteria and value.')
  }
  
  # load data and convert to dataframe
  data <- suppressWarnings(raster::brick(data))
  data.df <- raster::as.data.frame(data,
                                   stringsAsFactors = FALSE)
  
  # select desired dates from data to create working dataset
  data.wrk <- as.data.frame(data.df[ , ,startday:endday],
                            stringsAsFactors = FALSE)
  
  # use boolean indexing in sapply to subset working data
  sel <- sapply(data.wrk,
                criteria,
                value)
  
  # convert df to array
  nr       <- raster::nrow(data)
  nc       <- raster::ncol(data)
  nbnd     <- ncol(sel)
  dim(sel) <- c(nr,nc,nbnd)
  
  # convert array to raster
  crs      <- raster::crs(data)
  ext      <- raster::extent(data)
  sel.ras  <- raster::brick(crs = crs,
                            nrows = nr,
                            ncols = nc,
                            nl = nbnd)
  sel.ras <- raster::setValues(sel.ras,
                               sel)
  
    
  # use SUM to gather the number of days that meet the criteria
  result <- raster::calc(sel.ras,
                         sum,
                         na.rm=TRUE)
  
  # return all data to raster, either as a geotiff or as a local object
  if (internal == FALSE){
    # create output file name
    year <- strsplit(input_file, "_")[[1]][3]
    output_file <- sprintf('nd_%s.tif',year)
    
    # write result to file
    raster::writeRaster(result,
                        output_file,
                        overwrite = TRUE)
  } else {
    # return result
    return(result)
  }
}