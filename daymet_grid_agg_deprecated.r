#' This is a deprecated version, retained to show a code example using 
#' base::aggregate application
#' 
#' Aggregates daily 'Daymet' gridded products to monthly or annual
#' time scales and creates a gridded (geotiff) output or raster 
#' object for easy post processing and modelling.
#' 
#' This is intended to work for swe, dayl and srad data, for which
#' ORNL-DAAC does not provide aggregated data.
#'
#' @param file character string of name of file to be aggregated
#' @param year Necessary if working with file in tif format. Provide
#'  the year that the data in file represents
#' @param int interval to aggregate by. Options are "monthly" or "annual"
#' @param FUN function to be used to aggregate data. Genertic R functions. 
#' "mean" and "sum" are suggested.
#' @param internal TRUE / FALSE (if FALSE, write the output to file)
#' using the Daymet file format protocol.

daymet_grid_agg = function(file = NULL,
                           year = NULL,
                           int = NULL,
                           FUN = NULL,
                           internal = FALSE){
  
  # stop on missing parameters
  if (is.null(file)|is.null(int)|is.null(FUN)) {
    stop('One or more parameters are missing. 
         Please specify all parameters.')
  }
  
  # get file extension
  ext <- tools::file_ext(file)
  
  if(is.null(year) & ext =='tif'){
    stop("Please specify the year for tif files.")
  }
  
  # load data into a raster brick
  data <- suppressWarnings(raster::brick(file))
  
  # extract time variable from data and covert to date format
  if (ext == 'tif' | ext == 'nc'){
    
    if (ext == 'nc'){
      dates <- as.Date(
        sub(pattern = "X",
            replacement = "",
            x = names(data)),
            format = "%Y.%m.%d")
      
    } else {
      dy <- as.numeric(as.vector(t(as.data.frame(strsplit(names(data),
                                                          "[.]"))[2,])))
      dates <- as.Date(x = dy, origin = sprintf('%s-01-01',year))
      
    }
    
  } else {
    stop('Unable to read dates.\n
         Files must be outputs from daymetr functions in tif or nc format.')
    
  }
  
  # use int to create list of values to be used by aggregate function
  if (int == 'monthly'){
    interval <- months(dates)
  }
  if (int=='annual'){
    interval <- as.numeric(substring(dates,1,4))
  }
  
  # create working dataset
  data.wrk <- raster::as.matrix(data,
                                na.rm = TRUE)
  data.wrk <- as.data.frame(t(data.wrk),
                            stringsAsFactors = FALSE)
  data.wrk <- cbind(interval,
                    data.wrk,
                    stringsAsFactors = FALSE)
  
  # aggregate bands by int using base R function aggregate
  result <- suppressWarnings(aggregate(x = data.wrk,
                                       by = list(data.wrk$interval),
                                       FUN = FUN,
                                       na.rm = TRUE))
  
  # formatting result and adding names
  if (int == 'monthly'){
    #sort months chronologically
    result$interval <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
    result <- result[order(result$interval),]
  }
  
  row.names(result) <- result$Group.1
  result <- result[,3:length(result)]
  result <- as.matrix(result)
  
  # return all data to raster, either as a geotiff or as a local object
  if (internal == FALSE){
      # create output file name
      input_file <- tools::file_path_sans_ext(file)
      output_file <- sprintf('%s_%s.tif', input_file, int)
      
      # get raster prarameters from data
      crs <- raster::crs(data)
      ext <- raster::extent(data)
      nr <- raster::nrow(data)
      nc <- raster::ncol(data)
      
      # convert to raster object
      if (nrow(result) > 1){
        # convert to brick for monthly data
        nrows <- nrow(result)
        result <- t(result)
        dim(result) <- c(nc,nr,nrows)
        result <- aperm(result, 
                        c(2,1,3))
        result.ras <- raster::brick(crs = crs,
                                    nrows = nr,
                                    ncols = nc,
                                    nl = nrows)
        result.ras <- raster::setValues(result.ras,
                                        result)
        raster::extent(result.ras) <- ext
        
      } else {
        # convert to standard raster for annual data
        dim(result) <- c(nc,nr)
        result <- t(result)
        result.ras <- raster::raster(crs = crs,
                                     ext = ext,
                                     nrows = nr,
                                     ncols = nc)
        result.ras <- raster::setValues(result.ras,
                                        result)
      }
      
      # write raster object to file
      raster::writeRaster(x = result.ras,
                          filename = output_file,
                          overwrite = TRUE)
  } else {
    # return to workspace
    return(result.ras)
  }
}