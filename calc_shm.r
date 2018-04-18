#' Summer heat moisture index calculation
#' 
#' Based on formula from: Wang, T., Hamann, A., Yanchuk, A., O'neill, G. A., Aitken, S. N., 2006, 
#' Use of response functions in selecting lodgepole pine populations for future climates, 
#' Global Change Biology, 12(12) 2404-2416.
#' 
#' Formula: SHM = MWMT/(MSP/1000)
#' SHM = Summer Heat Moisture Index
#' MWMT = Mean Warmest Month Temperature
#' MSP = Mean Summer Precipitation
#' 
#' @param temp vector containing the file name/path of monthly aggregate temperature data,
#'  which can be downloaded using the download_daymet functions
#' @param prcp vector containing the file name/path of monthly mean precipitation data
#' derived from daily total precipitation data using the daymet_grid_agg function
#' @param year the year that the data represents. This is used for naming the output.
#' @param internal logical indicating whether to write the output to a file (FALSE)
#' or to output it to the current workspace (TRUE)
#' @param output_directory vector (optional) A path to a 
#' directory where output files should be written. Used only if 
#' internal = FALSE.

calc_shm <- function(temp = NULL,
                     prcp = NULL,
                     year = NULL,
                     internal = FALSE,
                     output_directory = NULL){
  
  ## check whether parameters are set
  if(is.null(temp)|is.null(prcp)){
    stop("Please provide both temp and prcp data.")
  }
  
  if(internal == FALSE){
    if(is.null(year)){
      stop("Please specify the year.")
    }
  }
  
  # load data
  mmp <- suppressWarnings(raster::brick(prcp))
  mmt <- suppressWarnings(raster::brick(temp))
  
  # get mean warmest month temperature
  mwmt <- max(mmt)
  
  # get mean summer precipitation
  # note - defining summer as July/August
  st <- suppressWarnings(raster::stack(temp,
                                       bands=c(7,8)))
  msp <- raster::calc(x = st,
                      fun = mean,
                      na.rm = TRUE)
    
  # perform calculation
  result <- mwmt/(msp/1000)
  
  # return all data to raster, either as a geotiff or as a local object
  if (internal == FALSE){
    # create output file name
    output_file <- sprintf('shm_calc_%s.tif',year)
    
    # setting output directory if specified
    if (!is.null(output_directory)){
      output_file <- sprintf("%s/%s", output_directory, output_file)
    }
    
    # write result to file
    raster::writeRaster(result,
                        output_file,
                        overwrite = TRUE)
  } else {
    # return result
    return(result)
  }
}