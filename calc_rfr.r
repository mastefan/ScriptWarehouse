#' Root Freezing Risk Index Calculation
#' 
#' No source for this, making it up as I go!
#' (maybe look for a reference, surely someone has done something similar)
#' 
#' by day(if swe = 0 AND temp <0){RFR = abs(temp)}
#' 
#' #better to simply count the number of days in a year?
#' 
#' For aggregation pass output to daymet_grid_agg.r
#' 
#' @param temp vector containing the file name/path of minimum daily air temperature
#'  data, which can be downloaded using the download_daymet functions
#' @param swe a vector containing the file name/path of daily snow-water equivalent
#' data, which can be downloaded using the download_daymet functions
#' @param year the year that the data represents. This is used for naming the output.
#' @param internal logical indicating whether to write the output to a file (FALSE)
#' or to output it to the current workspace (TRUE)
#' @param output_directory vector (optional) A path to a 
#' directory where output files should be written. Used only if 
#' internal = FALSE.

calc_rfr <- function(temp=NULL,
                     swe=NULL,
                     year=NULL,
                     internal=FALSE,
                     output_directory = NULL){
  
  # check whether temp and swe are provided
  if(is.null(temp)|is.null(swe)){
    stop("Please provide both temp and prcp data.")
  }
  
  # load data
  temp <- suppressWarnings(raster::brick(temp))
  swe  <- suppressWarnings(raster::brick(swe))
  
  # prepare data for calculation by reclassifying values
  temp [temp    > 0]   <-  0 # remove non-freezing temperatures
  temp           <- temp - 1 # shift temperatures -1 to enable positive temperatures to have value of 0
  temp [is.na(temp)]   <-  0 # set NA values to 0 to prevent NAs from removing data in result
  swe  [swe    == 0]   <- -1 # set zero swe to -1 for multiplication
  swe  [swe     > 0]   <-  0 # remove non-zero swe
  
  # calculation of root freeze risk and reclassification of results
  result <- (temp * swe)  -1 # gives positive air temperature when swe = 0 and corrects for positive temperatures
  result [result == -1] <- 0 # sets instances of positive temperatures as 0 to mask from results
  
  # return all data to raster, either as a geotiff or as a local object
  if (internal == FALSE){
    # create output file name
    output_file <- sprintf('rfr_calc_%s.tif',year)
    
    # setting output directory if specified
    if (!is.null(output_directory)){
      output_file <- sprintf("%s/%s", output_directory, output_file)
    }
    
    # write result to file
    raster::writeRaster(result,
                        output_file,
                        overwrite = TRUE)
  } else {
    # return result to workspace
    return(result)
  }
}