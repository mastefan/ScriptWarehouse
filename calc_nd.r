#' @description Counts the number of days that meet a user-specified 
#' condition and returns a spatially-explicit result. This can be 
#' used to extract indices such as Growing Degree Days (tmin > 0), 
#' or days with precipitation (prcp != 0).
#' 
#' @param file string The name of the file to be processed. Use daily 
#' Daymet data.
#' @param start_day numeric Day of year at which counting should begin. 
#' Default = 0 (January 1).
#' @param end_day numeric Day of year at which counting should end. 
#' Default = 365 (usually December 31).
#' @param criteria A binary operator (e.g. ">") to be evaluated. This 
#' must be in quotes "".
#' @param value numeric The value that the criteria is to be evaluated
#'  against.
#' @param internal logical If FALSE, write the output to a tif 
#' file using the Daymet file format protocol.
#' @param name character An identifier to include in the name of the 
#' output file. Used only if internal = FALSE.
#' @param output_directory vector (optional) A path to a 
#' directory where output files should be written. Used only if 
#' internal = FALSE.
#' 
#' @keywords daymet, climate data
#' 
#' @examples
#' 
#'  \dontrun{
#'  # These examples demonstrate how calc_nd can be used to count 
#'  # Growing Degree Days and Freezing Degree Days.
#'  
#'  #In this example the data has already been downloaded using the 
#'  #download_daymet_ncss function.
#'  
#'  # First, set the working directory as the directory 
#'  # containing the file to be processed.
#'  setwd(tempdir())
#'  
#'  # Next, specify the name of the file.
#'  data_file <- "tmin_daily_1980_ncss.nc"
#'  
#'  # Finally, run the function.
#'  # Growing Degree Days are defined here as days in which the 
#'  # minimum temperature is greater than 2 C (tmin > 2).
#'  calc_nd(file = data_file,
#'          criteria = ">",
#'          value = 2,
#'          name = "GDD")
#'  
#'  # If you wish to write the  result into a separate directory 
#'  #then specify the path to that directory, then pass that 
#'  #directory to the function.
#'  out_dir <- "c:/.../results/"
#'  
#'  calc_nd(file = data_file,
#'          criteria = ">",
#'          value = 2,
#'          name = "GDD",
#'          output_directory = out_dir)
#'  
#'  # Freezing degree days are defined here as days in which the 
#'  # minimum temperature is less than 0 C (tmin < 0).
#'  calc_nd(file = data_file,
#'          criteria = "<",
#'          value = 0,
#'          name = "FDD")
#'  
#'  } 

calc_nd <- function(file = NULL,
                    start_day = 0,
                    end_day = 365,
                    criteria = NULL,
                    value = NULL,
                    internal = FALSE,
                    name = NULL,
                    output_directory = NULL){
  
  # perform input checks
  if(is.null(file) | is.null(criteria) | is.null(value)){
    stop('Please specify file, criteria and value.')
  }
  
  # load desired bands from file
  data <- suppressWarnings(raster::stack(file,
                                         bands = c(start_day:end_day)))

  # use a binary operator to identify pixels that meet the criteria
  sel <- raster::overlay(x = data,
                         fun = function(x) do.call(criteria, list(x, value)))
  
  # use SUM to gather the number of days that meet the criteria
  result <- raster::calc(x = sel,
                         fun = sum,
                         na.rm = TRUE)
  
  # return all data to raster, either as a geotiff or as a local object
  if (internal == FALSE){
    # remove path from file
    file <- basename(file)
    
    # create output file name
    year <- strsplit(file, "_")[[1]][3]
    output_file <- sprintf('%s_nd_%s.tif', name, year)
    
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