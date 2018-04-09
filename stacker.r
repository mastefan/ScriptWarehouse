#' Stacker
#' 
#' Takes all .tif files in a directory and writes them as bands 
#' to a single .tif raster file.
#' 
#' @param in_path Path to directory containing files to be stacked
#' @param out_name Filename (and path) of the output. Optional.

stacker <- function(in_path = NULL, out_name = NULL){
  # input check for in_path
  if(is.null(in_path | out_name)){
    stop('Please specify in_path.')
  }
  
  # list .tif files at in_path
  tifs <- list.files(path = in_path,
                     pattern = ".tif$",
                     full.names = TRUE,
                     include.dirs = TRUE)
  
  # create raster stack using tifs list
  result <- raster::stack(tifs)
  
  # write the file
  raster::writeRaster(x = result,
                      filename = out_name)
}