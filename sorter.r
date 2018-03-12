#' sorter
#' 
#' This function sorts input data by name (plot), and produces a 
#' vector of outputs. This is useful for reformatting point-quarter 
#' data in 4 columns into a single vector for later manipulation.
#' 
#' @param data a data frame containing 5 columns 
#' (1 is name & 2:5 are values)
#' @param plot name "in quotes" by which data should be sorted 
#' (must be in column 1)
#' 
#' Used in FieldSheetsProcessing.1.rmd
#' 
#' Dependencies: dplyr

sorter = function(data, plot){
  df=dplyr::filter(data, Plot==plot)
  list=unlist(df[2:5],use.names = FALSE)
  list
}