#' spec.func
#' 
#' Produces a data frame showing a count of observations by species 
#' for a given plot
#' 
#' @param data a data frame including matched columns of plot names and
#' observations by species
#' @param plot the plot for which observations should be counted "in quotes"
#' 
#' Used in FieldSheetsProcessing.1.rmd
#' 
#' dependencies: dplyr

spec.func = function(data, plot){
  library(dplyr)  #load library
  count=dplyr::filter(data,Plot==plot)  #filter plot data
  count=as.matrix(count[2:5])  #turn df into matrix
  count=table(count)  #use table to count entries
  count=data.frame(count) #turn table into df
}