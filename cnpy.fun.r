#' cnpy.fun
#' 
#' calculates the canopy cover (%) of a given plot
#' 
#' @param cnpy.data a data frame containing matched columns of plot names
#' and canopy cover class
#' @param plot the plot for which canopy cover should be calculated "in quotes"
#' 
#' used in FieldSheetsProcessing.1.rmd
#' 
#' dependencies: dplyr

cnpy.fun <- function(cnpy.data,plot){
  a=dplyr::filter(cnpy.data, Plot==plot)
  CanopyCover=c(a$Cnpy)
  c=table(CanopyCover)
  c/13*100
}
