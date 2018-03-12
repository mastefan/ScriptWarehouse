#' cbind.fill
#' 
#' Will bind vectors of unequal length into a data frame 
#' and insert NA where values are missing in shorter vectors
#' 
#' @param ... objects (vectors) to be bound
#' 
#' used in FieldSheetsProcessing.1.rmd
#' 
#' Source: https://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}