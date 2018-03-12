#' Shannon
#' 
#' Calculates Shannon's Diversity by plot using the outputs from 
#' richness.func and spec.func
#' 
#' Note that these outputs must first be combined into a dataframe 
#' by "count" (see FieldSheetsProcessing.1.rmd)
#' 
#' Source: Krebs 1999, Ecological Methodology
#' 
#' @param data dataframe of outputs from richness.func and spec.func

Shannon <- function(data){
  su=apply(data,2,function(x) sum(x[2:27]))
  p=apply(data, 2, function(x) (x[2:27]/su)*(log(x[2:27]/su)))
  Diversity=as.data.frame(apply(p,2,function(x) -1*sum(x, na.rm=TRUE)))
  Diversity=t(Diversity)
  row.names(Diversity) <- c("Shannon Diversity")
  Diversity
}