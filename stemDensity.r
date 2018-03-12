#' stemDensity
#'
#' Calculates stem density using point-quarter distance measurements. 
#' Calculations can be made by any given subset. Subsets can be made 
#' using the sorter function.
#' 
#'  Source: Krebs 1999, Ecological Methodology &
#'  Pollard 1971, Biometrics 

stemDensity <- function(data){
  # calculations
  r2 <- sum(((data/100)^2),na.rm=TRUE)
  n <- length(data)/4
  den <- (4*(4*n-1))/(pi*r2)
  var <- (den^2)/(4*n-2)
  s.e <- sqrt(var/(4*n))
  #l.c <- ((sqrt(16*n-1))-1.96)/(sqrt(pi*r2))
  #u.c <- ((sqrt(16*n-1))+1.96)/(sqrt(pi*r2))
  d.u <- den+s.e
  d.l <- den-s.e
  
  # output creation
  out <- c(den,d.u,d.l,s.e)
  names(out) <- c("Stem Density (stems/ha)",
                  "Density + Standard Error",
                  "Density - Standard Error",
                  "Standard Error")
  
  # correction to stems/ha
  out <- out*10000
  
  # return output
  return <- out
}