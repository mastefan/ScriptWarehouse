#' pointSPEI
#' 
#' Calculate SPEI using point climate data. Will feed to the workspace and can write an output file.
#' 
#' @param path Directory path
#' @param region Name of region (in quotes "")
#' @param temp Name of mean temperature data file (in quotes "")
#' @param prcp Name of precipitation data file (in quotes "")
#' @param latitude Latitude of station data. Use mean when using several stations. (numeric)
#' @param scale Number of months to use as scale value (numeric)
#' @param write Should a file be written? (.csv file in directory specified by path)
#' 

pointSPEI <- function(path, region, temp, prcp, latitude, scale, write){
  # load data
  tme <- read.csv(file = sprintf("%s/%s", path, temp), header = TRUE)
  prt <- read.csv(file = sprintf("%s/%s", path, prcp), header = TRUE)
  
  # match lengths if there is a mismatch
  if(nrow(tme) != nrow(prt)){
    if(nrow(tme) > nrow(prt)){
      diff = matrix(data = ((nrow(tme) - nrow(prt)) * ncol(tme)),
                    ncol = ncol(tme))
      colnames(diff) <- colnames(tme)
      prt = rbind(diff, prt)
      prt$Year <- tme$Year
    }
    if(nrow(tme) < nrow(prt)){
      diff = matrix(data = rep(NA, times = ((nrow(prt) - nrow(tme)) * ncol(tme))),
                    ncol = ncol(tme))
      colnames(diff) <- colnames(tme)
      tme = rbind(diff, tme)
      tme$Year <- prt$Year
    }
  }
  
  # retain column names for output
  nm <- colnames(tme[1:13])
  
  # convert to long format
  tme <- tme[,1:13]
  colnames(tme) <- c("Year", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  tme <- reshape2::melt(data = tme, id = 'Year')
  tme <- dplyr::arrange(tme, Year)
  prt <- prt[,1:13]
  colnames(prt) <- c("Year", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  prt <- reshape2::melt(data = prt, id = 'Year')
  prt <- dplyr::arrange(prt, Year)
  
  # pet
  pet <- SPEI::thornthwaite(Tave = tme$value, lat = latitude, na.rm = TRUE)
  
  # spei
  spei <- SPEI::spei(data = (prt$value - pet), scale = scale, na.rm = TRUE)
  
  # output formatting
  spei.o <- data.frame(Year = tme$Year, Month = tme$variable, SPEI = round(spei$fitted, 2))
  colnames(spei.o) <- c('Year','Month','SPEI')
  spei.w <- tidyr::spread(data = spei.o, key = Month, value = SPEI)
  colnames(spei.w) <- nm
  
  # write output
  if(write == TRUE){
    write.csv(x = spei.w, file = sprintf("%s/%s_spei_%s.csv", path, region, scale))
  }
  
  return(spei.w)
}