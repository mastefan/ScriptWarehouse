#' SPEI calculator for station data
#' 
#' Takes weather station data inputs and provides SPEI in the same format
#' 
#' @param temp Vector containing the file path to monthly mean temperature data
#' @param prcp Vector containing the file path to monthly total precipitation data
#' @param latitude As in SPEI::spei
#' @param scale As in SPEI::spei
#' @param write Should the output be written to file? To write an output file provide
#'  a vector containing the file path, including name, of the file to be written.

SPEIstation <- function(temp = NULL, prcp = NULL, latitude = 45, scale = 1, write = FALSE){
  
  # load data
  tme <- read.csv(file = temp, header = TRUE)
  prt <- read.csv(file = prcp, header = TRUE)
  
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
  spei.o <- dplyr::filter(spei.o, !is.na(spei.o$Year))
  spei.w <- tidyr::spread(data = spei.o, key = Month, value = SPEI)
  colnames(spei.w) <- nm
  spei.w <- tibble::column_to_rownames(spei.w, var = "X")
  
  # write output
  if(write != FALSE){
    write.csv(x = spei.w, file = write)
  }
  
  return(spei.w)
}
