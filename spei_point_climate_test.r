## point climate spei test
#load data (frontenac)
tme <- read.csv(file = "E:/OneDrive - Queen's University/Processing/point_climate/frontenac_tme.csv", header = TRUE)
prt <- read.csv(file = "E:/OneDrive - Queen's University/Processing/point_climate/frontenac_prt.csv", header = TRUE)

# convert to long format
tme <- tme[,1:13]
colnames(tme) <- c("Year", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
tme <- reshape2::melt(data = tme, id = 'Year')
tme <- dplyr::arrange(tme, Year)

prt <- prt[,1:13]
colnames(prt) <- c("Year", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
prt <- reshape2::melt(data = prt, id = 'Year')
prt <- dplyr::arrange(prt, Year)

# unify climate data
clim <- data.frame(Year = tme$Year, Month = tme$variable, tmean = tme$value, precip = prt$value)

# latitudes
lat_b_p <- 44.60
lat_b_t <- 44.60
lat_k_p <- 44.24
lat_k_t <- 44.22
lat_g_p <- 44.57
lat_t <- mean(c(lat_b_t, lat_k_t))
lat_p <- mean(c(lat_b_p, lat_k_p, lat_g_p))
lat_a <- mean(c(lat_t, lat_p))
rm(lat_b_p, lat_b_t, lat_k_p, lat_k_t, lat_g_p, lat_p, lat_t)

# pet
pet <- SPEI::thornthwaite(Tave = clim$tmean, lat = lat_a, na.rm = TRUE)

# spei
spei <- SPEI::spei(data = (clim$precip - pet), scale = 1, na.rm = TRUE)

# output formatting
spei.o <- data.frame(Year = clim$Year, Month = clim$Month, SPEI = spei$fitted)
colnames(spei.o) <- c('Year','Month','SPEI')

# convert to wide format
spei.w <- tidyr::spread(data = spei.o, key = Month, value = 'SPEI')

# create function to run calculation going from raw data file to SPEI output in wide format
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

# testing the functon
path = "E:/OneDrive - Queen's University/Processing/point_climate"
region = "frontenac"
temp = "frontenac_tme.csv"
prcp = "frontenac_prt.csv"
latitude = lat_a
scale = 1
write = TRUE

frontenac_spei <- pointSPEI(path = path,
                            region = region,
                            temp = temp,
                            prcp = prcp,
                            latitude = latitude,
                            scale = scale,
                            write = write)

lat_m <- 45.50
lat_h <- 45.03
lat_b <- 45.13
lat_s_p <- 45.97
lat_s_t <- 45.95
lat_n <- 46.36
lat <- mean(lat_m, lat_h, lat_b, lat_s_p, lat_s_t, lat_n)
rm(lat_m, lat_h, lat_b, lat_s_p, lat_s_t, lat_n)

algonquin_spei <- pointSPEI(path = path,
                            region = "algonquin",
                            temp = "algonquin_tme.csv",
                            prcp = "algonquin_prt.csv",
                            latitude = lat,
                            scale = scale,
                            write = TRUE)
