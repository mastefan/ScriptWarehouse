# load data
tmean <- list.files(path = "D:/RawData/daymet", pattern = "tmean_monavg", full.names = TRUE)
tmean <- raster::stack(x = tmean[1:2])

prcp <- list.files(path = "D:/RawData/daymet", pattern = "prcp_monttl", full.names = TRUE)
prcp <- raster::stack(x = prcp[1:2])

# extracting latitude as decimal degrees
ref <- raster::projectExtent(tmean, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
int <- raster::projectRaster(from = tmean, to = ref)
latitude <- raster::setValues(tmean, sp::coordinates(int)[,"y"])
rm(ref, int)

# pet with correct latitude
pet_f <- function(tmean, latitude){
  SPEI::thornthwaite(Tave = tmean, lat = latitude, na.rm = TRUE)
}
pet <- raster::overlay(x = tmean, y = latitude, fun = Vectorize(pet_f))

# prcp - pet
int_f <- function(prcp, pet){
  prcp - pet
}
int <- raster::overlay(x = prcp, y = pet, fun = Vectorize(int_f))

# spei
spei_f <- function(int, scale){
  SPEI::spei(data = int, scale = scale, na.rm = TRUE)
}
scale = 1
spei1 <- raster::calc(x = int, fun = spei_f)

#####

# load data
tmean <- list.files(path = "D:/Processing/daymet", pattern = "tmean_monavg", full.names = TRUE)
tmean <- raster::stack(x = tmean)

prcp <- list.files(path = "D:/Processing/daymet", pattern = "prcp_monttl", full.names = TRUE)
prcp <- raster::stack(x = prcp)

## crop data
#box <- raster::extent(tmean)
#box[1] <- 1800
#box[2] <- 1810
#box[3] <- 500
#box[4] <- 510
#tmean <- raster::crop(tmean, box)
#prcp <- raster::crop(prcp, box)

# latitude function test
source("gridPETlat.r")
lat <- gridPETlat(tmean = tmean)
raster::plot(lat)
raster::writeRaster(x = lat, filename = "D:/Processing/daymet/pet_latitude.tif")

# pet function test
source("gridPET.r")
pet <- gridPET(tmean = tmean, latitude = lat)
raster::plot(pet)

## spei function test
source("gridSPEI.r")
spei <- gridSPEI(tmean, prcp, pet, scale = 12)
#raster::beginCluster()
#spei <- raster::clusterR(x = tmean, fun = gridSPEI, 
#                         args = list(prcp = prcp, 
#                                     pet = pet, 
#                                     scale = 12, 
#                                     start.year = 1988, 
#                                     end.year = 2014, 
#                                     na.rm = TRUE),
#                         export = c("prcp", "pet"),
#                         filename = "D:/Processing/daymet/spei12_allyears.tif")
#raster::endCluster()
#raster::plot(spei[[25:36]])


######## version 3 of functions - correct implimentation of raster functions?

# load data
tmean <- list.files(path = "D:/Processing/daymet", pattern = "tmean_monavg", full.names = TRUE)
tmean <- raster::brick(raster::stack(x = tmean))

prcp <- list.files(path = "D:/Processing/daymet", pattern = "prcp_monttl", full.names = TRUE)
prcp <- raster::brick(raster::stack(x = prcp))

# crop data
box <- raster::extent(tmean)
box[1] <- 2000000
box[2] <- 2002000
box[3] <-  200000
box[4] <-  202000
tme <- raster::crop(tmean, box)
prc <- raster::crop(prcp,  box)

# gridPETlat3 test
source("gridPETlat3.r")
lat <- gridPETlat3(tmean = tmean)

# gridPET3 test
source("gridPET3.r")
pet <- gridPET3(tmean = tmean, latitude = lat)
raster::beginCluster()
pet <- raster::clusterR(x = tmean, fun = gridSPEI3, args = list(latitude = lat), export = c("lat"))
raster::endCluster()

source("gridSPEI3.r")
test <- gridSPEI3(tmean = tmean, prcp = prcp, pet = pet, scale = 2)


# this is the glorious code that made spei_12.tif a reality. Thank you, glorious code.
raster::beginCluster(6)
spei <- gridSPEI3(tmean, prcp, pet)
raster::endCluster()
raster::writeRaster(x = spei, filename = "D:/Processing/daymet/spei_12.tif")
gc()
