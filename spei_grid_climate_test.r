# load data
tmean <- list.files(path = "E:/OneDrive - Queen's University/RawData/daymet", pattern = "tmean_monavg", full.names = TRUE)
tmean <- raster::stack(x = tmean[1:2])

prcp <- list.files(path = "E:/OneDrive - Queen's University/RawData/daymet", pattern = "prcp_monttl", full.names = TRUE)
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
