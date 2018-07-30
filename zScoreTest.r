## testing Stackoverflow feedback
# example data
data("Rlogo")
data <- raster::brick(Rlogo)
raster::plot(data)

# functions
fun1 <- function(x,y){x-y}
fun2 <- function(x,y){x/y}

# calculate sd
sd <- raster::calc(data, stats::sd)

# calculate mean (mean of layer x is mean of all layers but x)
mn <- data
set <- 1:nlayers(data)
for (i in set) {
  subset <- set[-i]
  wrk <- raster::stack(x = data, bands = subset)
  mn_i <- raster::calc(x = wrk, fun = mean)
  mn[[i]] <- mn_i
  rm(mn_i, wrk)
}
rm(i, subset)
names(mn) <- 1:5

# raw z score
z <- data
set <- 1:nlayers(data)
raster::beginCluster(2)
for(i in set){
  wrk <- raster::stack(data[[i]], mn[[i]])
  z_i <- clusterR(x = wrk, fun = raster::overlay, arg = list(fun = fun1))
  z[[i]] <- z_i
  rm(z_i, wrk)
}
raster::endCluster()
rm(i)

# normalized z score
raster::beginCluster(2)
for(i in set){
  wrk <- raster::stack(z[[i]], sd)
  z_i <- clusterR(x = wrk, fun = raster::overlay, arg = list(fun = fun2))
  z[[i]] <- z_i
  rm(z_i, wrk)
}
raster::endCluster()
rm(i, set)


## timing parallel vs standard processing
# standard run
system.time(zScore(file = "tin_a2a.tif", parallel = FALSE, na.rm = TRUE))
### result: 254s total

# parallel run
system.time(zScore(file = "tin_a2a.tif", parallel = TRUE, ncores = 7, na.rm = TRUE))
### result: 360s total

#### the standard run is faster