# create sample rasters
dep.1 <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 10, sd = 2),
                                   nrow = 10,
                                   ncol = 10))
dep.2 <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 10, sd = 2),
                                   nrow = 10,
                                   ncol = 10))

ind.maker <- function(){
  a <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 5, sd = 2),
                                 nrow = 10,
                                 ncol = 10))
  b <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 3, sd = 2),
                                 nrow = 10,
                                 ncol = 10))
  c <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 6, sd = 2),
                                 nrow = 10,
                                 ncol = 10))
  d <- raster::raster(x = matrix(data = rnorm(n = 100, mean = 4, sd = 2),
                                 nrow = 10,
                                 ncol = 10))
  out <- raster::stack(a,b,c,d)
  names(out) <- c("a","b","c","d")
  return(out)
}

ind.1 <- ind.maker()
ind.2 <- ind.maker()

# create dataframe based on rasters (long format, years stacked)
z <- rbind(raster::as.data.frame(dep.1),
           raster::as.data.frame(dep.2))
a <- rbind(raster::as.data.frame(raster::subset(ind.1, "a")),
           raster::as.data.frame(raster::subset(ind.2, "a")))
b <- rbind(raster::as.data.frame(raster::subset(ind.1, "b")),
           raster::as.data.frame(raster::subset(ind.2, "b")))
c <- rbind(raster::as.data.frame(raster::subset(ind.1, "c")),
           raster::as.data.frame(raster::subset(ind.2, "c")))
d <- rbind(raster::as.data.frame(raster::subset(ind.1, "d")),
           raster::as.data.frame(raster::subset(ind.2, "d")))
data <- data.frame(c(rep(1989, times = 100), rep(1990, times = 100)),
                  z,a,b,c,d)
colnames(data) <- c("year","z","a","b","c","d")
rm(z,a,b,c,d)
View(data)

rm(ind.maker, dep.1, dep.2, ind.1, ind.2)

# test with random forest
randomForest::randomForest(z ~ ., data = data[2:6])