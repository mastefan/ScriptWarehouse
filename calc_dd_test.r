# calc dd test

# test data
tmean <- array(rnorm(n = (3*4*365), mean = 15, sd = 5), c(3,4,365))
tmean <- raster::brick(tmean)

# get function
source("calc_dd.r")

# run function w/o accumulation
test_1 <- calc_dd(tmean, threshold = 15, index = "grow")
test_2 <- calc_dd(tmean, threshold = 15, index = "chill")
raster::plot(test_1, main = "HDD daily")
raster::plot(test_2, main = "CDD daily")

# run function w accumulation
test_1 <- calc_dd(tmean, threshold = 15, index = "grow", accumulate = TRUE, start = 1, end = 16)
test_2 <- calc_dd(tmean, threshold = 15, index = "chill", accumulate = TRUE, start = 1, end = 16)
raster::plot(test_1, main = "accu HDD daily")
raster::plot(test_1[[raster::nlayers(test_1)]], main = "accu HDD sum")
raster::plot(test_2, main = "accu CDD daily")
raster::plot(test_2[[raster::nlayers(test_2)]], main = "accu CDD sum")
