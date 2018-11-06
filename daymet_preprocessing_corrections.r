# file paths
p_path <- "F:/Processing/daymet"
s_path <- "F:/"

# functions
#source(sprintf("%s/daymet_grid_agg.r", s_path))
source(sprintf("%s/calc_dd.r", s_path))
#source(sprintf("%s/gridSPEI.r", s_path))
#source(sprintf("%s/gridPET.r", s_path))
#source(sprintf("%s/tmean.tif.r", s_path))

# set index
st <- 1988
en <- 2014
yr <- seq(from = st, to = en, by = 1)

### DONE
## monthly swe
#for (i in yr){
#  daymet_grid_agg(file = sprintf("%s/swe_daily_%s_ncss.tif", p_path, i),
#                  int = "monthly",
#                  FUN = "mean",
#                  internal = FALSE,
#                  output_directory = p_path)
#}

### DONE
## montly mean dayl
#for(i in yr){
#  daymet_grid_agg(file = sprintf("%s/dayl_daily_%s_ncss.tif", p_path, i),
#                  int = "monthly",
#                  FUN = "mean",
#                  internal = FALSE,
#                  output_directory = p_path)
#}

### DONE
## daily tmean
#for(i in yr){
#  #daymetr::tmean only works with .nc inputs, so I created a version 'tmean.tif.r' which works with .tif's
#  tmean(path = p_path, product = "daily", year = i, internal = FALSE)
#}

### degree day thresholds based on method in Richardson 2006, Phenology of...

## ahdd for use with sos using parallel processing
# get list of tmean files
tm <- list.files(p_path, "tmean_daily", full.names = TRUE)
for(i in 1:length(yr)){
  
  # set dates
  st = lubridate::yday(as.Date(sprintf("%s-01-01", yr[i]))) # Jan 1
  en = lubridate::yday(as.Date(sprintf("%s-04-30", yr[i]))) # Apr 30
  
  # begin parallel processing
  raster::beginCluster()
  # run calculation
  out <- raster::clusterR(x = raster::stack(tm[i]), 
                          fun = calc_dd,
                          args = list(index = "grow", accumulate = "sum", start = st, end = en, threshold = 4),
                          export = c('st', 'en', 'yr'),
                          filename = sprintf("%s/ahdd_sos_%s.tif", p_path, yr[i]), overwrite = TRUE,
                          progress = "text")
  # end parallel processing
  raster::endCluster()
  # cleanup
  rm(out)
}

# ahdd for use with max using parallel processing
tm <- list.files(p_path, "tmean_daily", full.names = TRUE)
for(i in 1:length(yr)){
  st = lubridate::yday(as.Date(sprintf("%s-01-01", yr[i]))) # Jan 1
  en = lubridate::yday(as.Date(sprintf("%s-07-15", yr[i]))) # Jul 15
  raster::beginCluster()
  out <- raster::clusterR(x = raster::stack(tm[i]), 
                          fun = calc_dd,
                          args = list(index = "grow", accumulate = "sum", start = st, end = en, threshold = 4),
                          export = c('st', 'en', 'yr'),
                          filename = sprintf("%s/ahdd_max_%s.tif", p_path, yr[i]), overwrite = TRUE,
                          progress = "text")
  raster::endCluster()
  rm(out)
}

# acdd for use with eos using parallel processing
tm <- list.files(p_path, "tmean_daily", full.names = TRUE)
for(i in 1:length(yr)){
  st = lubridate::yday(as.Date(sprintf("%s-07-16", yr[i]))) # Jul 16
  en = lubridate::yday(as.Date(sprintf("%s-12-15", yr[i]))) # Dec 15
  raster::beginCluster()
  out <- raster::clusterR(x = raster::stack(tm[i]), 
                          fun = calc_dd,
                          args = list(index = "chill", accumulate = "sum", start = st, end = en, threshold = 20),
                          export = c('st', 'en', 'yr'),
                          filename = sprintf("%s/acdd_eos_%s.tif", p_path, yr[i]), overwrite = TRUE,
                          progress = "text")
  raster::endCluster()
  rm(out)
}
