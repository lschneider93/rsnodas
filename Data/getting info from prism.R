raster::plot("C:/Users/Logan/Desktop/PRISM/april/monthly/elevation/PRISM_us_dem_800m_asc.asc")
setwd("elevation")
getwd()
#  "C:/Users/Logan/Desktop/PRISM/april/monthly/elevation"

################################################################################
######## Calculating the difference and storing that information     ###########
################################################################################

mon <- c('nov', 'dec', 'jan', 'feb', 'march', 'april', 'annual')
v <- c(11, 12, "01", "02", "03", "04", "annual" )
info <- c('max temp',
  "maximum vapor pressure deficit",
  "mean dew point temperature",
  "mean temp",
  "min temp",
  "minimum vapor pressure deficit",
  "precipitation")
file_type <- c("tmax_30yr_normal_800mM2_",
               "vpdmax_30yr_normal_800mM2_",
               "tdmean_30yr_normal_800mM2_",
               "tmean_30yr_normal_800mM2_",
               "tmin_30yr_normal_800mM2_",
               "vpdmin_30yr_normal_800mM2_",
               "ppt_30yr_normal_800mM2_")



# prism_values <- vector("list", length(mon) )
maxtemp <- vector("list", length(mon))
maxvapor <- vector("list", length(mon))
meandew <- vector("list", length(mon))
meantemp <- vector("list", length(mon))
mintemp <- vector("list", length(mon))
minvapor <- vector("list", length(mon))
precip <- vector("list", length(mon))

# for (i in 1:length(years)){
#   test_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]])
#   bilinear_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]],
#                                           method = "bilinear")
# }
?raster::extract()
raster::plot(raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[1], "/monthly/", info[1], "/PRISM_", "tmax_30yr_normal_800mM2_", v[1] , "_asc.asc")))

for (i in 1:length(mon)){

  r <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[1], "/PRISM_",
                             file_type[1], v[i] , "_asc.asc"))
  r2 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[2], "/PRISM_",
                             file_type[2], v[i] , "_asc.asc"))
  r3 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[3], "/PRISM_",
                             file_type[3], v[i] , "_asc.asc"))
  r4 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[4], "/PRISM_",
                             file_type[4], v[i] , "_asc.asc"))
  r5 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[5], "/PRISM_",
                             file_type[5], v[i] , "_asc.asc"))
  r6 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[6], "/PRISM_",
                             file_type[6], v[i] , "_asc.asc"))
  r7 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", mon[i],
                             "/monthly/", info[7], "/PRISM_",
                             file_type[7], v[i] , "_asc.asc"))
  new_r <- raster::projectRaster(r, crs = tcrs)
  new_r2 <- raster::projectRaster(r2, crs = tcrs)
  new_r3 <- raster::projectRaster(r3, crs = tcrs)
  new_r4 <- raster::projectRaster(r4, crs = tcrs)
  new_r5 <- raster::projectRaster(r5, crs = tcrs)
  new_r6 <- raster::projectRaster(r6, crs = tcrs)
  new_r7 <- raster::projectRaster(r7, crs = tcrs)

  maxtemp[[i]] <- raster::extract(new_r, error_stations_spatial )
  maxvapor[[i]] <- raster::extract(new_r2, error_stations_spatial )
  meandew[[i]] <- raster::extract(new_r3, error_stations_spatial )
  meantemp[[i]] <- raster::extract(new_r4 , error_stations_spatial )
  mintemp[[i]] <- raster::extract(new_r5, error_stations_spatial )
  minvapor[[i]] <- raster::extract(new_r6, error_stations_spatial )
  precip[[i]] <- raster::extract(new_r7 , error_stations_spatial )
  # uniq_stations
  # test_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]])
  # bilinear_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]],
  #                                         method = "bilinear")
}

r
r2
r3
r4
r5
r6
r7
new_r2
meandew[[1]]
raster::plot(r)

error_stations$NOV_maxtemp <- maxtemp[[1]]
error_stations$NOV_maxvapor <- maxvapor[[1]]
error_stations$NOV_meandew <- meandew[[1]]
error_stations$NOV_meantemp <- meantemp[[1]]
error_stations$NOV_mintemp <- mintemp[[1]]
error_stations$NOV_minvapor <- minvapor[[1]]
error_stations$NOV_precip <- precip[[1]]

error_stations$DEC_maxtemp <- maxtemp[[2]]
error_stations$DEC_maxvapor <- maxvapor[[2]]
error_stations$DEC_meandew <- meandew[[2]]
error_stations$DEC_meantemp <- meantemp[[2]]
error_stations$DEC_mintemp <- mintemp[[2]]
error_stations$DEC_minvapor <- minvapor[[2]]
error_stations$DEC_precip <- precip[[2]]

error_stations$JAN_maxtemp <- maxtemp[[3]]
error_stations$JAN_maxvapor <- maxvapor[[3]]
error_stations$JAN_meandew <- meandew[[3]]
error_stations$JAN_meantemp <- meantemp[[3]]
error_stations$JAN_mintemp <- mintemp[[3]]
error_stations$JAN_minvapor <- minvapor[[3]]
error_stations$JAN_precip <-  precip[[3]]

error_stations$FEB_maxtemp <- maxtemp[[4]]
error_stations$FEB_maxvapor <- maxvapor[[4]]
error_stations$FEB_meandew <- meandew[[4]]
error_stations$FEB_meantemp <- meantemp[[4]]
error_stations$FEB_mintemp <- mintemp[[4]]
error_stations$FEB_minvapor <- minvapor[[4]]
error_stations$FEB_precip <- precip[[4]]

error_stations$MARCH_maxtemp <- maxtemp[[5]]
error_stations$MARCH_maxvapor <- maxvapor[[5]]
error_stations$MARCH_meandew <- meandew[[5]]
error_stations$MARCH_meantemp <- meantemp[[5]]
error_stations$MARCH_mintemp <- mintemp[[5]]
error_stations$MARCH_minvapor <- minvapor[[5]]
error_stations$MARCH_precip <- precip[[5]]

error_stations$APRIL_maxtemp <- maxtemp[[6]]
error_stations$APRIL_maxvapor <- maxvapor[[6]]
error_stations$APRIL_meandew <- meandew[[6]]
error_stations$APRIL_meantemp <- meantemp[[6]]
error_stations$APRIL_mintemp <- mintemp[[6]]
error_stations$APRIL_minvapor <- minvapor[[6]]
error_stations$APRIL_precip <- precip[[6]]

error_stations$ANNUAL_maxtemp <- maxtemp[[7]]
error_stations$ANNUAL_maxvapor <- maxvapor[[7]]
error_stations$ANNUAL_meandew <- meandew[[7]]
error_stations$ANNUAL_meantemp <- meantemp[[7]]
error_stations$ANNUAL_mintemp <- mintemp[[7]]
error_stations$ANNUAL_minvapor <- minvapor[[7]]
error_stations$ANNUAL_precip <- precip[[7]]
error_stations$uniq_stations == uniq_stations

 for (j in 1:length(years)){
  april_df_ghcnd_spatial[[j]]$MODEL_WESD <- test_values[[j]]
  april_df_ghcnd[[j]]$MODEL_WESD <- test_values[[j]]
}


new_r <- raster::projectRaster(maxtemp_r_nov, crs = tcrs)

raster::plot(new_r)


################################################################################
######## Calculating the difference and storing that information     ###########
################################################################################

mon

elevation_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_04_asc.asc")
maxvapor_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_04_asc.asc")
meandew_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_04_asc.asc")
meantemp_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_04_asc.asc")
mintemp_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_04_asc.asc")
minvapor_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/april/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_04_asc.asc")

# elevation_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_03_asc.asc")
maxvapor_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_03_asc.asc")
meandew_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_03_asc.asc")
meantemp_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_03_asc.asc")
mintemp_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_03_asc.asc")
minvapor_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/march/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_03_asc.asc")

# elevation_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_02_asc.asc")
maxvapor_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_02_asc.asc")
meandew_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_02_asc.asc")
meantemp_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_02_asc.asc")
mintemp_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_02_asc.asc")
minvapor_r_feb <- raster::raster("C:/Users/Logan/Desktop/PRISM/feb/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_02_asc.asc")

#elevation_r_march <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_01_asc.asc")
maxvapor_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_01_asc.asc")
meandew_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_01_asc.asc")
meantemp_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_01_asc.asc")
mintemp_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_01_asc.asc")
minvapor_r_jan <- raster::raster("C:/Users/Logan/Desktop/PRISM/jan/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_01_asc.asc")

#elevation_r_dec <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_dec <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_04_asc.asc")
maxvapor_r_dec <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_04_asc.asc")
meandew_r_dec <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_04_asc.asc")
meantemp_r_dec <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_04_asc.asc")
mintemp_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_04_asc.asc")
minvapor_r_april <- raster::raster("C:/Users/Logan/Desktop/PRISM/dec/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_04_asc.asc")

#elevation_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/elevation/PRISM_us_dem_800m_asc.asc")
maxtemp_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/max temp/PRISM_tmax_30yr_normal_800mM2_11_asc.asc")
maxvapor_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/maximum vapor pressure deficit/PRISM_vpdmax_30yr_normal_800mM2_11_asc.asc")
meandew_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/mean dew point temperature/PRISM_tdmean_30yr_normal_800mM2_11_asc.asc")
meantemp_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/mean temp/PRISM_tmean_30yr_normal_800mM2_11_asc.asc")
mintemp_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/min temp/PRISM_tmin_30yr_normal_800mM2_11_asc.asc")
minvapor_r_nov <- raster::raster("C:/Users/Logan/Desktop/PRISM/nov/monthly/minimum vapor pressure deficit/PRISM_vpdmin_30yr_normal_800mM2_11_asc.asc")

raster::plot(maxtemp_r_nov)



# how to reproject a raster
?projectRaster()
new_r <- raster::projectRaster(maxtemp_r_nov, crs = tcrs)

raster::plot(new_r)



################################################################################
###### Looking at Certain Longitudes   #########
################################################################################

error_stations_spatial <- error_stations

sp::coordinates(error_stations_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

dim(error_stations)
