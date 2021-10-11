################################################################################
###### Looking at Certain Latitudes    #########
################################################################################
min(df_of_all_years$LATITUDE) # 37.248
max(df_of_all_years$LATITUDE) # 41.97
41.97 - 37.248 # 4.722

################################################################################
###### Looking at Certain Longitudes   #########
################################################################################
min(df_of_all_years$LONGITUDE) # -109.27
max(df_of_all_years$LONGITUDE) # -113.98
113.98 - 109.27  # 4.71

################################################################################
###### It could be an Interaction of both Longitudes and Latitudes   #########
################################################################################


################################################################################
###### Looking at Elevation    #########
################################################################################
min(df_of_all_years$ELEVATION) # 1216.5
max(df_of_all_years$ELEVATION) # 3342.4
3342.4 - 1216.5  # 2125.9

################################################################################
###### Looking at a Certain Station Every Year    #########
uniq_stations <- unique(df_of_all_years$ID)# 155 stations
################################################################################

unique(df_of_all_years$ID[df_of_all_years$DATE == "2004-04-01"]) # 85
l <- numeric()
for (i in 1:length(years)) { 
  l[i] <- dim(april_df_ghcnd[[i]])[1]
  }
l

################################################################################
#### Making a List of all the Individual Stations with Data at some point ######
################################################################################
individ_station <- vector("list", length(unique(df_of_all_years$ID)))

# this is creating a list of each station that has an observation
for (j in 1:length(unique(df_of_all_years$ID))){
  station_of_interest <- unique(df_of_all_years$ID)[j]
  individ_station[[j]] <- df_of_all_years[df_of_all_years$ID == station_of_interest, ] 
}

# This is calculating the average bias of the individual stations
for (i in 1:length(individ_station)){
  individ_station[[i]]$AVE_BIAS <- mean(individ_station[[i]]$MODEL_DIFFERENCE) 
}

# This is creating vectors neg, pos, zero, and obs for the amount of times that 
# the difference is positive, negative, and the total number of observations
for (i in 1:length(individ_station)){
  neg <- 0
  pos <- 0
  zero <- 0
  for (j in 1:length(individ_station[[i]]$DIFFERENCE)){
    if (individ_station[[i]]$DIFFERENCE[j] > 0){
      pos <- pos + 1
    }
    else if(individ_station[[i]]$DIFFERENCE[j] < 0){
      neg <- neg + 1
    }
    else {
      zero <- zero + 1
    }
    
  }
  individ_station[[i]]$pos <- pos
  individ_station[[i]]$neg <- neg
  individ_station[[i]]$zero <- zero
  individ_station[[i]]$obs <- pos + neg + zero
}

################################################################################
###### Calculating the Absolute Sum of the Difference, MSE, Total Error    #####
################################################################################
#MSE_LIST <- vector("list", length(unique(df_of_all_years$ID)))
MSE_VECTOR <- numeric()
total_error <- numeric()
summation <- numeric()
avediff <- numeric()
lat <- numeric()
lon <- numeric()
ele <- numeric()
reports <- numeric()

for (i in 1:length(uniq_stations)){
  lat[i] <- unique(df_of_all_years[df_of_all_years$ID == uniq_stations[i], "LATITUDE"])
  lon[i] <- unique(df_of_all_years[df_of_all_years$ID == uniq_stations[i], "LONGITUDE"])
  ele[i] <- unique(df_of_all_years[df_of_all_years$ID == uniq_stations[i], "ELEVATION"])
}

for (j in 1:length(unique(df_of_all_years$ID))){
  error <- 0 
  mse <- 0 
  summ <- 0
  report <- 0 
  
 for (i in 1:length(individ_station[[j]]$DIFFERENCE)){
   error <- abs(individ_station[[j]]$DIFFERENCE[i]) + error
   mse <- (individ_station[[j]]$DIFFERENCE[i])^2 + mse
   summ <- individ_station[[j]]$DIFFERENCE[i] + summ
   report <- report + 1
 }
  MSE_VECTOR[j] <- sqrt(mse / (length(individ_station[[j]]$DIFFERENCE)))
  total_error[j] <- error
  summation[j] <- summ
  reports[j] <- report
  avediff[j] <- summ/ (length(individ_station[[j]]$DIFFERENCE))
}

MSE_VECTOR
total_error
summation
reports
avediff
error_stations <- as.data.frame(cbind(uniq_stations, summation,
                                      total_error, MSE_VECTOR))
error_stations$LATITUDE <- lat
error_stations$LONGITUDE <- lon
error_stations$ELEVATION <- ele
error_stations$REPORTS <- reports
error_stations$AVE_BIAS <- avediff

################################################################################
###### subsetting and looking at those that have all positive errors   #########
################################################################################

class(error_stations$summation)

error_stations$summation <- as.numeric(error_stations$summation)
error_stations$total_error <- as.numeric(error_stations$total_error)
error_stations$MSE_VECTOR <- as.numeric(error_stations$MSE_VECTOR)

class(error_stations$summation)

error_stations_spatial <- error_stations

sp::coordinates(error_stations_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

################################################################################
########   Calculating and extracting the information from PRISM     ###########
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

################################################################################
########   Calculating and extracting the information from PRISM     ###########
################################################################################

# prism_values <- vector("list", length(mon) )
maxtemp <- vector("list", length(mon))
maxvapor <- vector("list", length(mon))
meandew <- vector("list", length(mon))
meantemp <- vector("list", length(mon))
mintemp <- vector("list", length(mon))
minvapor <- vector("list", length(mon))
precip <- vector("list", length(mon))

################################################################################
########   Calculating and extracting the information from PRISM     ###########
################################################################################

# paste0("C:/Users/Logan/Desktop/PRISM/", mon[1], "/monthly/", info[1], "/PRISM_", "tmax_30yr_normal_800mM2_", v[1] , "_asc.asc")

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

error_stations_spatial <- error_stations

sp::coordinates(error_stations_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

################################################################################
########   Calculating and extracting the information from PRISM     ###########
################################################################################

for (i in 1:length(uniq_stations)){
  #print(individ_station[[i]]$ID == uniq_stations[i])
  
  individ_station[[i]]$NOV_maxtemp <- maxtemp[[1]][i]
  individ_station[[i]]$NOV_maxvapor <- maxvapor[[1]][i]
  individ_station[[i]]$NOV_meandew <- meandew[[1]][i]
  individ_station[[i]]$NOV_meantemp <- meantemp[[1]][i]
  individ_station[[i]]$NOV_mintemp <- mintemp[[1]][i]
  individ_station[[i]]$NOV_minvapor <- minvapor[[1]][i]
  individ_station[[i]]$NOV_precip <- precip[[1]][i]
  
  individ_station[[i]]$DEC_maxtemp <- maxtemp[[2]][i]
  individ_station[[i]]$DEC_maxvapor <- maxvapor[[2]][i]
  individ_station[[i]]$DEC_meandew <- meandew[[2]][i]
  individ_station[[i]]$DEC_meantemp <- meantemp[[2]][i]
  individ_station[[i]]$DEC_mintemp <- mintemp[[2]][i]
  individ_station[[i]]$DEC_minvapor <- minvapor[[2]][i]
  individ_station[[i]]$DEC_precip <- precip[[2]][i]
  
  individ_station[[i]]$JAN_maxtemp <- maxtemp[[3]][i]
  individ_station[[i]]$JAN_maxvapor <- maxvapor[[3]][i]
  individ_station[[i]]$JAN_meandew <- meandew[[3]][i]
  individ_station[[i]]$JAN_meantemp <- meantemp[[3]][i]
  individ_station[[i]]$JAN_mintemp <- mintemp[[3]][i]
  individ_station[[i]]$JAN_minvapor <- minvapor[[3]][i]
  individ_station[[i]]$JAN_precip <-  precip[[3]][i]
  
  individ_station[[i]]$FEB_maxtemp <- maxtemp[[4]][i]
  individ_station[[i]]$FEB_maxvapor <- maxvapor[[4]][i]
  individ_station[[i]]$FEB_meandew <- meandew[[4]][i]
  individ_station[[i]]$FEB_meantemp <- meantemp[[4]][i]
  individ_station[[i]]$FEB_mintemp <- mintemp[[4]][i]
  individ_station[[i]]$FEB_minvapor <- minvapor[[4]][i]
  individ_station[[i]]$FEB_precip <- precip[[4]][i]
  
  individ_station[[i]]$MARCH_maxtemp <- maxtemp[[5]][i]
  individ_station[[i]]$MARCH_maxvapor <- maxvapor[[5]][i]
  individ_station[[i]]$MARCH_meandew <- meandew[[5]][i]
  individ_station[[i]]$MARCH_meantemp <- meantemp[[5]][i]
  individ_station[[i]]$MARCH_mintemp <- mintemp[[5]][i]
  individ_station[[i]]$MARCH_minvapor <- minvapor[[5]][i]
  individ_station[[i]]$MARCH_precip <- precip[[5]][i]
  
  individ_station[[i]]$APRIL_maxtemp <- maxtemp[[6]][i]
  individ_station[[i]]$APRIL_maxvapor <- maxvapor[[6]][i]
  individ_station[[i]]$APRIL_meandew <- meandew[[6]][i]
  individ_station[[i]]$APRIL_meantemp <- meantemp[[6]][i]
  individ_station[[i]]$APRIL_mintemp <- mintemp[[6]][i]
  individ_station[[i]]$APRIL_minvapor <- minvapor[[6]][i]
  individ_station[[i]]$APRIL_precip <- precip[[6]][i]
  
  individ_station[[i]]$ANNUAL_maxtemp <- maxtemp[[7]][i]
  individ_station[[i]]$ANNUAL_maxvapor <- maxvapor[[7]][i]
  individ_station[[i]]$ANNUAL_meandew <- meandew[[7]][i]
  individ_station[[i]]$ANNUAL_meantemp <- meantemp[[7]][i]
  individ_station[[i]]$ANNUAL_mintemp <- mintemp[[7]][i]
  individ_station[[i]]$ANNUAL_minvapor <- minvapor[[7]][i]
  individ_station[[i]]$ANNUAL_precip <- precip[[7]][i]
}


# Switch from a list to a data frame with all the observations from the list
# Store the first list as a data frame and then add the rows below.
individ_station_df <- individ_station[[1]]
for (i in 2:length(individ_station)){
  individ_station_df <- rbind(individ_station_df, individ_station[[i]])
}

dim(individ_station_df[individ_station_df$AVE_BIAS == 0, ])

try <- ifelse(individ_station_df$AVE_BIAS > 0, 1, individ_station_df$AVE_BIAS)
try <- ifelse(try < 0, -1, try)
length(try[try == 0])
individ_station_df$CLASS <- try

class(individ_station_df$CLASS)
################################################################################
#### Making a List of all the Stations that have 5 observations ######
################################################################################

sapply(individ_station, lengths)[1, ] > 5
try <- sapply(individ_station, lengths)[1, ] > 5

individ_station5 <- individ_station[try]

individ_station_df5 <- individ_station_df[individ_station_df$obs > 4, ]



################################################################################
#### Making a List of all the Stations that have 5 observations ######
################################################################################

individ_station[[i]]$ID == uniq_stations[i]



################################################################################
#### Making a List of all the Stations that have 5 observations ######
################################################################################

train_df
test_df

sample(nrow(individ_station_df5))


res <- cor(error_stations[, c("AVE_BIAS", "ELEVATION" , "LONGITUDE", 
                              "LATITUDE" , "ANNUAL_maxtemp" ,  "ANNUAL_precip" ,
                              "ANNUAL_minvapor", "ANNUAL_mintemp",
                              "ANNUAL_meantemp" ,  "ANNUAL_meandew" ,
                              "ANNUAL_maxvapor", "MARCH_maxtemp",
                              "MARCH_precip" ,  "MARCH_minvapor"
                              , "MARCH_mintemp" ,  "MARCH_meantemp" ,
                              "MARCH_meandew" ,  "MARCH_maxvapor",
                              "FEB_maxtemp" ,  "FEB_precip" ,
                              "FEB_minvapor", "FEB_mintemp",
                              "FEB_meantemp" ,  "FEB_meandew" ,
                              "FEB_maxvapor", "JAN_maxtemp",
                              "JAN_precip" ,  "JAN_minvapor",
                              "JAN_mintemp" ,  "JAN_meantemp" ,
                              "JAN_meandew" ,  "JAN_maxvapor",
                              "DEC_maxtemp" ,  "DEC_precip" ,
                              "DEC_minvapor", "DEC_mintemp" ,
                              "DEC_meantemp" ,  "DEC_meandew" ,
                              "DEC_maxvapor")])
round(res, 2)


res <- cor(error_stations[, c("AVE_BIAS", "ELEVATION" , "LONGITUDE", 
"LATITUDE" , "ANNUAL_maxtemp" ,  "ANNUAL_precip" ,  "ANNUAL_minvapor"
, "ANNUAL_mintemp" ,  "ANNUAL_meantemp" ,  "ANNUAL_meandew" ,  "ANNUAL_maxvapor")])
round(res, 2)



# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart.Correlation(error_stations[, c("AVE_BIAS", "ELEVATION" , "LONGITUDE", 
                                     "LATITUDE" , "ANNUAL_maxtemp" ,  "ANNUAL_precip" ,
                                     "ANNUAL_minvapor", "ANNUAL_mintemp",
                                     "ANNUAL_meantemp" ,  "ANNUAL_meandew" ,
                                     "ANNUAL_maxvapor")], histogram=TRUE, pch=19)
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart.Correlation(error_stations[, c("AVE_BIAS", "ELEVATION" , "LONGITUDE", 
                                     "LATITUDE" , "ANNUAL_maxtemp" ,  "ANNUAL_precip" ,
                                     "ANNUAL_minvapor", "ANNUAL_mintemp",
                                     "ANNUAL_meantemp" ,  "ANNUAL_meandew" ,
                                     "ANNUAL_maxvapor", "MARCH_maxtemp",
                                     "MARCH_precip" ,  "MARCH_minvapor"
                                     , "MARCH_mintemp" ,  "MARCH_meantemp" ,
                                     "MARCH_meandew" ,  "MARCH_maxvapor",
                                     "FEB_maxtemp" ,  "FEB_precip" ,
                                     "FEB_minvapor", "FEB_mintemp",
                                     "FEB_meantemp" ,  "FEB_meandew" ,
                                     "FEB_maxvapor", "JAN_maxtemp",
                                     "JAN_precip" ,  "JAN_minvapor",
                                     "JAN_mintemp" ,  "JAN_meantemp" ,
                                     "JAN_meandew" ,  "JAN_maxvapor",
                                     "DEC_maxtemp" ,  "DEC_precip" ,
                                     "DEC_minvapor", "DEC_mintemp" ,
                                     "DEC_meantemp" ,  "DEC_meandew" ,
                                     "DEC_maxvapor")], histogram=TRUE, pch=19)

