################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################

# load the GHCND Station DATA
load("~/Desktop/Graduate Studies/Research/snowload2-master/data/ghcnd_stations.rda")
load("snowload2-master/data/ghcnd_stations.rda") # for Windows
years <- c(2004:2016, 2018:2021)

# Vector of utah Stations
utah_STATIONS <- ghcnd_stations$ID[ghcnd_stations$STATE %in% "UT"]

# make a data frame of just the UTAH stations
station_info <- data.frame(ghcnd_stations[ghcnd_stations$ID == utah_STATIONS[1], ])

# appending a line for each station in Utah
for (i in 2:length(utah_STATIONS)){
  station_info <- rbind(station_info,ghcnd_stations[ghcnd_stations$ID == utah_STATIONS[i], ] )
}

# Just keeping the columns of ID, LAT, LONG, ELEV, STATE, and NAME
# We will add columns to this station_info
station_info <- station_info[, c("ID", "LATITUDE", "LONGITUDE",
                                 "ELEVATION", "STATE", "NAME")]
ut_stat_info <- station_info

# GETTING THE DATA from the GCHD ALL FOLDER
# Define helper functions
#=============================================================================
# extract()
#

get_station_data <- function(stations, source, elem = c("SNWD", "WESD"), progress = TRUE){
  # Set constants
  #=============================================================================
  files <- paste0(source, stations, ".dly")
  month_days <- 1:31
  max_days <- max(month_days)
  
  # Indices for fixed width file item locations identified from:
  # ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
  # Section 3
  id_start <- 1
  id_end <- 11
  year_start <- 12
  year_end <- 15
  month_start <- 16
  month_end <- 17
  elem_start <- 18
  elem_end <- 21
  value_start <- month_days * 8 + 14
  value_end <- value_start + 4
  mflag_start <- value_end + 1
  mflag_end <- mflag_start
  qflag_start <- mflag_start + 1
  qflag_end <- qflag_start
  sflag_start <- qflag_start + 1
  sflag_end <- sflag_start
  
  
  # Define helper functions
  #=============================================================================
  # extract()
  #
  # A wrapper for vapply with substring that extracts a vector of characters
  # from each string
  extract <- function(strings, start, end) {
    vapply(strings,
           substring,
           character(length(start)),
           USE.NAMES = FALSE,
           start,
           end)
  }
  
  # extract_dates()
  #
  # A wrapper for extract that extracts a vector of dates for a given month in
  # each string
  extract_dates <- function(strings) {
    paste(rep(extract(strings, year_start, year_end), each = max_days),
          rep(extract(strings, month_start, month_end), each = max_days),
          rep(month_days, times = length(strings)),
          sep = "-")
  }
  
  
  # Extract data for each station in station vector
  #=============================================================================
  data_list <- vector("list", length(files))
  # Create a progress bar for the data download if requested. (utils package)
  if(progress){
    pb <- txtProgressBar(min = 0, max = length(files), style = 3)
  }
  for (i in 1:length(files)) {
    # Read the lines from the FTP file
    dly <- try(readLines(files[i]), silent = TRUE)
    if(inherits(dly, "try-error")){
      print(paste("No viable file found at ", files[i], sep = ""))
      data_list[[i]] <- NULL
      next
    }
    
    # Remove lines that do not represent requried elements
    dly <- dly[substring(dly, elem_start, elem_end) %in% elem]
    
    
    # Manipulate lines into data.frame
    data_list[[i]] <- data.frame(
      "ID" = rep(substring(dly, id_start, id_end),
                 each = max_days),
      "DATE" = as.Date(extract_dates(dly)),
      "ELEMENT" = rep(substring(dly, elem_start, elem_end),
                      each = max_days),
      "VALUE" = as.numeric(extract(dly, value_start, value_end)),
      "MFLAG" = as.character(extract(dly, mflag_start, mflag_end)),
      "QFLAG" = as.character(extract(dly, qflag_start, qflag_end)),
      "SFLAG" = as.character(extract(dly, sflag_start, sflag_end)),
      stringsAsFactors = FALSE
    )
    
    if (nrow(data_list[[i]]) == 0) {
      data_list[[i]] <- NULL
    }
    
    if(progress){
      setTxtProgressBar(pb, i)
    }
  }
  if(progress){
    message("") # hack to start next messages on new line
  }
  
  # Combine into single data.frame and remove missing values
  #=============================================================================
  tdata <- dplyr::bind_rows(data_list)
  
  # Return data.frame
  if (is.null(tdata) || nrow(tdata) == 0) {
    return(NULL)
  } else {
    tdata <- tdata[tdata$VALUE != -9999,] # -9999 is defined as missing
    return(tdata)
  }
  
}

get_state_data <- function(states, source, elem = c("SNWD", "WESD"), progress = TRUE) {
  stations <- ghcnd_stations$ID[ghcnd_stations$STATE %in% states]
  get_station_data(stations, source, elem, progress)
}


######################################################################
# Saving the data with the WESD
# "~/Desktop/Graduate classes 2021 Spring/Research/snowload2-master/data/ghcnd_stations.rda"
utah2 <- get_state_data("UT", source ="~/Desktop/Graduate Studies/Research/rsnodas/ghcnd_all/",
                        elem = "WESD")
utah2 <- get_state_data("UT", source ="ghcnd_all/",
                        elem = "WESD")

# break up by the dates.
april_df_ghcnd <- vector("list", length(years))

# Extract data for each station for each year and put it in a different element of a list
#=============================================================================
for (i in 1:length(years)){
  april_df_ghcnd[[i]] <- utah2[utah2$DATE ==
                                 paste0(years[i], "-04-01"), ]
}

################################################################################
#####   Creating and storing the vectors of ELEVATION, LONG, and LAT    ########
################################################################################
# Initialize the vector of Elevation, Long and LAT for each element in the list
for (j in 1:length(april_df_ghcnd)){
  april_df_ghcnd[[j]]$ELEVATION <- 0
  april_df_ghcnd[[j]]$LONGITUDE <- 0
  april_df_ghcnd[[j]]$LATITUDE <- 0
  april_df_ghcnd[[j]]$NAME <- " "
  april_df_ghcnd[[j]]$ANNUAL_maxtemp <- 0
  april_df_ghcnd[[j]]$ANNUAL_maxvapor <- 0
  april_df_ghcnd[[j]]$ANNUAL_meandew <- 0
  april_df_ghcnd[[j]]$ANNUAL_meantemp <- 0
  april_df_ghcnd[[j]]$ANNUAL_mintemp <- 0
  april_df_ghcnd[[j]]$ANNUAL_minvapor <- 0
  april_df_ghcnd[[j]]$ANNUAL_precip <- 0
  april_df_ghcnd[[j]]$VALUE <- (april_df_ghcnd[[j]]$VALUE / 10)
}

ut_stat_info

ut_stat_info_spatial <- ut_stat_info

sp::coordinates(ut_stat_info_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(ut_stat_info_spatial) <- "+proj=longlat +datum=WGS84 +no_defs"

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them  #########
################################################################################
# mon <- c('nov', 'dec', 'jan', 'feb', 'march', 'april', 'annual')
mon <- c('annual')
# v <- c(11, 12, "01", "02", "03", "04", "annual")
v <- c("annual")
info <- c('max temp',
          "maximum vapor pressure deficit",
          "mean dew point temperature",
          "mean temp",
          "min temp",
          "minimum vapor pressure deficit",
          "precipitation",
          "elevation")
file_type <- c("tmax_30yr_normal_800mM2_",
               "vpdmax_30yr_normal_800mM2_",
               "tdmean_30yr_normal_800mM2_",
               "tmean_30yr_normal_800mM2_",
               "tmin_30yr_normal_800mM2_",
               "vpdmin_30yr_normal_800mM2_",
               "ppt_30yr_normal_800mM2_",
               "us_dem_800m_")

# prism_values <- vector("list", length(mon) )
maxtemp <- vector("list", length(mon))
maxvapor <- vector("list", length(mon))
meandew <- vector("list", length(mon))
meantemp <- vector("list", length(mon))
mintemp <- vector("list", length(mon))
minvapor <- vector("list", length(mon))
precip <- vector("list", length(mon))
elevation <- vector("list", length(mon))

# ~/Desktop/Graduate Studies/Research/rsnodas/ghcnd_all/
for (i in 1:length(mon)){
  # raster::raster(paste0("~/Desktop/PRISM/", mon[i],
  r <- raster::raster(paste0("PRISM/", mon[i],
                             "/monthly/", info[1], "/PRISM_",
                             file_type[1], v[i] , "_asc.asc"))
  r2 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[2], "/PRISM_",
                              file_type[2], v[i] , "_asc.asc"))
  r3 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[3], "/PRISM_",
                              file_type[3], v[i] , "_asc.asc"))
  r4 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[4], "/PRISM_",
                              file_type[4], v[i] , "_asc.asc"))
  r5 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[5], "/PRISM_",
                              file_type[5], v[i] , "_asc.asc"))
  r6 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[6], "/PRISM_",
                              file_type[6], v[i] , "_asc.asc"))
  r7 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[7], "/PRISM_",
                              file_type[7], v[i] , "_asc.asc"))
  r8 <- raster::raster(paste0("PRISM/", mon[i],
                              "/monthly/", info[8], "/PRISM_",
                              file_type[8], "asc.asc"))
  
  new_r <- raster::projectRaster(r, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r2 <- raster::projectRaster(r2, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r3 <- raster::projectRaster(r3, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r4 <- raster::projectRaster(r4, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r5 <- raster::projectRaster(r5, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r6 <- raster::projectRaster(r6, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r7 <- raster::projectRaster(r7, crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_r8 <- raster::projectRaster(r8, crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  maxtemp[[i]] <- raster::extract(new_r, ut_stat_info_spatial)
  maxvapor[[i]] <- raster::extract(new_r2, ut_stat_info_spatial)
  meandew[[i]] <- raster::extract(new_r3, ut_stat_info_spatial)
  meantemp[[i]] <- raster::extract(new_r4 , ut_stat_info_spatial)
  mintemp[[i]] <- raster::extract(new_r5, ut_stat_info_spatial)
  minvapor[[i]] <- raster::extract(new_r6, ut_stat_info_spatial)
  precip[[i]] <- raster::extract(new_r7 , ut_stat_info_spatial)
  elevation[[i]] <- raster::extract(new_r8, ut_stat_info_spatial)
}

# ut_stat_info$NOV_maxtemp <- maxtemp[[1]]
# ut_stat_info$NOV_maxvapor <- maxvapor[[1]]
# ut_stat_info$NOV_meandew <- meandew[[1]]
# ut_stat_info$NOV_meantemp <- meantemp[[1]]
# ut_stat_info$NOV_mintemp <- mintemp[[1]]
# ut_stat_info$NOV_minvapor <- minvapor[[1]]
# ut_stat_info$NOV_precip <- precip[[1]]
# ut_stat_info$NOV_elevation <- elevation[[1]]
#
# ut_stat_info$DEC_maxtemp <- maxtemp[[2]]
# ut_stat_info$DEC_maxvapor <- maxvapor[[2]]
# ut_stat_info$DEC_meandew <- meandew[[2]]
# ut_stat_info$DEC_meantemp <- meantemp[[2]]
# ut_stat_info$DEC_mintemp <- mintemp[[2]]
# ut_stat_info$DEC_minvapor <- minvapor[[2]]
# ut_stat_info$DEC_precip <- precip[[2]]
# ut_stat_info$DEC_elevation <- elevation[[2]]
#
# ut_stat_info$JAN_maxtemp <- maxtemp[[3]]
# ut_stat_info$JAN_maxvapor <- maxvapor[[3]]
# ut_stat_info$JAN_meandew <- meandew[[3]]
# ut_stat_info$JAN_meantemp <- meantemp[[3]]
# ut_stat_info$JAN_mintemp <- mintemp[[3]]
# ut_stat_info$JAN_minvapor <- minvapor[[3]]
# ut_stat_info$JAN_precip <- precip[[3]]
# ut_stat_info$JAN_elevation <- elevation[[3]]
#
# ut_stat_info$FEB_maxtemp <- maxtemp[[4]]
# ut_stat_info$FEB_maxvapor <- maxvapor[[4]]
# ut_stat_info$FEB_meandew <- meandew[[4]]
# ut_stat_info$FEB_meantemp <- meantemp[[4]]
# ut_stat_info$FEB_mintemp <- mintemp[[4]]
# ut_stat_info$FEB_minvapor <- minvapor[[4]]
# ut_stat_info$FEB_precip <- precip[[4]]
# ut_stat_info$FEB_elevation <- elevation[[4]]
#
# ut_stat_info$MARCH_maxtemp <- maxtemp[[5]]
# ut_stat_info$MARCH_maxvapor <- maxvapor[[5]]
# ut_stat_info$MARCH_meandew <- meandew[[5]]
# ut_stat_info$MARCH_meantemp <- meantemp[[5]]
# ut_stat_info$MARCH_mintemp <- mintemp[[5]]
# ut_stat_info$MARCH_minvapor <- minvapor[[5]]
# ut_stat_info$MARCH_precip <- precip[[5]]
# ut_stat_info$MARCH_elevation <- elevation[[5]]
#
# ut_stat_info$APRIL_maxtemp <- maxtemp[[6]]
# ut_stat_info$APRIL_maxvapor <- maxvapor[[6]]
# ut_stat_info$APRIL_meandew <- meandew[[6]]
# ut_stat_info$APRIL_meantemp <- meantemp[[6]]
# ut_stat_info$APRIL_mintemp <- mintemp[[6]]
# ut_stat_info$APRIL_minvapor <- minvapor[[6]]
# ut_stat_info$APRIL_precip <- precip[[6]]
# ut_stat_info$APRIL_elevation <- elevation[[6]]

ut_stat_info$ANNUAL_maxtemp <- maxtemp[[1]]
ut_stat_info$ANNUAL_maxvapor <- maxvapor[[1]]
ut_stat_info$ANNUAL_meandew <- meandew[[1]]
ut_stat_info$ANNUAL_meantemp <- meantemp[[1]]
ut_stat_info$ANNUAL_mintemp <- mintemp[[1]]
ut_stat_info$ANNUAL_minvapor <- minvapor[[1]]
ut_stat_info$ANNUAL_precip <- precip[[1]]
ut_stat_info$ANNUAL_elevation <- elevation[[1]]

ut_stat_info$ELEVATION_DIFFERENCE <- (ut_stat_info$ELEVATION - ut_stat_info$ANNUAL_elevation) /
  ut_stat_info$ELEVATION * 100

bad_elevation_stations <- ut_stat_info[ut_stat_info$ELEVATION_DIFFERENCE > 10 |
                                         ut_stat_info$ELEVATION_DIFFERENCE < -10,
                                       c("NAME", "ID", "ELEVATION", "ANNUAL_elevation",
                                         "LONGITUDE", "LATITUDE", "ELEVATION_DIFFERENCE")]

# Go through each list, (list for each year)
for (j in 1:length(april_df_ghcnd)){
  
  # then look through for each station in that list
  for (i in 1:length(april_df_ghcnd[[j]]$ID)){
    
    # create a logical vector of when the element Station ID matches
    w <- april_df_ghcnd[[j]]$ID[i] == ut_stat_info$ID
    
    # extracts the ELEVATION, LONG, and LAT of that Station
    april_df_ghcnd[[j]]$ELEVATION[i] <- ut_stat_info[w, "ELEVATION"]
    april_df_ghcnd[[j]]$LONGITUDE[i] <- ut_stat_info[w, "LONGITUDE"]
    april_df_ghcnd[[j]]$LATITUDE[i] <- ut_stat_info[w, "LATITUDE"]
    april_df_ghcnd[[j]]$NAME[i] <- ut_stat_info[w, "NAME"]
    april_df_ghcnd[[j]]$ANNUAL_maxtemp[i] <- ut_stat_info[w, "ANNUAL_maxtemp"]
    april_df_ghcnd[[j]]$ANNUAL_maxvapor[i] <- ut_stat_info[w, "ANNUAL_maxvapor"]
    april_df_ghcnd[[j]]$ANNUAL_meandew[i] <- ut_stat_info[w, "ANNUAL_meandew"]
    april_df_ghcnd[[j]]$ANNUAL_meantemp[i] <- ut_stat_info[w, "ANNUAL_meantemp"]
    april_df_ghcnd[[j]]$ANNUAL_mintemp[i] <- ut_stat_info[w, "ANNUAL_mintemp"]
    april_df_ghcnd[[j]]$ANNUAL_maxvapor[i] <- ut_stat_info[w, "ANNUAL_maxvapor"]
    april_df_ghcnd[[j]]$ANNUAL_minvapor[i] <- ut_stat_info[w, "ANNUAL_minvapor"]
    april_df_ghcnd[[j]]$ANNUAL_precip[i] <- ut_stat_info[w, "ANNUAL_precip"]
    april_df_ghcnd[[j]]$ANNUAL_elevation[i] <- ut_stat_info[w, "ANNUAL_elevation"]
    april_df_ghcnd[[j]]$ELEVATION_difference[i] <- ut_stat_info[w, "ELEVATION_DIFFERENCE"]
  }
}


# unique(individ_station_df[is.element(individ_station_df$ID ,bad_elevation_stations), ]$NAME)

# individ_station_df[is.element(individ_station_df$ID ,bad_elevation_stations), c("NAME", "ELEVATION", "ANNUAL_elevation") ]

################################################################################
##########       Creating and Spatial Points Data Frame       ################
################################################################################
april_df_ghcnd_spatial <- april_df_ghcnd
tcrs <- raster::crs(map[[1]])

for (j in 1:length(april_df_ghcnd_spatial)){
  sp::coordinates(april_df_ghcnd_spatial[[j]]) <- c("LONGITUDE", "LATITUDE")
  sp::proj4string(april_df_ghcnd_spatial[[j]]) <- "+proj=longlat +datum=WGS84 +no_defs"
}

################################################################################
######  Storing the values of the and Extracting values from the model  ########
################################################################################

test_values <- vector("list", length(years))
bilinear_values <- vector("list", length(years))

# for each list (year) we will go through and extract values at the stations
for (i in 1:length(years)){
  
  # extracting the values at the stations in the april_df_ghcnd_spatial
  test_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]])
  bilinear_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]],
                                          method = "bilinear")
}

################################################################################
########  Calculating the difference and storing that information    ###########
################################################################################
for (j in 1:length(years)){
  april_df_ghcnd_spatial[[j]]$MODEL_WESD <- test_values[[j]]
  april_df_ghcnd[[j]]$MODEL_WESD <- test_values[[j]]
}

################################################################################
######## Calculating the difference and storing that information     ###########
################################################################################

for (j in 1:length(years)){
  april_df_ghcnd_spatial[[j]]$MODEL_DIFFERENCE <- april_df_ghcnd_spatial[[j]]$VALUE -
    april_df_ghcnd_spatial[[j]]$MODEL_WESD
  april_df_ghcnd[[j]]$MODEL_DIFFERENCE <- april_df_ghcnd[[j]]$VALUE -
    april_df_ghcnd[[j]]$MODEL_WESD
}

# PLotting the difference vs Elevation
plot(april_df_ghcnd[[1]]$ELEVATION, april_df_ghcnd[[1]]$DIFFERENCE)

# Plotting elevation with difference for each year
for (j in 1:length(years)){
  plot(april_df_ghcnd[[j]]$ELEVATION, april_df_ghcnd[[j]]$DIFFERENCE,
       main = paste("Residuals for the year", years[j]))
}
?trunc()
################################################################################
######  Making a Long Data Frame of the VAriables to see all of them   #########
################################################################################
d <- signif(april_df_ghcnd[[1]]$LATITUDE, digits = 6)
df_gchnd_of_all_years <- rbind(april_df_ghcnd[[1]], april_df_ghcnd[[2]], april_df_ghcnd[[3]],
                         april_df_ghcnd[[4]], april_df_ghcnd[[5]], april_df_ghcnd[[6]],
                         april_df_ghcnd[[7]], april_df_ghcnd[[8]], april_df_ghcnd[[9]],
                         april_df_ghcnd[[10]], april_df_ghcnd[[11]], april_df_ghcnd[[12]],
                         april_df_ghcnd[[13]], april_df_ghcnd[[14]], april_df_ghcnd[[15]],
                         april_df_ghcnd[[16]], april_df_ghcnd[[17]])

df_gchnd_of_all_years$DATE <- as.factor(df_gchnd_of_all_years$DATE)
station5 <- unique(df_gchnd_of_all_years[df_gchnd_of_all_years$ELEVATION_difference > 10 |
                                     df_gchnd_of_all_years$ELEVATION_difference < -10,
                                   c("NAME", "ELEVATION", "ANNUAL_elevation", "ELEVATION_difference",
                                     "LATITUDE", "LONGITUDE")])

df_gchnd_of_all_years_copy <- df_gchnd_of_all_years
df_gchnd_of_all_years_spatial <- df_gchnd_of_all_years

# sp::proj4string(df_gchnd_of_all_years_spatial) <- tcrs
sp::coordinates(df_gchnd_of_all_years_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(df_gchnd_of_all_years_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

write.csv(df_gchnd_of_all_years,"april_data.csv", row.names = TRUE)

################################################################################
######  Visualization of stations that are higher  #########
################################################################################

for(i in years){
  # print(paste0(i, '-04-01'))
  print(sp::bubble(df_gchnd_of_all_years_spatial[df_gchnd_of_all_years_spatial$DATE
                                           == paste0(i, '-04-01'), ],
                   'MODEL_DIFFERENCE', main = paste0(i)))
  Sys.sleep(1)
}

################################################################################
###### Looking at a Certain Station Every Year #########
################################################################################
uniq_stations <- unique(df_gchnd_of_all_years$ID)# 155 stations

unique(df_gchnd_of_all_years$ID[df_gchnd_of_all_years$DATE == "2004-04-01"]) # 85
l <- numeric()
for (i in 1:length(years)) {
  l[i] <- dim(april_df_ghcnd[[i]])[1]
}
l

################################################################################
#### Making a List of all the Individual Stations with Data at some point ######
################################################################################
individ_station <- vector("list", length(unique(df_gchnd_of_all_years$ID)))

# this is creating a list of each station that has an observation
for (j in 1:length(unique(df_gchnd_of_all_years$ID))){
  station_of_interest <- unique(df_gchnd_of_all_years$ID)[j]
  individ_station[[j]] <- df_gchnd_of_all_years[df_gchnd_of_all_years$ID == station_of_interest, ]
}

# This is calculating the average and median bias of the individual stations
for (i in 1:length(individ_station)){
  individ_station[[i]]$AVE_BIAS <- mean(individ_station[[i]]$DIFFERENCE)
  individ_station[[i]]$MED_BIAS <- median(individ_station[[i]]$DIFFERENCE)
  individ_station[[i]]$ELEVATION_DIFF_PERCENT <- (individ_station[[i]]$ELEVATION -
                                                    individ_station[[i]]$ANNUAL_elevation) /
    individ_station[[i]]$ELEVATION * 100
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
###### Calculating the Absolute Sum of the Difference, MSE, Total Error  #####
################################################################################
# MSE_LIST <- vector("list", length(unique(df_gchnd_of_all_years$ID)))
MSE_VECTOR <- numeric()
total_error <- numeric()
summation <- numeric()
avediff <- numeric()
lat <- numeric()
lon <- numeric()
ele <- numeric()
reports <- numeric()
station_median <- numeric()

for (i in 1:length(uniq_stations)){
  lat[i] <- unique(df_gchnd_of_all_years[df_gchnd_of_all_years$ID == uniq_stations[i], "LATITUDE"])
  lon[i] <- unique(df_gchnd_of_all_years[df_gchnd_of_all_years$ID == uniq_stations[i], "LONGITUDE"])
  ele[i] <- unique(df_gchnd_of_all_years[df_gchnd_of_all_years$ID == uniq_stations[i], "ELEVATION"])
}

for (j in 1:length(unique(df_gchnd_of_all_years$ID))){
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
  station_median[j] <- median(individ_station[[j]]$DIFFERENCE)
}

error_stations <- as.data.frame(cbind(uniq_stations, summation,
                                      total_error, MSE_VECTOR))
error_stations$LATITUDE <- lat
error_stations$LONGITUDE <- lon
error_stations$ELEVATION <- ele
error_stations$REPORTS <- reports
error_stations$AVE_BIAS <- avediff
error_stations$STATION_MEDIAN <- station_median

################################################################################
######  Subsetting and looking at those that have all positive errors  #########
################################################################################

class(error_stations$summation)

error_stations$summation <- as.numeric(error_stations$summation)
error_stations$total_error <- as.numeric(error_stations$total_error)
error_stations$MSE_VECTOR <- as.numeric(error_stations$MSE_VECTOR)
error_stations$STATION_MEDIAN <- as.numeric(error_stations$STATION_MEDIAN)

class(error_stations$summation)

error_stations_spatial <- error_stations

sp::coordinates(error_stations_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

################################################################################
########   Calculating and extracting the information from PRISM     ###########
################################################################################

for (i in 1:length(uniq_stations)){
  individ_station[[i]]$NOV_maxtemp <- maxtemp[[1]][i]
  individ_station[[i]]$NOV_maxvapor <- maxvapor[[1]][i]
  individ_station[[i]]$NOV_meandew <- meandew[[1]][i]
  individ_station[[i]]$NOV_meantemp <- meantemp[[1]][i]
  individ_station[[i]]$NOV_mintemp <- mintemp[[1]][i]
  individ_station[[i]]$NOV_minvapor <- minvapor[[1]][i]
  individ_station[[i]]$NOV_precip <- precip[[1]][i]
  # individ_station[[i]]$NOV_elevation <- elevation[[1]][i]
  
  individ_station[[i]]$DEC_maxtemp <- maxtemp[[2]][i]
  individ_station[[i]]$DEC_maxvapor <- maxvapor[[2]][i]
  individ_station[[i]]$DEC_meandew <- meandew[[2]][i]
  individ_station[[i]]$DEC_meantemp <- meantemp[[2]][i]
  individ_station[[i]]$DEC_mintemp <- mintemp[[2]][i]
  individ_station[[i]]$DEC_minvapor <- minvapor[[2]][i]
  individ_station[[i]]$DEC_precip <- precip[[2]][i]
  # individ_station[[i]]$DEC_elevation <- elevation[[2]][i]
  
  individ_station[[i]]$JAN_maxtemp <- maxtemp[[3]][i]
  individ_station[[i]]$JAN_maxvapor <- maxvapor[[3]][i]
  individ_station[[i]]$JAN_meandew <- meandew[[3]][i]
  individ_station[[i]]$JAN_meantemp <- meantemp[[3]][i]
  individ_station[[i]]$JAN_mintemp <- mintemp[[3]][i]
  individ_station[[i]]$JAN_minvapor <- minvapor[[3]][i]
  individ_station[[i]]$JAN_precip <- precip[[3]][i]
  # individ_station[[i]]$JAN_elevation <- elevation[[3]][i]
  
  individ_station[[i]]$FEB_maxtemp <- maxtemp[[4]][i]
  individ_station[[i]]$FEB_maxvapor <- maxvapor[[4]][i]
  individ_station[[i]]$FEB_meandew <- meandew[[4]][i]
  individ_station[[i]]$FEB_meantemp <- meantemp[[4]][i]
  individ_station[[i]]$FEB_mintemp <- mintemp[[4]][i]
  individ_station[[i]]$FEB_minvapor <- minvapor[[4]][i]
  individ_station[[i]]$FEB_precip <- precip[[4]][i]
  # individ_station[[i]]$FEB_elevation <- elevation[[4]][i]
  
  individ_station[[i]]$MARCH_maxtemp <- maxtemp[[5]][i]
  individ_station[[i]]$MARCH_maxvapor <- maxvapor[[5]][i]
  individ_station[[i]]$MARCH_meandew <- meandew[[5]][i]
  individ_station[[i]]$MARCH_meantemp <- meantemp[[5]][i]
  individ_station[[i]]$MARCH_mintemp <- mintemp[[5]][i]
  individ_station[[i]]$MARCH_minvapor <- minvapor[[5]][i]
  individ_station[[i]]$MARCH_precip <- precip[[5]][i]
  # individ_station[[i]]$MARCH_precip <- elevation[[5]][i]
  
  individ_station[[i]]$APRIL_maxtemp <- maxtemp[[6]][i]
  individ_station[[i]]$APRIL_maxvapor <- maxvapor[[6]][i]
  individ_station[[i]]$APRIL_meandew <- meandew[[6]][i]
  individ_station[[i]]$APRIL_meantemp <- meantemp[[6]][i]
  individ_station[[i]]$APRIL_mintemp <- mintemp[[6]][i]
  individ_station[[i]]$APRIL_minvapor <- minvapor[[6]][i]
  individ_station[[i]]$APRIL_precip <- precip[[6]][i]
  # individ_station[[i]]$APRIL_elevation <- elevation[[6]][i]
  
  individ_station[[i]]$ANNUAL_maxtemp <- maxtemp[[7]][i]
  individ_station[[i]]$ANNUAL_maxvapor <- maxvapor[[7]][i]
  individ_station[[i]]$ANNUAL_meandew <- meandew[[7]][i]
  individ_station[[i]]$ANNUAL_meantemp <- meantemp[[7]][i]
  individ_station[[i]]$ANNUAL_mintemp <- mintemp[[7]][i]
  individ_station[[i]]$ANNUAL_minvapor <- minvapor[[7]][i]
  individ_station[[i]]$ANNUAL_precip <- precip[[7]][i]
  # individ_station[[i]]$ANNUAL_elevation <- elevation[[7]][i]
}

# Switch from a list to a data frame with all the observations from the list
# Store the first list as a data frame and then add the rows below.
individ_station_df <- individ_station[[1]]
for (i in 2:length(individ_station)){
  individ_station_df <- rbind(individ_station_df, individ_station[[i]])
}

################################################################################
####  adding new variables into the error stations (summary of stations) #######
################################################################################
error_stations$ANNUAL_maxtemp <- 0
error_stations$ANNUAL_maxvapor <- 0
error_stations$ANNUAL_meandew <- 0
error_stations$ANNUAL_meantemp <- 0
error_stations$ANNUAL_mintemp <- 0
error_stations$ANNUAL_minvapor <- 0
error_stations$ANNUAL_precip <- 0
error_stations$ANNUAL_elevation <- 0

for (i in 1:length(error_stations$uniq_stations)){
  w <- ut_stat_info[ut_stat_info$ID == error_stations$uniq_stations[i], ]
  error_stations$ANNUAL_maxtemp[i] <- w$ANNUAL_maxtemp
  error_stations$ANNUAL_maxvapor[i] <- w$ANNUAL_maxvapor
  error_stations$ANNUAL_meandew[i] <- w$ANNUAL_meandew
  error_stations$ANNUAL_meantemp[i] <- w$ANNUAL_meantemp
  error_stations$ANNUAL_mintemp[i] <- w$ANNUAL_mintemp
  error_stations$ANNUAL_minvapor[i] <- w$ANNUAL_minvapor
  error_stations$ANNUAL_precip[i] <- w$ANNUAL_precip
  error_stations$ANNUAL_elevation[i] <- w$ANNUAL_elevation
}

error_stations[, c("ANNUAL_elevation", "ELEVATION")]

error_stations$PRISM_elevation_difference <- (error_stations$ELEVATION -
                                                error_stations$ANNUAL_elevation) / error_stations$ANNUAL_elevation * 100

elevation_bad <- error_stations[error_stations$PRISM_elevation_difference > 10 |
                                  error_stations$PRISM_elevation_difference < -10, ]$uniq_stations

elevation_bad <- error_stations[error_stations$PRISM_elevation_difference > 10 |
                                  error_stations$PRISM_elevation_difference < -10, ]$uniq_stations

ut_stat_info[, c("NAME", "ELEVATION", "ANNUAL_elevation")]
individ_station_df[, c("APRIL_elevation", "ELEVATION")]
individ_station_df$PRISM_elevation_difference_percent <- (individ_station_df$ELEVATION -
                                                            individ_station_df$ANNUAL_elevation) /
  individ_station_df$ANNUAL_elevation *100
################################################################################
####  adding new variables into the error stations (summary of stations) #######
################################################################################

?is.element()

unique(individ_station_df[is.element(individ_station_df$ID, elevation_bad), ]$NAME)

individ_station_df[is.element(individ_station_df$ID, elevation_bad), c("ELEVATION", "ANNUAL_elevation", "")]


individ_station_df

df_gchnd_of_all_years$ELEVATION_DIFF_PERCENT <- (df_gchnd_of_all_years$ELEVATION - df_gchnd_of_all_years$ANNUAL_elevation) /
  df_gchnd_of_all_years$ELEVATION * 100

unique(df_gchnd_of_all_years[df_gchnd_of_all_years$ELEVATION_difference < -10 |
                         df_gchnd_of_all_years$ELEVATION_difference > 10, c("NAME", "ELEVATION",
                                                                      "ANNUAL_elevation",
                                                                      "ELEVATION_difference",
                                                                      "LONGITUDE",
                                                                      "LATITUDE") ])

mean(df_gchnd_of_all_years$DIFFERENCE)
median(df_gchnd_of_all_years$DIFFERENCE)

for (i in 1:length(april_df_ghcnd)){
  j <- (mean(april_df_ghcnd[[i]]$DIFFERENCE))
  k <- (median(april_df_ghcnd[[i]]$DIFFERENCE))
  print(c(years[i], j, k))
}

dim(ut_stat_info)

ut_stat_info[ut_stat_info$ELEVATION_DIFFERENCE < -10 |
               ut_stat_info$ELEVATION_DIFFERENCE > 10,
             c("NAME", "ELEVATION", "ANNUAL_elevation", "ELEVATION_DIFFERENCE") ]


elevation2
