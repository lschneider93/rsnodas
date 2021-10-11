# We need the coordinates, utah2 doesn't have them but ghcnd_stations does
utah2 #ghcnd_stations

# station_info was up above but it takes the ghcnd_stations that are in utah and 
# returns the "ID", "LATITUDE", "LONGITUDE","ELEVATION", "STATE", and "NAME".
station_info_copy <- station_info

# Changes this into a spatial object that 
coordinates(station_info) <- c("LONGITUDE", "LATITUDE")

# Sp is used to create a spatial object and 
# retrieve spatial coordinates from Spatial object.
# the proj4string is turning it into the correct projection used in the maps
tcrs <- raster::crs(map[[1]])
sp::coordinates(station_info) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(station_info) <- tcrs


################################################################################
########  Plotting the difference of all stations data    ##########
################################################################################


all_diff04
all_diff05
all_diff06
all_diff07
all_diff08
all_diff09
all_diff10
all_diff11
all_diff12
all_diff13
all_diff14
all_diff15
all_diff16
#all_diff17
all_diff18
all_diff19
all_diff20
all_diff21


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################


diff04
diff05
diff06
diff07
diff08
diff09
diff10
diff11
diff12
diff13
diff14
diff15
diff16
#diff17
diff18
diff19
diff20
diff21



stat
stations_2ob

































































station_info
test_values <- vector("list", length(years))
bilinear_values <- vector("list", length(years))
test_values1 <- test_values
bilinear_values1 <- bilinear_values

for (i in 1:length(years)){
  test_values[[i]] <- raster::extract(map[[i]], station_info)
  bilinear_values[[i]] <- raster::extract(map[[i]], station_info, 
                                          method = "bilinear")
}

test_values
#2021, 2020, 2019, 2018
bilinear_values[[18]] <- bilinear_values[[17]]
bilinear_values[[17]] <- bilinear_values[[16]]
bilinear_values[[16]] <- bilinear_values[[15]]
bilinear_values[[15]] <- bilinear_values[[14]]
bilinear_values[[14]] <- NULL

test_values[[18]] <- test_values[[17]]
test_values[[17]] <- test_values[[16]]
test_values[[16]] <- test_values[[15]]
test_values[[15]] <- test_values[[14]]
test_values[[14]] <- 0

bilinear_values
test_values



test_values
ut_stat_info






for (i in 1:length(years)){
  test_values1[[i]] <- raster::extract(utah_april_swe_rasters[[i]], station_info)
  bilinear_values1[[i]] <- raster::extract(utah_april_swe_rasters[[i]], station_info, 
                                          method = "bilinear")
}

test_values[[1]] == test_values1[[1]]
length(test_values[[1]])
length(test_values1[[1]])






































######################################################################
load("C:/Users/Logan/Desktop/snowload2-master/data/ghcnd_stations.rda")
years <- c(2004:2016, 2017:2021)

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
ut_stat_info <- station_info[, c("ID", "LATITUDE", "LONGITUDE", 
                                 "ELEVATION", "STATE", "NAME")]

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
  #ghcnd_stations <- snowload2::
  #load("ghcnd_stations.rda")
  #ghcnd_stations <- ghcnd_stations
  #load("C:/Users/Logan/Desktop/snowload2-master/data/ghcnd_stations.rda")
  stations <- ghcnd_stations$ID[ghcnd_stations$STATE %in% states]
  get_station_data(stations, source, elem, progress)
}


######################################################################
