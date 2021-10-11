#Downloading all the DATA:
getwd()
download_all_stations()

# load the GHCND Station DATA
load("C:/Users/Logan/Desktop/snowload2-master/data/ghcnd_stations.rda")
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
# Saving the data with the WESD
utah2 <- get_state_data("UT", source ="C:/Users/Logan/Desktop/ghcnd_all/",
                        elem = "WESD")

# break up by the dates.
april_df_ghcnd <- vector("list", length(years))

# Extract data for each station for each year and put it in a different element of a list
#=============================================================================
for (i in 1:length(years)){
  april_df_ghcnd[[i]] <- utah2[utah2$DATE == 
                                 paste0(years[1], "-04-01"), ]
}

# This has the DATE, ID, and the VALUE. Need Coordinates
april_df_ghcnd[[1]]

# This gives us a list and each element is a different year.  
# It goes from 2004-2021.
april_df_ghcnd

################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################

#april_2004 == april_df_ghcnd[[1]]
april_2004 <- utah2[utah2$DATE == "2004-04-01",]
april_2005 <- utah2[utah2$DATE == "2005-04-01",]
april_2006 <- utah2[utah2$DATE == "2006-04-01",]
april_2007 <- utah2[utah2$DATE == "2007-04-01",]
april_2008 <- utah2[utah2$DATE == "2008-04-01",]
april_2009 <- utah2[utah2$DATE == "2009-04-01",]
april_2010 <- utah2[utah2$DATE == "2010-04-01",]
april_2011 <- utah2[utah2$DATE == "2011-04-01",]
april_2012 <- utah2[utah2$DATE == "2012-04-01",]
april_2013 <- utah2[utah2$DATE == "2013-04-01",]
april_2014 <- utah2[utah2$DATE == "2014-04-01",]
april_2015 <- utah2[utah2$DATE == "2015-04-01",]
april_2016 <- utah2[utah2$DATE == "2016-04-01",]
#april_2017 <- utah2[utah2$DATE == "2017-04-01",]
april_2018 <- utah2[utah2$DATE == "2018-04-01",]
april_2019 <- utah2[utah2$DATE == "2019-04-01",]
april_2020 <- utah2[utah2$DATE == "2020-04-01",]
april_2021 <- utah2[utah2$DATE == "2021-04-01",]

# adding an extra column that will be LABELED WESD
WESD <- rep(0, length(ut_stat_info))

dim(station_info)
a2004 <- cbind(ut_stat_info, WESD)
a2005 <- cbind(ut_stat_info, WESD)
a2006 <- cbind(ut_stat_info, WESD)
a2007 <- cbind(ut_stat_info, WESD)
a2008 <- cbind(ut_stat_info, WESD)
a2009 <- cbind(ut_stat_info, WESD)
a2010 <- cbind(ut_stat_info, WESD)
a2011 <- cbind(ut_stat_info, WESD)
a2012 <- cbind(ut_stat_info, WESD)
a2013 <- cbind(ut_stat_info, WESD)
a2014 <- cbind(ut_stat_info, WESD)
a2015 <- cbind(ut_stat_info, WESD)
a2016 <- cbind(ut_stat_info, WESD)
#a2017 <- cbind(ut_stat_info, WESD)
a2018 <- cbind(ut_stat_info, WESD)
a2019 <- cbind(ut_stat_info, WESD)
a2020 <- cbind(ut_stat_info, WESD)
a2021 <- cbind(ut_stat_info, WESD)
 

head(ut_stat_info)
#any(t2 != t3)
#a2004[t, 2] <- april_2004$VALUE[1]
################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################

for (i in 1:length(april_2004$ID)){
  t <- april_2004$ID[i] == utah_STATIONS
  a2004[t, "WESD"] <- april_2004$VALUE[i]
}

for (i in 1:length(april_2005$ID)){
  t <- april_2005$ID[i] == utah_STATIONS
  a2005[t, "WESD"] <- april_2005$VALUE[i]
}

for (i in 1:length(april_2006$ID)){
  t <- april_2006$ID[i] == utah_STATIONS
  a2006[t, "WESD"] <- april_2006$VALUE[i]
}

for (i in 1:length(april_2007$ID)){
  t <- april_2007$ID[i] == utah_STATIONS
  a2007[t, "WESD"] <- april_2007$VALUE[i]
}

for (i in 1:length(april_2008$ID)){
  t <- april_2008$ID[i] == utah_STATIONS
  a2008[t, "WESD"] <- april_2008$VALUE[i]
}

for (i in 1:length(april_2009$ID)){
  t <- april_2009$ID[i] == utah_STATIONS
  a2009[t, "WESD"] <- april_2009$VALUE[i]
}

for (i in 1:length(april_2010$ID)){
  t <- april_2010$ID[i] == utah_STATIONS
  a2010[t, "WESD"] <- april_2010$VALUE[i]
}

for (i in 1:length(april_2011$ID)){
  t <- april_2011$ID[i] == utah_STATIONS
  a2011[t, "WESD"] <- april_2011$VALUE[i]
}

for (i in 1:length(april_2012$ID)){
  t <- april_2012$ID[i] == utah_STATIONS
  a2012[t, "WESD"] <- april_2012$VALUE[i]
}

for (i in 1:length(april_2013$ID)){
  t <- april_2013$ID[i] == utah_STATIONS
  a2013[t, "WESD"] <- april_2013$VALUE[i]
}

for (i in 1:length(april_2014$ID)){
  t <- april_2014$ID[i] == utah_STATIONS
  a2014[t, "WESD"] <- april_2014$VALUE[i]
}

for (i in 1:length(april_2015$ID)){
  t <- april_2015$ID[i] == utah_STATIONS
  a2015[t, "WESD"] <- april_2015$VALUE[i]
}

for (i in 1:length(april_2016$ID)){
  t <- april_2016$ID[i] == utah_STATIONS
  a2016[t, "WESD"] <- april_2016$VALUE[i]
}

#for (i in 1:length(april_2017$ID)){
#  t <- april_2017$ID[i] == utah_STATIONS
#  a2017[t, "WESD"] <- april_2017$VALUE[i]
#}

for (i in 1:length(april_2018$ID)){
  t <- april_2018$ID[i] == utah_STATIONS
  a2018[t, "WESD"] <- april_2018$VALUE[i]
}

for (i in 1:length(april_2019$ID)){
  t <- april_2019$ID[i] == utah_STATIONS
  a2019[t, "WESD"] <- april_2019$VALUE[i]
}

for (i in 1:length(april_2020$ID)){
  t <- april_2020$ID[i] == utah_STATIONS
  a2020[t, "WESD"] <- april_2020$VALUE[i]
}

for (i in 1:length(april_2021$ID)){
  t <- april_2021$ID[i] == utah_STATIONS
  a2021[t, "WESD"] <- april_2021$VALUE[i]
}

################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
### Save each as a data frame
# a2004 <- as.data.frame(a2004)
# a2005 <- as.data.frame(a2005)
# a2006 <- as.data.frame(a2006)
# a2007 <- as.data.frame(a2007)
# a2008 <- as.data.frame(a2008)
# a2009 <- as.data.frame(a2009)
# a2010 <- as.data.frame(a2010)
# a2011 <- as.data.frame(a2011)
# a2012 <- as.data.frame(a2012)
# a2013 <- as.data.frame(a2013)
# a2014 <- as.data.frame(a2014)
# a2015 <- as.data.frame(a2015)
# a2016 <- as.data.frame(a2016)
# a2017 <- as.data.frame(a2017)
# a2018 <- as.data.frame(a2018)
# a2019 <- as.data.frame(a2019)
# a2020 <- as.data.frame(a2020)
# a2021 <- as.data.frame(a2021)

a2020

# Adding a new column for each WESD for the year
station_info$april_2004_WESD <- a2004$WESD
station_info$april_2005_WESD <- a2005$WESD
station_info$april_2006_WESD <- a2006$WESD
station_info$april_2007_WESD <- a2007$WESD
station_info$april_2008_WESD <- a2008$WESD
station_info$april_2009_WESD <- a2009$WESD
station_info$april_2010_WESD <- a2010$WESD
station_info$april_2011_WESD <- a2011$WESD
station_info$april_2012_WESD <- a2012$WESD
station_info$april_2013_WESD <- a2013$WESD
station_info$april_2014_WESD <- a2014$WESD
station_info$april_2015_WESD <- a2015$WESD
station_info$april_2016_WESD <- a2016$WESD
#station_info$april_2017_WESD <- a2017$WESD
station_info$april_2018_WESD <- a2018$WESD
station_info$april_2019_WESD <- a2019$WESD
station_info$april_2020_WESD <- a2020$WESD
station_info$april_2021_WESD <- a2021$WESD

################################################################################

utah2 #ghcnd_stations

# station_info was up above but it takes the ghcnd_stations that are in utah and 
# returns the "ID", "LATITUDE", "LONGITUDE","ELEVATION", "STATE", and "NAME".
station_info_copy <- station_info
station_info_copy2 <- station_info_copy

# Changes this into a spatial object that 
#coordinates(station_info) <- c("LONGITUDE", "LATITUDE")

# Sp is used to create a spatial object and 
# retrieve spatial coordinates from Spatial object.
# the proj4string is turning it into the correct projection used in the maps
tcrs <- raster::crs(map[[1]])
sp::coordinates(station_info) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(station_info) <- tcrs


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################

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



class(station_info_copy$april_2004_WESD)

test <- (station_info_copy$april_2004_WESD == 0 & station_info_copy$april_2005_WESD == 0 &
  station_info_copy$april_2006_WESD == 0 & station_info_copy$april_2012_WESD == 0 &
  station_info_copy$april_2007_WESD == 0 & station_info_copy$april_2013_WESD == 0 &
  station_info_copy$april_2008_WESD == 0 & station_info_copy$april_2014_WESD == 0 &
  station_info_copy$april_2009_WESD == 0 & station_info_copy$april_2015_WESD == 0 &
  station_info_copy$april_2010_WESD == 0 & station_info_copy$april_2016_WESD == 0 &
  station_info_copy$april_2011_WESD == 0 & #station_info_copy$april_2017_WESD == 0 &
  station_info_copy$april_2020_WESD == 0 & station_info_copy$april_2018_WESD == 0 &
  station_info_copy$april_2021_WESD == 0 & station_info_copy$april_2019_WESD == 0)

stations_1ob <- station_info_copy[!test, ]

any(test == FALSE)


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
#test_values[[1]]
a2004$MODEL_WESD <- test_values[[1]]
a2005$MODEL_WESD <- test_values[[2]]
a2006$MODEL_WESD <- test_values[[3]]
a2007$MODEL_WESD <- test_values[[4]]
a2008$MODEL_WESD <- test_values[[5]]
a2009$MODEL_WESD <- test_values[[6]]
a2010$MODEL_WESD <- test_values[[7]]
a2011$MODEL_WESD <- test_values[[8]]
a2012$MODEL_WESD <- test_values[[9]]
a2013$MODEL_WESD <- test_values[[10]]
a2014$MODEL_WESD <- test_values[[11]]
a2015$MODEL_WESD <- test_values[[12]]
a2016$MODEL_WESD <- test_values[[13]]
#a2017$MODEL_WESD <- test_values[[18]]
a2018$MODEL_WESD <- test_values[[14]]
a2019$MODEL_WESD <- test_values[[15]]
a2020$MODEL_WESD <- test_values[[16]]
a2021$MODEL_WESD <- test_values[[17]]

################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
# stations_2ob$april_2004_WESD <- (stations_2ob$april_2004_WESD / 10)
# stations_2ob$april_2005_WESD <- (stations_2ob$april_2005_WESD / 10)
# stations_2ob$april_2006_WESD <- (stations_2ob$april_2006_WESD / 10)
# stations_2ob$april_2007_WESD <- (stations_2ob$april_2007_WESD / 10)
# stations_2ob$april_2008_WESD <- (stations_2ob$april_2008_WESD / 10)
# stations_2ob$april_2009_WESD <- (stations_2ob$april_2009_WESD / 10)
# stations_2ob$april_2010_WESD <- (stations_2ob$april_2010_WESD / 10)
# stations_2ob$april_2011_WESD <- (stations_2ob$april_2011_WESD / 10)
# stations_2ob$april_2012_WESD <- (stations_2ob$april_2012_WESD / 10)
# stations_2ob$april_2013_WESD <- (stations_2ob$april_2013_WESD / 10)
# stations_2ob$april_2014_WESD <- (stations_2ob$april_2014_WESD / 10)
# stations_2ob$april_2015_WESD <- (stations_2ob$april_2015_WESD / 10)
# stations_2ob$april_2016_WESD <- (stations_2ob$april_2016_WESD / 10)
# #stations_2ob$april_2017_WESD <- (stations_2ob$april_2017_WESD / 10)
# stations_2ob$april_2018_WESD <- (stations_2ob$april_2018_WESD / 10)
# stations_2ob$april_2019_WESD <- (stations_2ob$april_2019_WESD / 10)
# stations_2ob$april_2020_WESD <- (stations_2ob$april_2020_WESD / 10)
# stations_2ob$april_2021_WESD <- (stations_2ob$april_2021_WESD / 10)


a2004$DIFFERENCE <- (a2004$WESD/10) - a2004$MODEL_WESD
a2005$DIFFERENCE <- (a2005$WESD/10) - a2005$MODEL_WESD
a2006$DIFFERENCE <- (a2006$WESD/10) - a2006$MODEL_WESD
a2007$DIFFERENCE <- (a2007$WESD/10) - a2007$MODEL_WESD
a2008$DIFFERENCE <- (a2008$WESD/10) - a2008$MODEL_WESD
a2009$DIFFERENCE <- (a2009$WESD/10) - a2009$MODEL_WESD
a2010$DIFFERENCE <- (a2010$WESD/10) - a2010$MODEL_WESD
a2011$DIFFERENCE <- (a2011$WESD/10) - a2011$MODEL_WESD
a2012$DIFFERENCE <- (a2012$WESD/10) - a2012$MODEL_WESD
a2013$DIFFERENCE <- (a2013$WESD/10) - a2013$MODEL_WESD 
a2014$DIFFERENCE <- (a2014$WESD/10) - a2014$MODEL_WESD
a2015$DIFFERENCE <- (a2015$WESD/10) - a2015$MODEL_WESD
a2016$DIFFERENCE <- (a2016$WESD/10) - a2016$MODEL_WESD
#a2017$DIFFERENCE <- a2017$WESD - a2017$MODEL_WESD
a2018$DIFFERENCE <- (a2018$WESD/10) - a2018$MODEL_WESD
a2019$DIFFERENCE <- (a2019$WESD/10) - a2019$MODEL_WESD
a2020$DIFFERENCE <- (a2020$WESD/10) - a2020$MODEL_WESD
a2021$DIFFERENCE <- (a2021$WESD/10) - a2021$MODEL_WESD 
#pivotlonger in tidyr package

plot(a2004$ELEVATION, a2004$DIFFERENCE)
plot(a2005$ELEVATION, a2005$DIFFERENCE)
plot(a2006$ELEVATION, a2006$DIFFERENCE)
plot(a2007$ELEVATION, a2007$DIFFERENCE)
plot(a2008$ELEVATION, a2008$DIFFERENCE)
plot(a2009$ELEVATION, a2009$DIFFERENCE)
plot(a2010$ELEVATION, a2010$DIFFERENCE)
plot(a2011$ELEVATION, a2011$DIFFERENCE)
plot(a2012$ELEVATION, a2012$DIFFERENCE)
plot(a2013$ELEVATION, a2013$DIFFERENCE)
plot(a2014$ELEVATION, a2014$DIFFERENCE)
plot(a2015$ELEVATION, a2015$DIFFERENCE)
plot(a2016$ELEVATION, a2016$DIFFERENCE)
#plot(a2017$ELEVATION, a2017$DIFFERENCE)
plot(a2018$ELEVATION, a2018$DIFFERENCE)
plot(a2019$ELEVATION, a2019$DIFFERENCE)
plot(a2020$ELEVATION, a2020$DIFFERENCE)
plot(a2021$ELEVATION, a2021$DIFFERENCE)

?scatterplot()


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
station_info_copy$april_2004_MODEL <- a2004$MODEL_WESD
station_info_copy$april_2005_MODEL <- a2005$MODEL_WESD
station_info_copy$april_2006_MODEL <- a2006$MODEL_WESD
station_info_copy$april_2007_MODEL <- a2007$MODEL_WESD
station_info_copy$april_2008_MODEL <- a2008$MODEL_WESD
station_info_copy$april_2009_MODEL <- a2009$MODEL_WESD
station_info_copy$april_2010_MODEL <- a2010$MODEL_WESD
station_info_copy$april_2011_MODEL <- a2011$MODEL_WESD
station_info_copy$april_2012_MODEL <- a2012$MODEL_WESD
station_info_copy$april_2013_MODEL <- a2013$MODEL_WESD
station_info_copy$april_2014_MODEL <- a2014$MODEL_WESD
station_info_copy$april_2015_MODEL <- a2015$MODEL_WESD
station_info_copy$april_2016_MODEL <- a2016$MODEL_WESD
#station_info_copy$april_2017_MODEL <- a2017$WESD
station_info_copy$april_2018_MODEL <- a2018$MODEL_WESD
station_info_copy$april_2019_MODEL <- a2019$MODEL_WESD
station_info_copy$april_2020_MODEL <- a2020$MODEL_WESD
station_info_copy$april_2021_MODEL <- a2021$MODEL_WESD

station_info$april_2004_MODEL <- a2004$MODEL_WESD
station_info$april_2005_MODEL <- a2005$MODEL_WESD
station_info$april_2006_MODEL <- a2006$MODEL_WESD
station_info$april_2007_MODEL <- a2007$MODEL_WESD
station_info$april_2008_MODEL <- a2008$MODEL_WESD
station_info$april_2009_MODEL <- a2009$MODEL_WESD
station_info$april_2010_MODEL <- a2010$MODEL_WESD
station_info$april_2011_MODEL <- a2011$MODEL_WESD
station_info$april_2012_MODEL <- a2012$MODEL_WESD
station_info$april_2013_MODEL <- a2013$MODEL_WESD
station_info$april_2014_MODEL <- a2014$MODEL_WESD
station_info$april_2015_MODEL <- a2015$MODEL_WESD
station_info$april_2016_MODEL <- a2016$MODEL_WESD
#station_info_copy$april_2017_MODEL <- a2017$WESD
station_info$april_2018_MODEL <- a2018$MODEL_WESD
station_info$april_2019_MODEL <- a2019$MODEL_WESD
station_info$april_2020_MODEL <- a2020$MODEL_WESD
station_info$april_2021_MODEL <- a2021$MODEL_WESD

#
station_info_copy$april_2004_DIFFERENCE <- a2004$DIFFERENCE
station_info_copy$april_2005_DIFFERENCE <- a2005$DIFFERENCE
station_info_copy$april_2006_DIFFERENCE <- a2006$DIFFERENCE
station_info_copy$april_2007_DIFFERENCE <- a2007$DIFFERENCE
station_info_copy$april_2008_DIFFERENCE <- a2008$DIFFERENCE
station_info_copy$april_2009_DIFFERENCE <- a2009$DIFFERENCE
station_info_copy$april_2010_DIFFERENCE <- a2010$DIFFERENCE
station_info_copy$april_2011_DIFFERENCE <- a2011$DIFFERENCE
station_info_copy$april_2012_DIFFERENCE <- a2012$DIFFERENCE
station_info_copy$april_2013_DIFFERENCE <- a2013$DIFFERENCE
station_info_copy$april_2014_DIFFERENCE <- a2014$DIFFERENCE
station_info_copy$april_2015_DIFFERENCE <- a2015$DIFFERENCE
station_info_copy$april_2016_DIFFERENCE <- a2016$DIFFERENCE
#station_info_copy$april_2017_DIFFERENCE <- a2017$DIFFERENCE
station_info_copy$april_2018_DIFFERENCE <- a2018$DIFFERENCE
station_info_copy$april_2019_DIFFERENCE <- a2019$DIFFERENCE
station_info_copy$april_2020_DIFFERENCE <- a2020$DIFFERENCE
station_info_copy$april_2021_DIFFERENCE <- a2021$DIFFERENCE

station_info$april_2004_DIFFERENCE <- a2004$DIFFERENCE
station_info$april_2005_DIFFERENCE <- a2005$DIFFERENCE
station_info$april_2006_DIFFERENCE <- a2006$DIFFERENCE
station_info$april_2007_DIFFERENCE <- a2007$DIFFERENCE
station_info$april_2008_DIFFERENCE <- a2008$DIFFERENCE
station_info$april_2009_DIFFERENCE <- a2009$DIFFERENCE
station_info$april_2010_DIFFERENCE <- a2010$DIFFERENCE
station_info$april_2011_DIFFERENCE <- a2011$DIFFERENCE
station_info$april_2012_DIFFERENCE <- a2012$DIFFERENCE
station_info$april_2013_DIFFERENCE <- a2013$DIFFERENCE
station_info$april_2014_DIFFERENCE <- a2014$DIFFERENCE
station_info$april_2015_DIFFERENCE <- a2015$DIFFERENCE
station_info$april_2016_DIFFERENCE <- a2016$DIFFERENCE
#station_info_copy$april_2017_DIFFERENCE <- a2017$DIFFERENCE
station_info$april_2018_DIFFERENCE <- a2018$DIFFERENCE
station_info$april_2019_DIFFERENCE <- a2019$DIFFERENCE
station_info$april_2020_DIFFERENCE <- a2020$DIFFERENCE
station_info$april_2021_DIFFERENCE <- a2021$DIFFERENCE


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
station_info_copy$RECORDS <- 0


a <- c(april_2004$ID, april_2005$ID,
       april_2006$ID, april_2007$ID,
       april_2008$ID, april_2009$ID,
       april_2010$ID, april_2011$ID,
       april_2012$ID, april_2013$ID,
       april_2014$ID, april_2015$ID,
       april_2016$ID, #april_2017$ID
       april_2018$ID, april_2019$ID,
       april_2020$ID, april_2021$ID)
b <- unique(a)
length(b) # 155 

# t <- b[1] == utah_STATIONS
# station_info_copy[t,"RECORDS"] <- 1

for (i in 1:length(b)){
  t <- b[i] == utah_STATIONS
  station_info_copy[t,"RECORDS"] <- 1
}

station_info_copy$RECORDS
################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
# test2 <- (station_info_copy$april_2004_WESD == 0 & station_info_copy$april_2005_WESD == 0 &
#             station_info_copy$april_2006_WESD == 0 & station_info_copy$april_2012_WESD == 0 &
#             station_info_copy$april_2007_WESD == 0 & station_info_copy$april_2013_WESD == 0 &
#             station_info_copy$april_2008_WESD == 0 & station_info_copy$april_2014_WESD == 0 &
#             station_info_copy$april_2009_WESD == 0 & station_info_copy$april_2015_WESD == 0 &
#             station_info_copy$april_2010_WESD == 0 & station_info_copy$april_2016_WESD == 0 &
#             station_info_copy$april_2011_WESD == 0 & #station_info_copy$april_2017_WESD == 0 &
#             station_info_copy$april_2020_WESD == 0 & station_info_copy$april_2018_WESD == 0 &
#             station_info_copy$april_2021_WESD == 0 & station_info_copy$april_2019_WESD == 0)
# 
# stations_2ob <- station_info_copy[!test2, ]

stations_2ob <- station_info_copy[station_info_copy$RECORDS == 1, ]
#station_info_copy$RECORDS == 1

################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################

# diff04 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2004_DIFFERENCE)
# diff05 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2005_DIFFERENCE)
# diff06 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2006_DIFFERENCE)
# diff07 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2007_DIFFERENCE)
# diff08 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2008_DIFFERENCE)
# diff09 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2009_DIFFERENCE)
# diff10 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2010_DIFFERENCE)
# diff11 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2011_DIFFERENCE)
# diff12 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2012_DIFFERENCE)
# diff13 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2013_DIFFERENCE)
# diff14 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2014_DIFFERENCE)
# diff15 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2015_DIFFERENCE)
# diff16 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2016_DIFFERENCE)
# diff17 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2017_DIFFERENCE)
# diff18 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2018_DIFFERENCE)
# diff19 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2019_DIFFERENCE)
# diff20 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2020_DIFFERENCE)
# diff21 <- plot(stations_2ob$ELEVATION, stations_2ob$april_2021_DIFFERENCE)

library(ggplot2)

# i = 1
# for (i in 1:length(years)){
#   v <- paste0("april_", i + 2003, "_DIFFERENCE")
#   print(v)
#   plot(stations_2ob$ELEVATION, stations_2ob$)
# }

library(ggplot2)
#ggplot2::
ggplot(stations_2ob, aes(x = ELEVATION, y = april_2004_DIFFERENCE)) + 
  geom_point()


diff04 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2004_DIFFERENCE)) + geom_point()
diff05 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2005_DIFFERENCE)) + geom_point()
diff06 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2006_DIFFERENCE)) + geom_point()
diff07 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2007_DIFFERENCE)) + geom_point()
diff08 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2008_DIFFERENCE)) + geom_point()
diff09 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2009_DIFFERENCE)) + geom_point()
diff10 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2010_DIFFERENCE)) + geom_point()
diff11 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2011_DIFFERENCE)) + geom_point()
diff12 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2012_DIFFERENCE)) + geom_point()
diff13 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2013_DIFFERENCE)) + geom_point()
diff14 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2014_DIFFERENCE)) + geom_point()
diff15 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2015_DIFFERENCE)) + geom_point()
diff16 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2016_DIFFERENCE)) + geom_point()
#diff17 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2017_DIFFERENCE)) + geom_point()
diff18 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2018_DIFFERENCE)) + geom_point()
diff19 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2019_DIFFERENCE)) + geom_point()
diff20 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2020_DIFFERENCE)) + geom_point()
diff21 <- ggplot(stations_2ob, aes(x = ELEVATION, y = april_2021_DIFFERENCE)) + geom_point()


station_info_copy

all_diff04 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2004_DIFFERENCE)) + geom_point()
all_diff05 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2005_DIFFERENCE)) + geom_point()
all_diff06 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2006_DIFFERENCE)) + geom_point()
all_diff07 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2007_DIFFERENCE)) + geom_point()
all_diff08 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2008_DIFFERENCE)) + geom_point()
all_diff09 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2009_DIFFERENCE)) + geom_point()
all_diff10 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2010_DIFFERENCE)) + geom_point()
all_diff11 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2011_DIFFERENCE)) + geom_point()
all_diff12 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2012_DIFFERENCE)) + geom_point()
all_diff13 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2013_DIFFERENCE)) + geom_point()
all_diff14 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2014_DIFFERENCE)) + geom_point()
all_diff15 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2015_DIFFERENCE)) + geom_point()
all_diff16 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2016_DIFFERENCE)) + geom_point()
#all_diff17 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2017_DIFFERENCE)) + geom_point()
all_diff18 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2018_DIFFERENCE)) + geom_point()
all_diff19 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2019_DIFFERENCE)) + geom_point()
all_diff20 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2020_DIFFERENCE)) + geom_point()
all_diff21 <- ggplot(station_info_copy, aes(x = ELEVATION, y = april_2021_DIFFERENCE)) + geom_point()


colnames(station_info_copy)
diff04





