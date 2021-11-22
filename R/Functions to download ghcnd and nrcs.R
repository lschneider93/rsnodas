# functions for research
# function for downloading ghcnd_station_data
download_ghcnd_station_data <- function(){
  # load("~/Desktop/Graduate Studies/Research/snowload2-master/data/ghcnd_stations.rda")
  # Note this is all ghcnd_stations
  load("~/Desktop/GitHub/rsnodas/data/ghcnd_stations.rda")
  # years <- c(2004:2016, 2018:2021) # is this needed?  year is the exact same

  # Vector of Utah Stations
  utah_STATIONS <- ghcnd_stations$ID[ghcnd_stations$STATE %in% "UT"]

  # make a data frame of just the Utah stations.
  # this is done by starting the ghcnd_station_info with matching the ID of the first
  # Utah station and matching it in all the stations.  Retrive LAT, LONG, ELEV, NAME
  ghcnd_station_info <- data.frame(ghcnd_stations[ghcnd_stations$ID == utah_STATIONS[1], ])

  # Search through ghcnd_station_info with matching the ID of the 2:906 Utah
  # station ID and matching it in all the stations.  Retrive LAT, LONG, ELEV, NAME
  # appending a line for each station in Utah
  for (i in 2:length(utah_STATIONS)){
    ghcnd_station_info <- rbind(ghcnd_station_info,ghcnd_stations[ghcnd_stations$ID == utah_STATIONS[i], ] )
  }

  # Keep a subset of the columns. ( ID, LAT, LONG, ELEV, STATE, and NAME...)
  ghcnd_station_info <- ghcnd_station_info[, c("ID", "LATITUDE", "LONGITUDE",
                                               "ELEVATION", "STATE", "NAME")]
  # ut_stat_info <- ghcnd_station_info
  return(ghcnd_station_info)
}

# ghcnd_station_info <- download_ghcnd_station_data()
# ghcnd_station_info <- download_ghcnd_station_data()





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

#get_state_data()
load("~/Desktop/Graduate Studies/Research/snowload2-master/data/ghcnd_stations.rda")
# loads ghcnd_stations

# utah2 <- get_state_data("UT", source ="~/Desktop/Graduate Studies/Research/rsnodas/ghcnd_all/",
#                         elem = "WESD")
utah2 <- get_state_data("UT", source ="~/Desktop/GitHub/rsnodas/MISC/Research/rsnodas/Data/ghcnd_all/",
                        elem = "WESD")



















#### Downloading the NRCS station data information
download_nrcs_station_data <- function(){
  URL = "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/state=%22UT%22%20AND%20county=%22Beaver%22,%22Box%20Elder%22,%22Cache%22,%22Carbon%22,%22Daggett%22,%22Davis%22,%22Duchesne%22,%22Emery%22,%22Garfield%22,%22Grand%22,%22Iron%22,%22Juab%22,%22Kane%22,%22Millard%22,%22Morgan%22,%22Piute%22,%22Rich%22,%22Salt%20Lake%22,%22San%20Juan%22,%22Sanpete%22,%22Sevier%22,%22Summit%22,%22Tooele%22,%22Uintah%22,%22Utah%22,%22Wasatch%22,%22Washington%22,%22Wayne%22,%22Weber%22%7Cname/2021-04-01,2021-04-01:M%7C4,D%7C1/name,stationId,state.name,state.code,network.name,network.code,elevation,latitude,longitude?fitToScreen=false"

  # download the csv
  snowURL <- RCurl::getURL(URL)

  # text file of all the stations
  text_station_info <- unlist(strsplit(snowURL, '\n'))

  # get the first line of the stations
  first_line <- min(grep('^[[:alpha:]]', text_station_info))

  # this creates a csv
  snowCSV <- read.csv(textConnection(snowURL), skip = first_line - 1)

}


nrcs_csv <- download_nrcs_station_data()
dim(nrcs_csv)

# write.csv(nrcs_csv, "nrcs_station_data.csv")

# Website with the actionID
# https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/332:UT:SNTL%7C455:UT:SNTL%7Cstate=%22UT%22%7Cname/0,0/stationId,network.code,network.name,dco.code,actonId,shefId?fitToScreen=false
