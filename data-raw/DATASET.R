## code to prepare `april_station_info` dataset goes here

# Note that in order to preform the following code, you must have already
#   downloaded the Snotel Station data.

# CHECKPOINTS:
#  1) download_snodas_data
#  2) format_dates
#  3) with data-raw
#  4)
#  5)
#  6)


#  1)
#  2)
################################################################################
###### Making a Long Data Frame of the VAriables of GHCND    #########
################################################################################

# load the GHCND Station DATA, Put this in the Data
load("C:/Users/Logan/Desktop/improved/data/ghcnd_stations.rda")

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

get_state_data <- function(states, source,
                           elem = c("SNWD", "WESD"),
                           progress = TRUE) {
  stations <- ghcnd_stations$ID[ghcnd_stations$STATE %in% states]
  get_station_data(stations, source, elem, progress)
}


################################################################################
# Saving the data with the WESD
# "~/Desktop/Graduate classes 2021 Spring/Research/snowload2-master/data/ghcnd_stations.rda"
################################################################################
# Use the function get_state_data from the ghcnd_add File and get "WESD".
# *Note that Utah2 has each
utah2 <- get_state_data("UT", source ="C:/Users/Logan/Desktop/ghcnd_all/",
                        elem = "WESD")

# Change the units into mm
utah2$VALUE <- (utah2$VALUE / 10)

# save the ghcnd_station_info of lat, lon into the utah2 data frame.
utah2[, "ELEVATION"] <- 0
utah2[, 'LONGITUDE'] <- 0
utah2[, "LATITUDE"] <- 0
utah2[, "NAME"] <- " "
utah2$annual_max_temp <- 0
utah2$annual_max_vapor <- 0
utah2$annual_mean_dew <- 0
utah2$annual_mean_temp <- 0
utah2$annual_min_temp <- 0
utah2$annual_min_vapor <- 0
utah2$annual_precip <- 0
utah2$slope <- 0
utah2$aspect <- 0

# Make ghcnd_station_info an sf object.
ghcnd_station_sf <- sf::st_as_sf(ghcnd_station_info,
                                 coords = c("LONGITUDE", "LATITUDE"),
                                 crs = sf::st_crs(4326))

# Read in PRISM
mon <- c('annual')
v <- c("annual")

info <- c('max temp',
          "maximum vapor pressure deficit",
          "mean dew point temperature",
          "mean temp",
          "min temp",
          "minimum vapor pressure deficit",
          "precipitation",
          "elevation")
info_names <- c('max_temp', "max_vp", "mean_dew_temp", "mean_temp",
                "min_temp", "min_vp", "precip","ext_elevat")

file_type <- c("tmax_30yr_normal_800mM2_",
               "vpdmax_30yr_normal_800mM2_",
               "tdmean_30yr_normal_800mM2_",
               "tmean_30yr_normal_800mM2_",
               "tmin_30yr_normal_800mM2_",
               "vpdmin_30yr_normal_800mM2_",
               "ppt_30yr_normal_800mM2_",
               "us_dem_800m_")

for (i in 1:8) {
  if (i != 8) {
    r <- stars::read_stars(paste0("C:/Users/Logan/Desktop/PRISM/annual",
                                  "/monthly/", info[i], "/PRISM_",
                                  file_type[i], "annual_asc.asc"))
  } else {
    # This is for Elevation because it is different
    r <- stars::read_stars(paste0("C:/Users/Logan/Desktop/PRISM/annual",
                                   "/monthly/", info[i], "/PRISM_",
                                   file_type[i], "asc.asc"))
  }

  # Reproject the stars object to have the same CRS
  r <- stars::st_warp(r, crs = sf::st_crs(4326))

  # Extract the Values from the Raster and store in the ghcnd data frame
  ghcnd_station_info[, paste0(info_names[i])] <- stars::st_extract(r,
                                                                   ghcnd_station_sf)
}

# Extract the slope and aspect. r
# ele_terra <- terra::rast(paste0("C:/Users/Logan/Desktop/PRISM/annual",
#                                 "/monthly/", info[i], "/PRISM_",
#                                 file_type[i], "asc.asc"))

ele_terra2 <- terra::rast(r)

slope_terra <- terra::terrain(ele_terra2, neighbors = 8,
                              v = "slope", unit = "degrees")
aspect_terra <- terra::terrain(ele_terra2, neighbors = 8,
                               v = "aspect", unit = "degrees")


ghcnd_station_terra <- terra::vect(ghcnd_station_sf)
ghcnd_station_info[, "slope"] <- terra::extract(slope_terra,
                                                ghcnd_station_terra)[, 2]
ghcnd_station_info[, "aspect"] <- terra::extract(aspect_terra,
                                                 ghcnd_station_terra)[, 2]


# Get just april 1st snotel data
april_1_snotel_data <- utah2[(utah2$DATE == "1982-04-01"), ]
year <- 1982:2021# length(year)
for (i in 2:length(year)) {
 april_1_snotel_data <- rbind(april_1_snotel_data,
                             utah2[(utah2$DATE == paste0(year[i], "-04-01")), ])
}

# Extract the following information: elev, lat, lon, name, and prism variables.
for (i in 1:length(april_1_snotel_data$ID)) {
  st_info <- april_1_snotel_data$ID[i] == ghcnd_station_info$ID
  april_1_snotel_data[, "ELEVATION"][i] <- ghcnd_station_info[st_info, "ELEVATION"]
  april_1_snotel_data[, 'LONGITUDE'][i] <- ghcnd_station_info[st_info, "LONGITUDE"]
  april_1_snotel_data[, "LATITUDE"][i] <- ghcnd_station_info[st_info, "LATITUDE"]
  april_1_snotel_data[, "NAME"][i] <- ghcnd_station_info[st_info, "NAME"]
  april_1_snotel_data$annual_max_temp[i] <- ghcnd_station_info[st_info, "max_temp"]
  april_1_snotel_data$annual_max_vapor[i] <- ghcnd_station_info[st_info, "max_vp"]
  april_1_snotel_data$annual_mean_dew[i] <- ghcnd_station_info[st_info, "mean_dew_temp"]
  april_1_snotel_data$annual_mean_temp[i] <- ghcnd_station_info[st_info, "mean_temp"]
  april_1_snotel_data$annual_min_temp[i] <- ghcnd_station_info[st_info, "min_temp"]
  april_1_snotel_data$annual_min_vapor[i] <- ghcnd_station_info[st_info, "min_vp"]
  april_1_snotel_data$annual_precip[i] <- ghcnd_station_info[st_info, "precip"]
  april_1_snotel_data$slope[i] <- ghcnd_station_info[st_info, "slope"]
  april_1_snotel_data$aspect[i] <- ghcnd_station_info[st_info, "aspect"]
}

# save the april 1st snotel data
usethis::use_data(april_1_snotel_data, overwrite = TRUE)


# Making UA maps data tif object
year <- 1982:2020
base_file <- "data-raw/ua_data/4km_SWE_Depth_WY"
end_file <- "_v01.nc"

ua_us_maps <- vector("list", length(year))
"%>%" <- magrittr::"%>%"

ut_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = sf::st_crs(4326))

# ut_map2 <- sf::st_read("C:/Users/Logan/Desktop/utah_shp 2/Utah.shp")
# ut_map2 <- sf::st_transform(ut_map2["2", "STATE"],
#                             crs = sf::st_crs(4326))

# get the university maps for each year, can crop to a different state...
for (i in 1:length(year)) {
  # read in the file
  # ua_star <- stars::read_ncdf(paste0(base_file, year[i], end_file))
  ua_map <- terra::rast(paste0(base_file, year[i], end_file))
  # b <- raster::brick(paste0(base_file, year[i], end_file))

  # Find the layer with april 1st by checking it it is a leap year
  april_1 <- ifelse(terra::nlyr(ua_map) == 730, 183, 184)
  ua_map <- terra::subset(ua_map, april_1)

  # write a tif object. note this will be deleted
  #   for some reason the terra won't let me change to the correct crs
  # terra::project(ua_terra, sf::st_crs(4326))
  # terra::project(ua_terra, ("+proj=utm +zone=10 +datum=WGS84"))
  # terra::project(ua_terra, "+proj=longlat +datum=WGS84 +no_defs")
  # stars::st_as_stars(ua_terra)
  terra::writeRaster(ua_map, "data-raw/swe.tif", overwrite=TRUE)
  ua_map <- stars::read_stars("data-raw/swe.tif")

  ua_map <- stars::st_warp(ua_map, crs = sf::st_crs(4326))

  # crop to the state of utah
  ua_ut <- sf::st_crop(ua_map, ut_map)

  # store in a  list
  ua_us_maps[[i]] <- ua_ut # ua_map
}

# go through the list of maps and combine to a stars stack
for (i in 2:length(ua_us_maps)) {
  # Combine/Concatenate the first 2 maps
  if (i == 2) {
    combined_ua <- c(ua_us_maps[[(i - 1)]], ua_us_maps[[i]])
  } else {
    # Combine the later maps
    combined_ua <- c(combined_ua, ua_us_maps[[i]])
  }
}

# Rename each object to contain the dates by creating a vector of SWE_MM_DD_YYYY
a <- vector("character", length(combined_ua))
for (i in 1:length(combined_ua)) {
  a[i] <- paste0("SWE_04_01_", year[i])
}

# Renaming and combine
names(combined_ua) <- a
combined_ua_us <- merge(combined_ua)
combined_ua_us <- stars::st_set_dimensions(combined_ua_us,
                                           names = c("x", "y", "time"))

# Write as an object
# stars::write_stars(combined_ua_us, "data/ua_april1.tif")
stars::write_stars(combined_ua_us, "data-raw/ua_ut_april1.tif")
# usethis::use_data(utah2, overwrite = TRUE)
