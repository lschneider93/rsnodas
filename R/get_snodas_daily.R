#' Download Daily SNODAS Data maps
#'
#' @param dates date with YYYY-MM-dd written as a six digit character string
#' @param masked if TRUE, the maps will be from the masked
#'   if FALSE, the maps returned will be unmasked.
#'
#' @param data data is the type of maps wanted. There are 8 options:
#'   "SWE" for Snow-water-equivalent. "
#'   "SP" for Snow precipatation 24 hrs
#'   "SD" for Modeled Snow Depth
#'   "SPT" for Snowpack average temperature
#'   "BSS" for Modeled blowing snow sublimation rate
#'   "MELT" for Modeled melt rate
#'   "SPS" for Modeled snowpack sublimation rate
#'   "NSP" for Non-snow precipitation
#'
#' @param permanent permanent is the name of a permanent folder that will
#'   be created in the working directory. The User can input a different name
#'   for the permanent folder.
#'
#' @return a brick of rasters with elements corresponding to the
#'   input vaiable "data".
#'
#' @details This function creates a permanent folder and downloads data
#' from the SNODAS website for the inputed date(s), map type(s), and data map
#' that the user provides. This data is stored in the permanent folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list and combines them all into a RasterBrick.
#'
#' @export
get_snodas_daily <- function(dates = c("2010-01-01", "2012-4-15"),
                             masked = TRUE,
                             data = c('swe', 'SP', "SD", "SPT",
                                      'bss', 'melt', 'SPS', 'NSP'),
                             permanent = "permanent folder"){

  # Create a new folder called "temp". This will be deleted at the end
  dir.create("data")
  dir.create(permanent) # User can specify what this permanent folder is called

  # save old working directory file path
  oldwd <- getwd()

  ### How to separate the day, month, and year and put them in Vector Forms
  # First create blank vectors for date, month, month1, and year
  date <- numeric()
  month <- numeric()
  month1 <- numeric()
  year <- numeric()
  url <- numeric()

  date <- lubridate::day(dates)
  month <- lubridate::month(dates)
  year <- lubridate::year(dates)

  # Double-check to make sure the length of dates, months and years are the same
  #=============================================================================
  for (i in 1:length(dates)){
    if (nchar(date[i]) == 1) {
      date[i] = paste(rep(0, 1), date[i], sep = "")
    } else if (nchar(date[i]) == 2) {
      date[i] = date[i]
    } else stop("Error: day must be in the form of YYYY/MM/dd")

    if (nchar(month[i]) == 1) {
      month[i] = paste(rep(0, 1), month[i], sep = "")
    } else if (nchar(month[i]) == 2) {
      month[i] = month[i]
    } else stop("Error: Month must be in the form of YYYY/MM/dd")

    # Stop function and put in the conditions to get month1 in Character format.
    if((month[i] == "01")){
      month1[i] <- "Jan"
    } else if (month[i] == "02"){
      month1[i] = "Feb"
    } else if (month[i] == "03"){
      month1[i] = "Mar"
    } else if (month[i] == "04"){
      month1[i] = "Apr"
    } else if (month[i] == "05"){
      month1[i] = "May"
    } else if (month[i] == "06"){
      month1[i] = "Jun"
    } else if (month[i] == "07"){
      month1[i] = "Jul"
    } else if (month[i] == "08"){
      month1[i] = "Aug"
    } else if (month[i] == "09"){
      month1[i] = "Sep"
    } else if (month[i] == "10"){
      month1[i] = "Oct"
    } else if (month[i] == "11"){
      month1[i] = "Nov"
    } else if (month[i] == "12"){
      month1[i] = "Dec"
    }
    else stop("Error: Month must be in the form of 'MM'")

    # Changes "YY" to "20YY", and gives an error if user inputs anything different
    # than a 2 or 4 character input.
    if (nchar(year[i]) == 2) {
      year[i] = paste("20", year[i], sep = "")
    } else if (nchar(year[i]) == 4) {
      year[i] = year[i]
    } else stop("Error: Year must be in the form of YYYY/MM/dd")

    #=============================================================================

    # get the url from the day, month, and year
    if(masked == TRUE){
      url[i] <- paste("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/",
                      year[i], "/", month[i], "_", month1[i],
                      "/SNODAS_", year[i], month[i], date[i], ".tar", sep = '')
      prefix <- "us"
    } else if (masked == FALSE){
      url[i] <- paste("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/",
                      year[i], "/", month[i], "_", month1[i],
                      "/SNODAS_unmasked", "_", year[i], month[i], date[i], ".tar", sep = '')
      prefix <- "zz"
    }
    else stop("Error: Masked needs to be TRUE/FALSE")
  }

  # Download all of the SNODAS data files and put them in the data folder
  #=============================================================================
  for (i in 1:length(dates)){
    destfile <- paste(oldwd, "/data", "/urlfile", i, ".tar", sep = "")
    download.file(url[i], destfile = destfile)
    utils::untar(paste("data/urlfile", i, ".tar", sep = ""),
                 exdir = paste(oldwd, "/data", sep = ""))
  }

  # Note that each data has 2 files.  Note that this area can be simplified
  swe <- paste(prefix, "_ssmv11034tS__T0001TTNATS",
               year, month, date, "05HP001.txt", sep = '')
  swe1 <- paste(prefix, "_ssmv11034tS__T0001TTNATS",
                year, month, date, "05HP001.txt.gz", sep = '')
  swe2 <- paste(prefix, "_ssmv11034tS__T0001TTNATS",
                year, month, date, "05HP001.dat.gz", sep = '')
  swe3 <- paste(prefix, "_ssmv11034tS__T0001TTNATS",
                year, month, date, "05HP001.dat", sep = '')

  sp <- paste(prefix, "_ssmv01025SlL01T0024TTNATS",
              year, month, date, "05DP001.txt", sep = '')
  sp1 <- paste(prefix, "_ssmv01025SlL01T0024TTNATS",
               year, month, date, "05DP001.txt.gz", sep = '')
  sp2 <- paste(prefix, "_ssmv01025SlL01T0024TTNATS",
               year, month, date, "05DP001.dat.gz", sep = '')
  sp3 <- paste(prefix, "_ssmv01025SlL01T0024TTNATS",
               year, month, date, "05DP001.dat", sep = '')

  msd <- paste(prefix, "_ssmv11036tS__T0001TTNATS",
               year, month, date, "05HP001.txt", sep = '')
  msd1 <- paste(prefix, "_ssmv11036tS__T0001TTNATS",
                year, month, date, "05HP001.txt.gz", sep = '')
  msd2 <- paste(prefix, "_ssmv11036tS__T0001TTNATS",
                year, month, date, "05HP001.dat.gz", sep = '')
  msd3 <- paste(prefix, "_ssmv11036tS__T0001TTNATS",
                year, month, date, "05HP001.dat", sep = '')

  savetemp <- paste(prefix, "_ssmv11038wS__A0024TTNATS",
                    year, month, date, "05DP001.txt", sep = '')
  savetemp1 <- paste(prefix, "_ssmv11038wS__A0024TTNATS",
                     year, month, date, "05DP001.txt.gz", sep = '')
  savetemp2 <- paste(prefix, "_ssmv11038wS__A0024TTNATS",
                     year, month, date, "05DP001.dat.gz", sep = '')
  savetemp3 <- paste(prefix, "_ssmv11038wS__A0024TTNATS",
                     year, month, date, "05DP001.dat", sep = '')

  sublim <- paste(prefix, "_ssmv11039lL00T0024TTNATS",
                  year, month, date, "05DP000.txt", sep = '')
  sublim1 <- paste(prefix, "_ssmv11039lL00T0024TTNATS",
                   year, month, date, "05DP000.txt.gz", sep = '')
  sublim2 <- paste(prefix, "_ssmv11039lL00T0024TTNATS",
                   year, month, date, "05DP000.dat.gz", sep = '')
  sublim3 <- paste(prefix, "_ssmv11039lL00T0024TTNATS",
                   year, month, date, "05DP000.dat", sep = '')

  # There are two types of files for the melt rate.  They went from Hourly to
  # Daily in some years and other years that they just changed it.

  # THIS? us_ssmv11044bS__T0001TTNATS2010010105HP000.txt
  # THIS? us_ssmv11044bS__T0024TTNATS2011020105DP000.txt
  meltrate <- paste(prefix, "_ssmv11044bS__T0024TTNATS",
                    year, month, date, "05DP000.txt", sep = '')
  meltrate1 <- paste(prefix, "_ssmv11044bS__T0024TTNATS",
                     year, month, date, "05DP000.txt.gz", sep = '')
  meltrate2 <- paste(prefix, "_ssmv11044bS__T0024TTNATS",
                     year, month, date, "05DP000.dat.gz", sep = '')
  meltrate3 <- paste(prefix, "_ssmv11044bS__T0024TTNATS",
                     year, month, date, "05DP000.dat", sep = '')

  meltrate4 <- paste(prefix, "_ssmv11044bS__T0001TTNATS",
                     year, month, date, "05HP000.txt", sep = '')
  meltrate5 <- paste(prefix, "_ssmv11044bS__T0001TTNATS",
                     year, month, date, "05HP000.txt.gz", sep = '')
  meltrate6 <- paste(prefix, "_ssmv11044bS__T0001TTNATS",
                     year, month, date, "05HP000.dat.gz", sep = '')
  meltrate7 <- paste(prefix, "_ssmv11044bS__T0001TTNATS",
                     year, month, date, "05HP000.dat", sep = '')

  spsublim <- paste(prefix, "_ssmv11050lL00T0024TTNATS",
                    year, month, date, "05DP000.txt", sep = '')
  spsublim1 <- paste(prefix, "_ssmv11050lL00T0024TTNATS",
                     year, month, date, "05DP000.txt.gz", sep = '')
  spsublim2 <- paste(prefix, "_ssmv11050lL00T0024TTNATS",
                     year, month, date, "05DP000.dat.gz", sep = '')
  spsublim3 <- paste(prefix, "_ssmv11050lL00T0024TTNATS",
                     year, month, date, "05DP000.dat", sep = '')

  nsprecip <- paste(prefix, "_ssmv01025SlL00T0024TTNATS",
                    year, month, date, "05DP001.txt", sep = '')
  nsprecip1 <- paste(prefix, "_ssmv01025SlL00T0024TTNATS",
                     year, month, date, "05DP001.txt.gz", sep = '')
  nsprecip2 <- paste(prefix, "_ssmv01025SlL00T0024TTNATS",
                     year, month, date, "05DP001.dat.gz", sep = '')
  nsprecip3 <- paste(prefix, "_ssmv01025SlL00T0024TTNATS",
                     year, month, date, "05DP001.dat", sep = '')


  # Create a vector of lists with 8 * number of dates, one for each map.
  # The elements of the List that are empty will be deleted at the end.
  map <- vector("list", 8 * length(dates))

  for (i in 1:length(dates)){
    # if any of the Snow Water Equivalent
    if(any(data == "SWE") | any(data == "swe")){

      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", swe1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", swe[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         swe2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         swe3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 7]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               swe[i], sep = ""))
    }

    # Snow precipatation 24 hrs and put it in a list
    if(any(data == "SP") | any(data == "sp")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", sp1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", sp[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         sp2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         sp3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 6]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               sp[i], sep = ""))
    }

    # Modeled Snow Depth
    if(any(data == "sd") | any(data == "SD")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", msd1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", msd[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         msd2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         msd3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 5]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               msd[i], sep = ""))
    }

    # Snowpack average temperature
    if(any(data == "SPT") | any(data == "spt")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", savetemp1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", savetemp[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         savetemp2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         savetemp3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 4]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               savetemp[i], sep = ""))
    }

    # Modeled blowing snow sublimation rate.
    if(any(data == "bss") | any(data == "BSS")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", sublim1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", sublim[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         sublim2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         sublim3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 3]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               sublim[i], sep = ""))
    }

    # Modeled melt rate, 24-hour total
    if(any(data == "melt") | any(data == "Melt") | any(data == "MELT")){
      b <- try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", meltrate1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", meltrate[i], sep = ""),
        remove = TRUE), silent = TRUE)
      if(inherits(b, "try-error")){
        try(R.utils::gunzip(
          filename = paste(oldwd, "/data/", meltrate5[1], sep = ""),
          destname = paste(oldwd, "/", permanent, "/", meltrate4[i], sep = ""),
          remove = TRUE))
        try(R.utils::gunzip(
          filename = paste(oldwd, "/data/",
                           meltrate6[i], sep = ""),
          destname = paste(oldwd, "/", permanent, "/",
                           meltrate7[i], sep = ""),
          remove = TRUE))
        map[[(8*i) - 2]] <- raster::raster(paste(oldwd, "/",
                                                 permanent, "/",
                                                 meltrate4[i], sep = ""))
      }
      else {
        #R.utils::gunzip(
        #filename = paste(oldwd, "/data/", meltrate1[i], sep = ""),
        #destname = paste(oldwd, "/", permanent, "/", meltrate[i], sep = ""),
        #remove = TRUE)
        try(R.utils::gunzip(
          filename = paste(oldwd, "/data/",
                           meltrate2[i], sep = ""),
          destname = paste(oldwd, "/", permanent, "/",
                           meltrate3[i], sep = ""),
          remove = TRUE))
        map[[(8*i) - 2]] <- raster::raster(paste(oldwd, "/",
                                                 permanent, "/",
                                                 meltrate[i], sep = ""))
      }
    }

    # Modeled snowpack sublimation rate, 24-hour total
    if(any(data == "SPS") | any(data == "sps")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", spsublim1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", spsublim[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         spsublim2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         spsublim3[i], sep = ""),
        remove = TRUE))
      map[[(8*i) - 1]] <- raster::raster(paste(oldwd, "/",
                                               permanent, "/",
                                               spsublim[i], sep = ""))
    }

    # Non-snow precipitation, 24-hour total
    if(any(data == "NSP") | any(data == "nsp")){
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/", nsprecip1[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/", nsprecip[i], sep = ""),
        remove = TRUE))
      try(R.utils::gunzip(
        filename = paste(oldwd, "/data/",
                         nsprecip2[i], sep = ""),
        destname = paste(oldwd, "/", permanent, "/",
                         nsprecip3[i], sep = ""),
        remove = TRUE))
      map[[(8*i)]] <- raster::raster(paste(oldwd, "/",
                                           permanent, "/",
                                           nsprecip[i], sep = ""))
    }
  }

  # example of how to get delete all the empty elements of a list.
  # raster::brick(a[vapply(a, Negate(is.null), NA)])
  map <- raster::brick(map[vapply(map, Negate(is.null), NA)])

  # Deletes the folder "data" in the working directory and all the it contains
  unlink("data", recursive = TRUE)

  return(map)
}


# share with kinekenneth48
