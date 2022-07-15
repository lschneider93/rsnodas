#' Download PRISM Climate Data maps
#'
#' @param sp_res a character vector that denotes the spatial resolution of the
#'   climate maps wanted
#' @param data a character vector that is one of the following:
#'   ppt, tmin, tmax, tmean, tdmean, vpdmin, or vpdmax
#' @param start_date a character vector or date object in the format YYYY-MM-DD
#' @param end_date a character vector or date object in the format YYYY-MM-DD
#' @param t_res a character vector that denotes the time resolution
#'   of the map. (daily, monthly, or yearly)
#'
#' @param out_dir out_dir is the name of a output directory or folder that will
#'   be created in the working directory. The User can input a name for the
#'   in which the data will be saved in.
#'
#'
#' @details This function creates a folder and downloads PRISM data
#'  for the selected date(s), map type(s), and data map that the user provides.
#'  The downloaded data is stored in the permanent folder.
#'
# #' @importFrom utils download.file untar
# #' @importFrom R.utils gunzip
# #' @importFrom lubridate day month year
# #' @importFrom stars read_stars
#'
#' @export
# Modified May 2022 by Logan Schneider
download_prism <- function(sp_res = "4km", # or 800m
                           data = "ppt", #c("ppt", c("tmin", "tmax", "tmean"),
                           # data = c("tdmean", "vpdmin", "vpdmax", "ppt")
                           # data = c("ppt","tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")
                           start_date = as.Date("2017-04-01"),
                           end_date = as.Date("2017-08-03"),
                           t_res = "monthly", # monthly, yearly
                           out_dir = paste0(getwd(), "/data-raw/prism")) {

  # Testing to ensure variables are in the correct format
  if ((sp_res != "4km") & (sp_res != "800m")) {
    stop("spatial resolution argument must be 4km or 800m.")
  }

  # Double-checking that every element in data is one of the products
  data <- tolower(data)
  for (i in 1:length(data)) {
    if (!(data[i] %in% c("ppt", "tmin", "tmax", "tmean",
                         "tdmean", "vpdmin","vpdmax"))) {
      stop("all data argument(s) must be a valid data option")
    }
  }

  # If no end_date, then the start date is the end date as well.
  if (missing(end_date)) {end_date <- start_date}

  time_var <- c("daily", "monthly", "yearly")
  time_va <- c("day", "month", "year")
  time_arg <- match(t_res, time_var)

  # get the first and last year, this will be used in the "for" loop
  t <- seq(start_date, end_date, by = time_va[time_arg])

  # For loops depend on time resolution
  if (t_res == "daily") {
    tdate <- seq(start_date, end_date, by = "day")
    years <- gsub("-", "", substring(tdate, 1, 4))
    tdate <- gsub("-", "", tdate)

  } else if(t_res == "monthly") {
    tdate <- seq(start_date, end_date, by = "month")
    years <- gsub("-", "", substring(tdate, 1, 4))

    # remove the day argument and get rid of the "-" and return a 6 character
    tdate <- gsub("-", "", substring(tdate, 1, 7))

  } else if(t_res == "yearly") {
    tdate <- seq(start_date, end_date, by = "year")
    years <- gsub("-", "", substring(tdate, 1, 4))
    tdate <- unique(gsub("-", "", substring(tdate, 1, 4)))
  }

  tsource <- c()
  destination <- c()
  tagname <- c()
  final_location <- c()

  l = 1
  for (var in data) {
    for (j in 1:length(years)) {
      # Creates the vector tdate with all the dates of interest
      tsource[l] <- paste("http://services.nacse.org/prism/data/public",
                          sp_res, var, tdate[j], sep = "/")
      destination[l] <- out_dir
      tagname[l] <- paste("PRISM", var, sp_res, tdate[j], sep = "_")
      final_location[l] <- paste0(destination[l], "/", tagname[l], ".zip")
      l = l + 1
    }
  }


  # Create directory of each file if it doesn't exist.
  # - https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
  for (i in 1:length(destination)) {
    if (!dir.exists(destination[i])) {
      dir.create(destination[i], recursive = TRUE)
    }
  }

  # go through and download, unzip and remove the zipped file
  for (i in 1:length(tsource)) {
    print(paste("Downloading day", i, "of", length(tsource)))
    try(utils::download.file(tsource[i],
                             final_location[i], mode = "wb"))
    try(utils::unzip(final_location[i],
                     exdir = destination[i]))
    try(file.remove(final_location[i]))
    Sys.sleep(1)
  }
}
