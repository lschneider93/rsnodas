#' Download Snodas Urls to a specific folder
#'
#' @param dates date with YYYY-MM-dd written as a six digit character string
#' @param masked if TRUE, the maps will be from the masked
#'   if FALSE, the maps returned will be unmasked.
#' @param urls a vector of urls that are from snodas and used to
#'
#' @return a vector of urls that are in the same order as the dates given.
#'
#' @details This function takes dates in the form of YYYY-MM-DD and returns a
#'   vector of urls that can be used to download the information for the
#'   specific dates given.
#'
download_snodas_urls <- function(dates, masked, urls) {
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
  for (i in 1:length(dates)) {

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
    if((month[i] == "01")) {
      month1[i] <- "Jan"
    } else if (month[i] == "02") {
      month1[i] = "Feb"
    } else if (month[i] == "03") {
      month1[i] = "Mar"
    } else if (month[i] == "04") {
      month1[i] = "Apr"
    } else if (month[i] == "05") {
      month1[i] = "May"
    } else if (month[i] == "06") {
      month1[i] = "Jun"
    } else if (month[i] == "07") {
      month1[i] = "Jul"
    } else if (month[i] == "08") {
      month1[i] = "Aug"
    } else if (month[i] == "09") {
      month1[i] = "Sep"
    } else if (month[i] == "10") {
      month1[i] = "Oct"
    } else if (month[i] == "11") {
      month1[i] = "Nov"
    } else if (month[i] == "12") {
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

  if(masked == TRUE) {
    prefix <- "us"
  } else if (masked == FALSE) {
    prefix <- "zz"
  } else stop("Error: Masked needs to be TRUE/FALSE")

  }

  ####### TRY #4   I THINK THIS ONE DOES IT!!!!!! YAY!
  # website: https://stackoverflow.com/questions/48927391/trouble-downloading-tar-in-r

  # Download all of the SNODAS data files and put them in the data folder
  #=============================================================================
  for (i in 1:length(dates)) {
    destfile <- paste(oldwd, "/data",  "/SNODAS_", year[i], month[i], date[i],
                      ".tar", sep = "")
    download.file(urls[i], destfile = destfile, , mod = "wb")
    utils::untar(paste("data/SNODAS_", year[i], month[i], date[i],
                       ".tar", sep = ""),
                 exdir = paste(oldwd, "/data", sep = ""))
  }

}
