#' Formats dates into YYYY-MM-DD.
#'
#' @param day day will be written as a 1 or 2 digit character string
#' @param month month will be written as a 1 or 2 digit character string
#' @param year year will be written as a 4 digit character string
#'
#' @return a list of rasters with elements corresponding to the
#'   input vaiable "data".
#'
#' @details This function creates a temporary folder and downloads data
#' from the SNODAS website for the inputed date, map type, and data map
#' that the user provides. This data is stored in the temporary folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list.
#'
#' @export
format_snodas_daily <- function(day = 1:2,
                                month = 4:5,
                                year = 2011:2012){
  # This expands the dates into a data frame of x column being days and
  # y column is the month
  d1 <- expand.grid(x = day, y = month, stringsAsFactors = FALSE)
  paste0(d1$y, "-", d1$x)

  formated_dates <- numeric()

  # This goes through each date and adds the year for each day and month
  # and puts all dates into formatted_dates vector
  for (i in 1:length(year)){
    x <- paste0(rep(year[i], length(d1$y)), "-", d1$y, "-", d1$x)
    formated_dates <- append(formated_dates, x, after = length(formated_dates))
  }
  return(formated_dates)
}



