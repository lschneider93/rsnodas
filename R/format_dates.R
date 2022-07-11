#' Formats dates into YYYY-MM-DD.
#'
#' This function creates a temporary folder and downloads data
#'   from the SNODAS website for the inputed date, map type, and data map
#'   that the user provides. This data is stored in the temporary folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list.
#'
#' @param day a 1 or 2 digit character string (DD) or (D)
#' @param month a 1 or 2 digit character string (MM) or (M)
#' @param year a 4 digit character string (YYYY)
#'
#' @return a list of rasters with elements corresponding to the
#'   input vaiable "data".
#'
#' @export
format_dates <- function(day = 1:2,
                         month = 4:5,
                         year = 2011:2012) {
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
