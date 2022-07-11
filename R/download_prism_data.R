#' Download PRISM Climate Data maps
#'
#' @param sp_res spatial resolution of the climate maps wanted
#' @param normals if TRUE, the maps the normal for the last 30 years
#' @param overwrite if TRUE, if the file already exists it will be overwritten
#' @param remove_zip if TRUE, the temporary folder with all the zipped folder
#'   will be deleted.
#'
#' @param data_saved data_saved is the type of maps wanted. There are 8 options:
#'
#'
#' @param out_dir out_dir is the name of a output directory or folder that will
#'   be created in the working directory. The User can input a name for the
#'   in which the data will be saved in.
#'
#' @param GTiff a logical variable that will save the maps of interest as a
#'   gtif object.
#'
#' @return a list of star objects with elements corresponding to the
#'   input vaiable "data_saved".
#'
#' @details This function creates a permanent folder and downloads data
#'   from the SNODAS website for the selected date(s), map type(s), and data map
#'   that the user provides. This data is stored in the permanent folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list of star objects.
#'
# #' @importFrom utils download.file untar
# #' @importFrom R.utils gunzip
# #' @importFrom lubridate day month year
# #' @importFrom stars read_stars
#'
#' @export

# Modified May 2022 by Logan Schneider
download_prism_data <- function(sp_res = "4km", # or 800m
                                var = "ppt", # ppt, tmin, tmax, tmean, tdmean, vpdmin, or vpdmax
                                start_date = as.Date("2012-01-01"),
                                end_date = as.Date("2013-01-15"),
                                t_res = "monthly", # daily, yearly
                                normal = "FALSE",
                                out_dir = "data-raw/prism") {

  # Testing to ensure variables are in the correct format
  if ((sp_res != "4km") & (sp_res != "800m")) {
    stop("spatial resolution argument must be 4km or 800m.")
  }

  if (!(var %in% c("ppt", "tmin", "tmax", "tmean",
                  "tdmean", "vpdmin","vpdmax"))) {
    stop("variable must be ppt, tmin, tmax, tmean, tdmean, vpdmin, or vpdmax")
  }

  # If no end_date, then the start date is the end date as well.
  if (missing(end_date)) {end_date <- star_date}
  if (class(start_date) != "Date" | class(end_date) != "Date") {
    stop("start and end date need to be of date class")
  }

  sp_res = "4km" # 800m
  var = "ppt" # ppt, tmin, tmax, tmean, tdmean, vpdmin, or vpdmax
  start_date = 2012
  end_date =
  t_res = "monthly" # daily, yearly
  normal = "FALSE"

  # we need to get variables for each year
  for (year in start_year:end_year) {
    for (var in c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")) {

    # Create all the days of the year if requested.
    if (t_res == "daily") {
      tdate <- seq(start_date,
                   end_date,
                   by = "day")
      tdate <- gsub("-", "", tdate)
    } else if(t_res == "monthly") {
      tdate <- seq(start_date,
                   end_date,
                   by = "month")
      # remove the day arguement and get rid of the "-".
      # this returns a 6 character
      tdate <- gsub("-", "", substring(tdate, 1, 7))

    } else {
      tdate <- seq(start_date,
                   end_date,
                   by = "year")

      tdate <- unique(gsub("-", "", substring(tdate, 1, 4)))
    }

    tsource <- paste("http://services.nacse.org/prism/data/public", sp_res, var, tdate, sep = "/")
    destination <- paste(out_dir, year, var, t_res, sep = "/")
    tagname <- paste("PRISM", var, sp_res, tdate, sep = "_")
    final_location <- paste0(destination, "/", tagname, ".zip")

    # Create directory if it doesn't exist.
    # - https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
    if (!dir.exists(destination)){
      dir.create(destination, recursive = TRUE)
    }

    for(i in 1:length(tsource)) {
      print(paste("Downloading day", i, "of", length(tsource)))
      try(utils::download.file(tsource[i], final_location[i], mode = "wb"))
      try(utils::unzip(final_location[i],
                       exdir = destination))
      try(file.remove(final_location[i]))
      Sys.sleep(1)
    }
  }
}







