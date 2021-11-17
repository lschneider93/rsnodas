# download the data for University of Arizona

# Looks like the user can download from online with just the website:
# https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0719_SWE_Snow_Depth_v1/4km_SWE_Depth_WY
# 1993
# _v01.nc


#' Download yearly University of Arizona 4km maps
#'
#' This function creates a permanent folder and downloads data
#'   from the SNODAS website for the inputed date(s), map type(s), and data map
#'   that the user provides. This data is stored in the permanent folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list and combines them all into a RasterBrick.
#'
#' @param year with YYYY, written as a 4 digit character string
#' @param permanent permanent is the name of a permanent folder that will
#'   be created in the working directory. The User can input a different name
#'   for the permanent folder.
#'
#' @return a brick of rasters with elements corresponding to the
#'   input vaiable "SWE" or Snowdepth.
#'
#' @export
get_ua_yearly <- function(year = 2004,
                          permanent = "permanent folder") {

  # Create a new folder called "per".
  try(dir.create(permanent), silent = TRUE) # User can specify what this permanent folder is called

  # save old working directory file path
  oldwd <- getwd()

  for (i in seq_len(year)) {
    # get the url from the day, month, and year
    url <- paste("https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0719",
                 "_SWE_Snow_Depth_v1/4km_SWE_Depth_WY",
                 year[i], "/SNODAS_v01.nc", sep = '')
    destfile <- paste(oldwd, "/",  permanent, "/4km_SWE_Depth_WY",
                      year, "_v01.nc", sep = '')
    download.file(url, destfile = destfile)
  }

}

