#' Download all .dly weather data from the web.
#'
#' This function is designed to facilitate faster creation of data sets in
#' the functions get_station_data and get_state_data functions.
#' This is done by downloading all .dly files available, then
#' using the downloaded folder as the "source" instead of the web source.
#'
#' @param directory The directory to download the .dly files. Default is the
#' current working directory.
#'
#' @param keep_compressed If TRUE, the compressed file is retained in the
#'   specified directory. The default false removes the tar file after
#'   unzipping.
#'
#' @return Uncompressed ghcnd_all folder and ghcnd-version.txt in the supplied
#' directory.
#'
#' @export
# This code came from Brennan Bean and Jadon Wagstaff snowload package
download_all_ghcnd_stations <- function(directory = ".", keep_compressed = FALSE) {
  # Set file locations and names
  #=============================================================================
  if (directory == ".") directory <- getwd()
  directory <- path.expand(directory)
  # Remove trailing delimiter if present.
  directory <- gsub(directory, pattern = "/$", replacement = "")
  compressed_file <- paste0(directory, "/ghcnd_all.tar.gz")
  source <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd_all.tar.gz"

  # Download data
  #=============================================================================
  message("Downloading ghcnd_all.tar.gz to", directory, "...")
  utils::download.file(source, compressed_file)

  # Uncompress files and delete zip file (if requested)
  #=============================================================================
  message("Uncompressing downloaded station data...")
  utils::untar(compressed_file, exdir = directory)
  if(!keep_compressed){file.remove(compressed_file)}
  message("All files downloaded and uncompressed.")
}


