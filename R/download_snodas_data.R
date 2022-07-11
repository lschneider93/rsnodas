#' Download Daily SNODAS Data maps
#'
#' @param dates date with YYYY-MM-dd written as a six digit character string
#' @param masked if TRUE, the maps will be from the masked
#'   if FALSE, the maps returned will be unmasked.
#' @param overwrite if TRUE, if the file already exists it will be overwritten
#' @param remove_zip if TRUE, the temporary folder with all the zipped folder
#'   will be deleted.
#'
#' @param data_saved data_saved is the type of maps wanted. There are 8 options:
#'   "SWE" for Snow-water-equivalent. "
#'   "SP" for Snow precipatation 24 hrs
#'   "SD" for Modeled Snow Depth
#'   "SPT" for Snowpack average temperature
#'   "BSS" for Modeled blowing snow sublimation rate
#'   "MELT" for Modeled melt rate
#'   "SPS" for Modeled snowpack sublimation rate
#'   "NSP" for Non-snow precipitation
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
#' @importFrom utils download.file untar
#' @importFrom R.utils gunzip
#' @importFrom lubridate day month year
#' @importFrom stars read_stars
#'
#' @export

# Modified Code from the following GitHub packages:
# Author: Brian J. Smith, Package: snowdl  https://github.com/bsmity13/snowdl
# Author: Richard E. Marinos, Package: snodasr
#   https://github.com/marinosr/SNODASR

# Adapted for sf and stars functionality by Logan Schneider (May 2022)

download_snodas_data <- function(dates = c("2010-01-01", "2017-4-1"),
                                 masked = TRUE,
                                 overwrite = TRUE,
                                 remove_zip = FALSE,
                                 data_saved = c('swe', 'SP', "SD", "SPT",
                                                'bss', 'melt', 'SPS', 'NSP'),
                                 out_dir = "snodas_data",
                                 GTiff = TRUE) {

  # Check to make sure the inputs are correct
  # Check masked, overwrite, removed, and GTiff is logical expressions

  if (class(masked) != "logical") {
    stop("masked argument must be a logical expression.")
  }

  if (class(overwrite) != "logical") {
    stop("overwrite argument must be a logical expression.")
  }

  if (class(remove_zip) != "logical") {
    stop("remove_zip argument must be a logical expression.")
  }

  if (class(GTiff) != "logical") {
    stop("GTiff argument must be a logical expression.")
  }

  # Check data_saved
  data_saved <- tolower(data_saved)
  if (!(all(data_saved %in% c("swe", "sp", "sd", "spt", 'bss',
                              'melt', "sps", "nsp")))) {
    stop("Argument 'data_saved' must all be correctly spelled.")
  }

  # Create a new folder called "temp". This will be deleted at the end
  if (!dir.exists("temp_data")) {
    dir.create("temp_data", recursive = TRUE)
  }

  # Create out_dir if necessary
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Grab date components of year, month, and day that will be in the url
  date <- as.Date(dates)
  year <- format(date, "%Y")
  month <- format(date, "%m")
  mon <- format(date, "%b")
  d <- format(date, "%d")

  # get the url from the day, month, and year
  if(masked == TRUE) {
    url <- paste("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/",
                 year, "/", month, "_", mon,
                 "/SNODAS_", year, month, d, ".tar", sep = '')

  } else if (masked == FALSE) {
    url <- paste("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/",
                 year, "/", month, "_", mon,
                 "/SNODAS_unmasked", "_",
                 year, month, d, ".tar", sep = '')
  }

  # Download all of the SNODAS data files and put them in the data folder
  #=============================================================================
  #### This one works well!
  # https://stackoverflow.com/questions/48927391/trouble-downloading-tar-in-r
  for (i in seq_len(length(dates))) {
    vncTar <- paste("vnc.tar", sep = "//") # Create destination file name
    download.file(url[i], vncTar, mode = "wb")
    files <- utils::untar(vncTar, list = TRUE) # Saves list of file names
    utils::untar(vncTar, exdir = paste0("temp_data/")) # Extracts the files
  }

  # Remove vnc.tar from directory
  unlink("vnc.tar", recursive = TRUE)

  # create a map object to contain the maps of interest
  map <- vector("list", (length(data_saved) * length(date)))
  j <- 1

  # Unzip all the swe files
  if (any(data_saved == 'swe')) {

    # Get SWE files in all directories
    swe.txt.gz <- list.files("temp_data",
                             pattern = utils::glob2rx('*ssmv11034tS*.txt.gz'),
                             full.names = TRUE,
                             recursive = TRUE)

    # create file paths to temporary data folder and output folder
    swe.dat.gz <- gsub(".txt.", ".dat.", swe.txt.gz, fixed = TRUE)
    dest.swe.dat <- gsub("temp_data", "snodas_data", swe.dat.gz)
    dest.swe.txt <- gsub("temp_data", "snodas_data", swe.txt.gz)
    dest.swe.dat <- gsub(".gz", "", dest.swe.dat)
    dest.swe.txt <- gsub(".gz", "", dest.swe.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(swe.txt.gz[i],
                      dest.swe.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Unzip the dat files
      R.utils::gunzip(swe.dat.gz[i],
                      dest.swe.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- substr(x = basename(dest.swe.txt[i]), start = 28, stop = 35)

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.swe.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }

      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.swe.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/swe",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }



  # Unzip all the sp files
  if (any(data_saved == 'sp')) {
    # Get sp files in all directories
    sp.txt.gz <- list.files("temp_data",
                            pattern = utils::glob2rx('*_ssmv01025S*.txt.gz'),
                            full.names = TRUE,
                            recursive = TRUE)

    sp.dat.gz <- gsub(".txt.", ".dat.", sp.txt.gz, fixed = TRUE)
    dest.sp.dat <- gsub("temp_data", "snodas_data", sp.dat.gz)
    dest.sp.txt <- gsub("temp_data", "snodas_data", sp.txt.gz)
    dest.sp.dat <- gsub(".gz", "", dest.sp.dat)
    dest.sp.txt <- gsub(".gz", "", dest.sp.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(sp.txt.gz[i],
                      dest.sp.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(sp.dat.gz[i],
                      dest.sp.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")

      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.sp.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)

      }

      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.sp.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/sp",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }



  # Unzip all the sd files
  if (any(data_saved == 'sd')) {
    # Get sd files in all directories
    sd.txt.gz <- list.files("temp_data",
                            pattern = utils::glob2rx('*_ssmv11036tS*.txt.gz'),
                            full.names = TRUE,
                            recursive = TRUE)
    sd.dat.gz <- gsub(".txt.", ".dat.", sd.txt.gz, fixed = TRUE)
    dest.sd.dat <- gsub("temp_data", "snodas_data", sd.dat.gz)
    dest.sd.txt <- gsub("temp_data", "snodas_data", sd.txt.gz)
    dest.sd.dat <- gsub(".gz", "", dest.sd.dat)
    dest.sd.txt <- gsub(".gz", "", dest.sd.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(sd.txt.gz[i],
                      dest.sd.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(sd.dat.gz[i],
                      dest.sd.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.sd.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.sd.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/sd",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }

  # Unzip all the spt files
  if (any(data_saved == 'spt')) {
    # Get spt files in all directories
    spt.txt.gz <- list.files("temp_data",
                             pattern = utils::glob2rx('*_ssmv11038wS*.txt.gz'),
                             full.names = TRUE,
                             recursive = TRUE)
    spt.dat.gz <- gsub(".txt.", ".dat.", spt.txt.gz, fixed = TRUE)
    dest.spt.dat <- gsub("temp_data", "snodas_data", spt.dat.gz)
    dest.spt.txt <- gsub("temp_data", "snodas_data", spt.txt.gz)
    dest.spt.dat <- gsub(".gz", "", dest.spt.dat)
    dest.spt.txt <- gsub(".gz", "", dest.spt.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(spt.txt.gz[i],
                      dest.spt.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(spt.dat.gz[i],
                      dest.spt.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.spt.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.spt.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/spt",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }

  # Unzip all the spt files
  if (any(data_saved == 'bss')) {
    # Get bss files in all directories
    bss.txt.gz <- list.files("temp_data",
                             pattern = utils::glob2rx('*_ssmv11039lL*.txt.gz'),
                             full.names = TRUE,
                             recursive = TRUE)
    bss.dat.gz <- gsub(".txt.", ".dat.", bss.txt.gz, fixed = TRUE)
    dest.bss.dat <- gsub("temp_data", "snodas_data", bss.dat.gz)
    dest.bss.txt <- gsub("temp_data", "snodas_data", bss.txt.gz)
    dest.bss.dat <- gsub(".gz", "", dest.bss.dat)
    dest.bss.txt <- gsub(".gz", "", dest.bss.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(bss.txt.gz[i],
                      dest.bss.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(bss.dat.gz[i],
                      dest.bss.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.bss.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.bss.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/bss",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }

  # Unzip all the melt files
  if (any(data_saved == 'melt')) {
    # Get melt files in all directories
    melt.txt.gz <- list.files("temp_data",
                              pattern = utils::glob2rx('*_ssmv11044bS*.txt.gz'),
                              full.names = TRUE,
                              recursive = TRUE)
    melt.dat.gz <- gsub(".txt.", ".dat.", melt.txt.gz, fixed = TRUE)
    dest.melt.dat <- gsub("temp_data", "snodas_data", melt.dat.gz)
    dest.melt.txt <- gsub("temp_data", "snodas_data", melt.txt.gz)
    dest.melt.dat <- gsub(".gz", "", dest.melt.dat)
    dest.melt.txt <- gsub(".gz", "", dest.melt.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(melt.txt.gz[i],
                      dest.melt.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(melt.dat.gz[i],
                      dest.melt.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.melt.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.melt.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/melt",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }

  if (any(data_saved == 'sps')) {
    # Get sps files in all directories
    sps.txt.gz <- list.files("temp_data",
                             pattern = utils::glob2rx('*_ssmv11050lL*.txt.gz'),
                             full.names = TRUE,
                             recursive = TRUE)
    sps.dat.gz <- gsub(".txt.", ".dat.", sps.txt.gz, fixed = TRUE)
    dest.sps.dat <- gsub("temp_data", "snodas_data", sps.dat.gz)
    dest.sps.txt <- gsub("temp_data", "snodas_data", sps.txt.gz)
    dest.sps.dat <- gsub(".gz", "", dest.sps.dat)
    dest.sps.txt <- gsub(".gz", "", dest.sps.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(sps.txt.gz[i],
                      dest.sps.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(sps.dat.gz[i],
                      dest.sps.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix the format because of previously mentioned bug
        xfun::gsub_file(file = dest.sps.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.sps.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/sps",
                                  date[i], ".tif"))
      }
      j <- j + 1

    }
  }


  if (any(data_saved == 'nsp')) {
    # Get nsp files in all directories
    nsp.txt.gz <- list.files("temp_data",
                             pattern = utils::glob2rx('*_ssmv11050lL*.txt.gz'),
                             full.names = TRUE,
                             recursive = TRUE)
    nsp.dat.gz <- gsub(".txt.", ".dat.", nsp.txt.gz, fixed = TRUE)
    dest.nsp.dat <- gsub("temp_data", "snodas_data", nsp.dat.gz)
    dest.nsp.txt <- gsub("temp_data", "snodas_data", nsp.txt.gz)
    dest.nsp.dat <- gsub(".gz", "", dest.nsp.dat)
    dest.nsp.txt <- gsub(".gz", "", dest.nsp.txt)

    for (i in 1:length(dates)) {
      # Unzip the txt files
      R.utils::gunzip(nsp.txt.gz[i],
                      dest.nsp.txt[i],
                      overwrite = overwrite,
                      remove = FALSE)
      # Unzip the dat files
      R.utils::gunzip(nsp.dat.gz[i],
                      dest.nsp.dat[i],
                      overwrite = overwrite,
                      remove = FALSE)

      # Extract date string from filenames
      ds <- date[i]

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (as.Date(ds, format = "%Y%m%d") %in% problem_ds) {
        # Fix nsp
        xfun::gsub_file(file = dest.nsp.txt[i],
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }
      # Read in and save the map
      map[[j]] <- stars::read_stars(paste0(getwd(), "/", dest.nsp.txt[i]))

      # Write a tif object
      if (GTiff == TRUE) {
        stars::write_stars(map[[j]],
                           paste0(getwd(), "/", out_dir, "/nsp",
                                  date[i], ".tif"))
      }
      j <- j + 1
    }
  }

  df <- expand.grid(x = date, y = data_saved)
  formated_names <- vector("character")

  # This goes through each date and adds the year for each day and month
  # and puts all dates into formatted_dates vector
  for (i in 1:length(map)) {
    x <- paste0(df$y[i], "_", df$x[i])
    formated_names <- append(formated_names, x, after = length(formated_names))
  }

  # Used to name the list of maps:
  # https://stackoverflow.com/questions/38643000/naming-list-elements-in-r
  names(map) <- formated_names
  if (remove_zip == TRUE) {
    # Deletes the folder "temp_data" in the working directory and contents
    unlink("temp_data", recursive = TRUE)
  }

  return(map)
}

# share with kinekenneth48
