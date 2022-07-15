#' Generate a data frame from a generalized additive model
#'
#' @param model_data the data in which the model is create. The variables of
#'   'slope', 'aspect', and 'elevation' need to be lowercase.
#' @param raster_template a data frame with the columns of the model that will
#'   be used to create the model.
#' @param model_x a character vector where each entry symbolizes a column
#'   in the model_data data frame.  These will be the variables used to try
#'   to explain the Independent variable.
#' @param model_y a character vector of the Independent variable.
#' @param coords the LON/LAT column names of the data.  This will be used to
#'   in the model no matter what.
#' @param path_to_prism a character vector that has the file path
#'   to the PRISM files.
#'
#' @return a data frame with the columns with the variables needed to
#'   make predictions of the model.
#'
#' @details This function creates gridded product by taking in a model_data,
#'   raster template, a model object or model x and y's, path to prism variables,
#'
#'
#' @importFrom stats predict
#' @importFrom methods hasArg
#' @importFrom sf st_coordinates st_as_sf st_crs
#' @importFrom stars read_stars st_warp st_as_stars
#' @importFrom terra terrain rast vect extract
#'
#' @details This function creates a permanent folder and downloads data
#'   from the SNODAS website for the selected date(s), map type(s), and data map
#'   that the user provides. This data is stored in the permanent folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list of star objects.
#'
#' @export
gam_to_df <- function(model_data,
                          raster_template,
                          model_x,
                          model_y,
                          coords = c("LONGITUDE", "LATITUDE"),
                          path_to_prism) {

  get_prism_info <- function(x) {
    # split the character variable by the "-"
    nat <- unlist(strsplit(x, "_"))

    # get the information
    name <- nat[1]
    year <- nat[2]
    mon <- nat[3]
    day <- nat[4]

    info <- c(name, year, mon, day)
    info <- info[!is.na(info)]
    return(info)
  }

  rm_asp_slop_elev <- function(x) {
    # match and see which elements in vector correspond with the
    l <- match(c("aspect","elevation", "slope"), x)

    # need to remove the NA arguements
    l <- l[!is.na(l)]

    # remove the other arguments
    x[-l]
  }

  # model_data = snotel_ut_2014
  # raster_template = snodas_ut_2014
  # path_to_prism = "/Users/loganschneider/Desktop/GitHub/prism"
  # model_x <- c("ppt_normal_annual", "elevation")
  # model_y <- c("VALUE")
  # coords = c("LONGITUDE", "LATITUDE")

  # We are going to Make Raster layer of GAM predictions.
  # First, make a data frame of all the grid cells longitude and latitude
  spts <- sf::st_coordinates(raster_template, as_points = TRUE)

  # Change the names to be LAT/LON
  colnames(spts) <- c(coords[1], coords[2])
  lat_lon <- spts

  # Convert to a sf object with point geometry that is in the same crs as raster
  sppts <- sf::st_as_sf(lat_lon, coords = c("LONGITUDE", "LATITUDE"),
                        crs = sf::st_crs(raster_template))

  # Create a Data.frame that includes the values and lon/lat of each point
  df <- as.data.frame(lat_lon)

  # Creating the GAM
    l_model_x <- tolower(model_x)



    # If they have elevation, get slope and aspect now
    ####### ONE POTENTIAL PROBLEM IS THAT ELEVATION NEEDS TO BE LOWER CASE
    if (any(l_model_x %in% c("aspect", "elevation", "slope"))) {

      # This is for Elevation because it is different
      r <- stars::read_stars(paste0(path_to_prism, "/", "PRISM_",
                                    "us_dem_800m_", "bil.bil"))

      # Reproject the stars object to have the same CRS
      r <- stars::st_warp(r, crs = sf::st_crs(raster_template))

      # Extract the Values from the Raster and store in the ghcnd data frame
      # df[, paste0(info_names[i])] <- stars::st_extract(r, sppts)
      r <- terra::rast(r)
      ghcnd_station_terra <- terra::vect(sppts)
      df[, paste0("elevation")] <- terra::extract(r, ghcnd_station_terra)[, 2]

      # Extract and Store the variables of Slope and Aspect using terra
      slope_terra <- terra::terrain(r, neighbors = 8,
                                    v = "slope", unit = "degrees")
      aspect_terra <- terra::terrain(r, neighbors = 8,
                                     v = "aspect", unit = "degrees")

      df[, "slope"] <- terra::extract(slope_terra, ghcnd_station_terra)[, 2]
      df[, "aspect"] <- terra::extract(aspect_terra, ghcnd_station_terra)[, 2]

      # remove the aspect slope and elevation from the model variables
      rm_model_x <- rm_asp_slop_elev(l_model_x)
    }

    # get the PRISM data for the rest of the variables
    for (i in 1:length(rm_model_x)) {

      info <- get_prism_info(rm_model_x[i])
      l_num <- length(info)

      # Annual normal
      if (info[2] == "normal" & info[l_num] == "annual") {
        prism_fp <- paste0("PRISM_", info[1], "_30yr_normal_800m",
                           "M3_", info[l_num], "_bil.bil")

      # Annual 04 month
      } else if (info[2] == "normal" &
                 info[l_num] %in% c("01", "02", "03", "04", "05", "06",
                                    "07", "08", "09", "10", "11", "12")) {

        prism_fp <- paste0("PRISM_", info[1], "_30yr_normal_800m",
                           "M2_", info[l_num], "_bil.bil")

        # Daily Map
      } else if (info[2] != "normal" & length(info) == 4) {
        prism_fp <- paste0("PRISM_", info[1], "_stable_4kmD2_",
                           info[2], info[3], info[4], "_bil.bil")

        # Monthly map
      } else if (info[2] != "normal" & length(info) == 3) {
        prism_fp <- paste0("PRISM_", info[1], "_stable_4kmM3_",
                           info[2], info[3], "_bil.bil")

        # PRISM_ppt_stable_4kmM3_201703_bil
        # yearly data
      } else if (info[2] != "normal" & length(info) == 2) {
        prism_fp <- paste0("PRISM_", info[1], "_stable_4kmM3_",
                           info[2], "_bil.bil")
      }

       # This is for Elevation because it is different
      r <- stars::read_stars(paste0(path_to_prism, "/", prism_fp))

      # Reproject the stars object to have the same CRS
      r <- stars::st_warp(r, crs = sf::st_crs(raster_template))

      # Extract the Values from the Raster and store in the ghcnd data frame
      df[, paste0(rm_model_x[i])] <- stars::st_extract(r, sppts)
    }

#     # I don't know how to create a function that takes a different amount of
#     # arguments ?
#
#     if (length(l_model_x) == 1) {
#       model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                    model_data[, coords[2]],
#                                                    bs = "sos", k = 25) +
#                            s(model_data[, model_x[1]]),
#                          method = "REML")
#   } else if (length(l_model_x) == 2) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                   s(model_data[, model_x[1]]) +
#                   s(model_data[, model_x[2]]),
#                 method = "REML")
#   } else if (length(l_model_x) == 3) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                 s(model_data[, model_x[1]]) +
#                 s(model_data[, model_x[2]]) +
#                 s(model_data[, model_x[3]]),
#               method = "REML")
#   } else if (length(l_model_x) == 4) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                 s(model_data[, model_x[1]]) +
#                 s(model_data[, model_x[2]]) +
#                 s(model_data[, model_x[3]]) +
#                 s(model_data[, model_x[4]]),
#               method = "REML")
#   } else if (length(l_model_x) == 5) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                 s(model_data[, model_x[1]]) +
#                 s(model_data[, model_x[2]]) +
#                 s(model_data[, model_x[3]]) +
#                 s(model_data[, model_x[4]]) +
#                 s(model_data[, model_x[5]]),
#               method = "REML")
#   } else if (length(l_model_x) == 6) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                 s(model_data[, model_x[1]]) +
#                 s(model_data[, model_x[2]]) +
#                 s(model_data[, model_x[3]]) +
#                 s(model_data[, model_x[4]]) +
#                 s(model_data[, model_x[5]]) +
#                 s(model_data[, model_x[6]]),
#               method = "REML")
#   } else if (length(l_model_x) == 7) {
#     model <- mgcv::gam(model_data[, model_y] ~ s(model_data[, coords[1]],
#                                                  model_data[, coords[2]],
#                                                  bs = "sos", k = 25) +
#                 s(model_data[, model_x[1]]) +
#                 s(model_data[, model_x[2]]) +
#                 s(model_data[, model_x[3]]) +
#                 s(model_data[, model_x[4]]) +
#                 s(model_data[, model_x[5]]) +
#                 s(model_data[, model_x[6]]) +
#                 s(model_data[, model_x[7]]),
#               method = "REML")
#   }
#
#     #  Finally Make predictions :0
#     model_predictions <- stats::predict(model, newdata = df)
#     model_predictions <- ifelse(model_predictions < 0, 0,
#                                 model_predictions)
#
#   # add the predictions to the data frame of longitude and latitude
#   df$MODEL_PRED <- model_predictions
#   utah <- df[c("LONGITUDE", "LATITUDE", "MODEL_PRED")]
#
#   # creating a stars object with the predictions
#   utah_pred_star <- stars::st_as_stars(utah, coords = c("LONGITUDE", "LATITUDE"),
#                                        crs = sf::st_crs(raster_template))
#
#   # put the prediction raster into the template of the raster provided
#   utah_pred_star <- stars::st_warp(utah_pred_star, raster_template)

  return(df)
}




#' Helper function in GAM to Raster
#'
#' @param x a character vector with model parameters
#'
#' @return a character vector with model parameters
#'
#' @details helps get rid of aspect slope and elevation in one stop because
#'   they all use the same PRISM file.
#'
rm_asp_slop_elev <- function(x) {
  # match and see which elements in vector correspond with the
  l <- match(c("aspect","elevation", "slope"), x)

  # need to remove the NA arguements
  l <- l[!is.na(l)]

  # remove the other arguments
  x[-l]
}


#' Helper function in GAM to Raster
#'
#' @param x a character vector with model parameters
#'
#' @return a character vector with model parameters
#'
#' @details helps for naming and reading in files for raster or star objects
#'
get_prism_info <- function(x) {
  # split the character variable by the "-"
  nat <- unlist(strsplit(x, "_"))

  # get the information
  name <- nat[1]
  year <- nat[2]
  mon <- nat[3]
  day <- nat[4]

  info <- c(name, year, mon, day)
  info <- info[!is.na(info)]
  return(info)
}

