#' Generate a raster from a generalized additive model
#'
#' @param model to apply to the data and predict.
#' @param model_data the data in which the model is created
#' @param raster_template a data frame with the columns of the model that will
#'   be used to create the model.
#' @param path_to_prism a character vector that has the path to the PRISM files
#'
#' @return a stars object that contains the predictions of the model
#'
#' @details This function creates a permanent folder and downloads data
#' from the SNODAS website for the inputed date(s), map type(s), and data map
#' that the user provides. This data is stored in the permanent folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list and combines them all into a RasterBrick.
#'
#' @importFrom stats predict
#' @importFrom methods hasArg
#' @importFrom sf st_coordinates st_as_sf st_crs
#' @importFrom stars read_stars st_warp st_as_stars
#' @importFrom terra terrain rast vect extract
#' @importFrom mgcv gam
#'
#' @details This function creates a permanent folder and downloads data
#'   from the SNODAS website for the selected date(s), map type(s), and data map
#'   that the user provides. This data is stored in the permanent folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list of star objects.
#'
#' @export
gam_to_raster <- function(model_data,# = april_1_snotel[april_1_snotel$DATE ==
                          #                                                         dates[17], ],
                          raster_template,# = snodas_april_maps[[17]],
                          model = NA,
                          path_to_prism) {# = "/Users/loganschneider/Desktop/PRISM") {


  if (methods::hasArg(model) == TRUE &
      any(class(model) %in% c("gam", "glm", "lm", "rf"))) {
    stop("model needs to be a gam, glm, lm, or random forest object")
  }

  # We are going to Make Raster layer of GAM predictions.
  # First, make a data frame of all the grid cells longitude and latitude
  spts <- sf::st_coordinates(raster_template, as_points = TRUE)

  # Change the names to be LAT/LON
  colnames(spts) <- c("LONGITUDE", "LATITUDE")
  lat_lon <- spts

  # Convert to a sf object with point geometry that is in the same crs as raster
  sppts <- sf::st_as_sf(lat_lon, coords = c("LONGITUDE", "LATITUDE"),
                        crs = sf::st_crs(raster_template))

  # Create a Data.frame that includes the values and lon/lat of each point
  df <- as.data.frame(lat_lon)

  # Read in PRISM
  info <- c('max temp',
            "maximum vapor pressure deficit",
            "mean dew point temperature",
            "mean temp",
            "min temp",
            "minimum vapor pressure deficit",
            "precipitation",
            "elevation")

  info_names <- c('max_temp', "max_vp", "mean_dew_temp", "mean_temp",
                  "min_temp", "min_vp", "annual_precip","ELEVATION")

  file_type <- c("tmax_30yr_normal_800mM2_",
                 "vpdmax_30yr_normal_800mM2_",
                 "tdmean_30yr_normal_800mM2_",
                 "tmean_30yr_normal_800mM2_",
                 "tmin_30yr_normal_800mM2_",
                 "vpdmin_30yr_normal_800mM2_",
                 "ppt_30yr_normal_800mM2_",
                 "us_dem_800m_")

  for (i in 1:8) {
    if (i != 8) {
      r <- stars::read_stars(paste0(path_to_prism, "/", "PRISM_",
                                    file_type[i], "annual_asc.asc"))
    } else {
      # This is for Elevation because it is different
      r <- stars::read_stars(paste0(path_to_prism, "/", "PRISM_",
                                    file_type[i], "asc.asc"))
    }

    # Reproject the stars object to have the same CRS
    r <- stars::st_warp(r, crs = sf::st_crs(raster_template))

    # Extract the Values from the Raster and store in the ghcnd data frame
    # df[, paste0(info_names[i])] <- stars::st_extract(r, sppts)
    r <- terra::rast(r)
    ghcnd_station_terra <- terra::vect(sppts)
    df[, paste0(info_names[i])] <- terra::extract(r, ghcnd_station_terra)[, 2]
  }

  # Extract and Store the variables of Slope and Aspect using terra
  slope_terra <- terra::terrain(r, neighbors = 8,
                                v = "slope", unit = "degrees")
  aspect_terra <- terra::terrain(r, neighbors = 8,
                                 v = "aspect", unit = "degrees")

  df[, "slope"] <- terra::extract(slope_terra, ghcnd_station_terra)[, 2]
  df[, "aspect"] <- terra::extract(aspect_terra, ghcnd_station_terra)[, 2]

  # Check and see if the user inputted a model
  if (methods::hasArg(model) == FALSE) {

    # If no model inputted, create the model
    model <- mgcv::gam(data = model_data,
                       VALUE ~ s(LATITUDE, LONGITUDE, bs = "sos", k = 25) +
                         s(annual_precip) + s(slope) + s(aspect) + s(ELEVATION),
                       method = "REML")

    # Make the predictions
    model_predictions <- stats::predict(model, newdata = df)
    model_predictions <- ifelse(model_predictions < 0, 0,
                                model_predictions)

  } else if (any(class(model) %in% c("gam", "glm", "lm", "rf"))) {

    # skip making the model and make predictions
    model_predictions <- stats::predict(model, newdata = df)
    model_predictions <- ifelse(model_predictions < 0, 0,
                                model_predictions)
  }

  # add the predictions to the data frame of longitude and latitude
  df$MODEL_PRED <- model_predictions
  utah <- df[c("LONGITUDE", "LATITUDE", "MODEL_PRED")]

  # creating a stars object with the predictions
  utah_pred_star <- stars::st_as_stars(utah, coords = c("LONGITUDE", "LATITUDE"),
                                       crs = sf::st_crs(raster_template))

  # put the prediction raster into the template of the raster provided
  utah_pred_star <- stars::st_warp(utah_pred_star, raster_template)

  return(utah_pred_star)
}
