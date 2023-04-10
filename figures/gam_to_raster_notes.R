#' Generate a raster from a generalized additive model
#'
#' @param model to apply to the data and predict.
#' @param model_data
#' @param raster_template a data frame with the columns of the model that will be
#'   used to create the model.
#'
#' @return a stars object that contains the predictions of the model
#'
#' @details This function creates a permanent folder and downloads data
#' from the SNODAS website for the inputed date(s), map type(s), and data map
#' that the user provides. This data is stored in the permanent folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list and combines them all into a RasterBrick.
#'
#' @importFrom utils download.file untar
#' @importFrom R.utils gunzip
#' @importFrom lubridate day month year
#'
#' @export
gam_to_raster <- function(model_data = april_df_ghcnd[[17]],
                          raster_template = snodas_maps_utah[[17]],
                          model) {

  if (methods::hasArg(model) == TRUE &
      any(class(model) %in% c("gam", "glm", "lm", "rf"))) {
    stop("model needs to be a gam, glm, lm, or random forest object")
  }

  # if(!any(class(model) %in% c("gam", "glm", "lm", "rf"))) {
  #   model <- mgcv::gam(data = model_data,
  #                      VALUE ~ s(LATITUDE, LONGITUDE, bs = "sos", k = 25) +
  #                        s(annual_precip) + s(slope) + s(aspect) + s(ELEVATION),
  #                      method = "REML")
  # }

  # ###  Make Raster layer of GAM predictions
  # First make a data frame of all the grid cell lat/lon
  spts <- sf::st_coordinates(raster_template, as_points = TRUE)
  # Change the names to be LAT/LON
  # dim(spts); class(spts);
  colnames(spts) <- c("LONGITUDE", "LATITUDE")
  lat_lon <- spts

  # Convert to a sf object with point geometry
  sppts <- sf::st_as_sf(lat_lon, coords = c("LONGITUDE", "LATITUDE"),
                        crs = sf::st_crs(raster_template))

  # # ### Create a Data.frame that includes the values and lon/lat of each point
  df <- as.data.frame(lat_lon)
  #
  # colnames(df) <- c("MODEL_SWE", "LONGITUDE", "LATITUDE")
  #
  # # Create a spatial points data frame or a SF object.
  # sppts <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"),
  #                   crs = st_crs(r8))
  # sppts

  # Read in PRISM
  mon <- c('annual')
  v <- c("annual")

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
      r <- stars::read_stars(paste0("C:/Users/Logan/Desktop/PRISM/annual",
                                    "/monthly/", info[i], "/PRISM_",
                                    file_type[i], "annual_asc.asc"))
    } else {
      # This is for Elevation because it is different
      r <- stars::read_stars(paste0("C:/Users/Logan/Desktop/PRISM/annual",
                                    "/monthly/", info[i], "/PRISM_",
                                    file_type[i], "asc.asc"))
    }

    # Reproject the stars object to have the same CRS
    r <- stars::st_warp(r, crs = sf::st_crs(4326))

    # Extract the Values from the Raster and store in the ghcnd data frame
    # df[, paste0(info_names[i])] <- stars::st_extract(r, sppts)
    r <- terra::rast(r)
    ghcnd_station_terra <- terra::vect(sppts)
    df[, paste0(info_names[i])] <- terra::extract(r, ghcnd_station_terra)[, 2]
  }

  # df[, "ANNUAL_maxtemp"] <- stars::st_extract(r, sppts)$
  #   "PRISM_tmax_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ANNUAL_maxvapor"] <- stars::st_extract(r2, sppts)$
  #   "PRISM_vpdmax_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ANNUAL_meandew"] <- stars::st_extract(r3, sppts)$
  #   "PRISM_tdmean_30yr_normal_800mM2_annual_asc"
  # df[, "ANNUAL_meantemp"] <- stars::st_extract(r4 , sppts)$
  #   "PRISM_tmean_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ANNUAL_mintemp"] <- stars::st_extract(r5, sppts)$
  #   "PRISM_tmin_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ANNUAL_minvapor"] <- stars::st_extract(r6, sppts)$
  #   "PRISM_vpdmin_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ANNUAL_precip"] <- stars::st_extract(r7, sppts)$
  #   "PRISM_ppt_30yr_normal_800mM2_annual_asc.asc"
  # df[, "ELEVATION"] <- stars::st_extract(r8, sppts)$
  #   "PRISM_us_dem_800m_asc.asc"

  # # ##### EXTRACT and STORE SLOPE and ASPECT
  # ele_terra2 <- terra::rast(r)
  slope_terra <- terra::terrain(r, neighbors = 8,
                                v = "slope", unit = "degrees")
  aspect_terra <- terra::terrain(r, neighbors = 8,
                                 v = "aspect", unit = "degrees")


  df[, "slope"] <- terra::extract(slope_terra, ghcnd_station_terra)[, 2]
  df[, "aspect"] <- terra::extract(aspect_terra, ghcnd_station_terra)[, 2]

  # df[, "Aspect"] <- stars::st_extract(utah_aspect_stars, sppts)$"aspect"
  # df[, "Slope"] <- stars::st_extract(utah_slope_stars, sppts)$"slope"

  # colnames(df)

  # ##### EXTRACT and STORE SLOPE and ASPECT
  # utah_grid_raster <- df

  # df <- read.csv("/Users/loganschneider/Desktop/GitHub/rsnodas/utah_grid.csv")
  # dim(df);
  # make sure that x's are numeric
  # model_data$Annual_precip <- as.numeric(model_data$Annual_precip)
  # model_data$Slope <- as.numeric(model_data$Slope)
  # model_data$Aspect <- as.numeric(model_data$Aspect)
  # model_data$ELEVATION <- as.numeric(model_data$ELEVATION)

  # Check and see if the user inputted a model
  if (methods::hasArg(model) == FALSE) {

    # If no model inputted, create the model
    model <- mgcv::gam(data = model_data,
                       VALUE ~ s(LATITUDE, LONGITUDE, bs = "sos", k = 25) +
                         s(annual_precip) + s(slope) + s(aspect) + s(ELEVATION),
                       method = "REML")

    # Make the predictions
    model_predictions <- predict(model, newdata = df)
    model_predictions <- ifelse(model_predictions < 0, 0,
                                model_predictions)

  } else if (any(class(model) %in% c("gam", "glm", "lm", "rf"))) {

    # skip making the model and make predictions
    model_predictions <- predict(model, newdata = df)
    model_predictions <- ifelse(model_predictions < 0, 0,
                                model_predictions)
  }

  # add the precictions to the data frame of longitude and latitude
  # utah_grid_raster$MODEL_PRED <- model_predictions
  df$MODEL_PRED <- model_predictions

  utah <- df[c("LONGITUDE", "LATITUDE", "MODEL_PRED")]

  #### creating a stars object
  #coordinates(utah) <- ~ LONGITUDE + LATITUDE
  # utah_sf <- sf::st_as_sf(utah, coords = c("LONGITUDE", "LATITUDE"))
  # utah_pred_star <- stars::st_as_stars(utah_sf)
  utah_pred_star <- stars::st_as_stars(utah, coords = c("LONGITUDE", "LATITUDE"),
                                        crs = sf::st_crs(4326))

  utah_pred_star <- stars::st_warp(utah_pred_star, raster_template)


  # coerce to SpatialPixelsDataFrame
  #gridded(utah) <- TRUE

  # coerce to raster
  # rasterDF <- raster::raster(utah)

  return(utah_pred_star)
}
