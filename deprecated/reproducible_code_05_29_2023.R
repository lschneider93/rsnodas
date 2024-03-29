# reproducible code
set.seed(123)
################################################################################
#############              Creating SNODAS and UA MAPS             #############
################################################################################
april_1_snotel <- rsnodas::april_1_snotel_data

# Download Snodas
snodas_april_maps <- download_snodas(dates = format_dates(day = 1,
                                                          month = 4,
                                                          year = 2004:2022),
                                     masked = TRUE,
                                     remove_zip = TRUE,
                                     data_saved = c("swe"),
                                     out_dir = "snodas_data")

plot(snodas_april_maps[[1]], breaks = "equal")

# get the pipe function from magrittr
"%>%" <- magrittr::"%>%"

# Get an shape of utah from the maps package
ut_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = sf::st_crs(snodas_april_maps[[1]]))

# Crop the maps to just the state of utah for
for (i in 1:length(snodas_april_maps)) {
  # crop to the state of utah
  s_ut <- sf::st_crop(snodas_april_maps[[i]], ut_map)

  # store in a  list
  snodas_april_maps[[i]] <- s_ut # ua_map
}

# read in the University of Arizona
ua_april_maps <- stars::read_stars("data-raw/ua_ut_april1.tif")

# split(ua_april_maps)
# plot(ua_april_maps)

################################################################################
#############              Combining SNODAS and UA MAPS            #############
################################################################################
ua_april_maps <- stars::st_warp(ua_april_maps, snodas_april_maps[[1]])

comb_ua_snodas_maps <- vector("list", length(snodas_april_maps))

years <- 2004:2022
for(i in 1:length(snodas_april_maps)) {
  if (i < (length(snodas_april_maps) - 2)) {
    comb_ua_snodas_maps[[i]] <- (.80 * snodas_april_maps[[i]]) +
      (.20 * ua_april_maps[, , , paste0("SWE_04_01_", years[i])])
  } else {
    comb_ua_snodas_maps[[i]] <- snodas_april_maps[[i]]
  }
}

################################################################################
#############          Creating GAM Models for each year           #############
################################################################################
# set.seed(1234)
# Vector of dates
dates <- as.Date(format_dates(day = 1, month = 4, year = 2004:2022))

gam_raster <- vector("list", length(snodas_april_maps))

for (i in 1:length(snodas_april_maps)) {
  g_df <- gam_to_df(model_data = april_1_snotel[april_1_snotel$DATE == dates[i], ],
                    model_x = c("ppt_normal_annual", "elevation",
                                "slope", "aspect"),
                    model_y = c("VALUE"),
                    raster_template = snodas_april_maps[[i]],
                    coords = c("LONGITUDE", "LATITUDE"),
                    path_to_prism = "/Users/loganschneider/Desktop/PRISM")

  gam_model <- mgcv::gam(data = april_1_snotel[april_1_snotel$DATE == dates[i], ],
                         VALUE ~ s(LONGITUDE, LATITUDE, bs = "sos", k = 25) +
                           s(ppt_normal_annual) + s(elevation) + s(slope) +
                           s(aspect), method = "REML")

  gam_raster[[i]] <- df_to_raster(model = gam_model,
                                  data_fram = g_df,
                                  raster_template = snodas_april_maps[[i]])
}

################################################################################
#############           Creating station density maps              #############
################################################################################
density_map <- vector("list", length(snodas_april_maps))

# Create the density map
for (i in 1:length(snodas_april_maps)) {
  density_map[[i]] <- points_to_density_stars(sp_points = april_1_snotel[april_1_snotel$DATE == dates[i], ],
                                              coords = c("LONGITUDE","LATITUDE"),
                                              raster_template = snodas_april_maps$`swe_2020-04-01`,
                                              sigma = 15000,
                                              max_weight = 1,
                                              flat_crs =
                                                "+proj=utm + zone=12 + datum=WGS84")
}


################################################################################
#############           Creating the final maps              #############
################################################################################
i = 1
comb_maps <- vector("list", 19)

for (i in 1:19) {
  comb_maps[[i]] <- ((density_map[[i]]) * gam_raster[[i]]) +
    ((1 - density_map[[i]]) * snodas_april_maps[[i]])
}


################################################################################
#############        Extracting information at the places          #############
################################################################################

april_1_snotel$CV_GAM_MODEL <- 99999
# set.seed(123)
for (i in 1:(length(snodas_april_maps))) {
  # Create a logical vector of all that are in a specific year
  tf_vector <- april_1_snotel$DATE == dates[i]

  # Get the data for just a specific year and calculate the length
  data <- april_1_snotel[tf_vector, ]
  x <- 1:length(data$VALUE)

  # Create a vector that will be the same length
  predictions <- numeric()
  randomized <- sample(x)

  #### Split the station data into 10 parts.  We are going to predict on 10%
  n <- 10
  # n <- x
  chunk <- function(x,n) split(x, factor(sort(rank(x) %% n)))
  numbers <- chunk(randomized, n)
  num <- unlist(numbers)

  for (k in 1:10) {
    #### Creating test and training data set
    train.data  <- data[-numbers[[k]], ]
    test.data <- data[numbers[[k]], ]

    train.data$ppt_normal_annual <- as.numeric(train.data$ppt_normal_annual)
    train.data$slope <- as.numeric(train.data$slope)
    train.data$aspect <- as.numeric(train.data$aspect)
    train.data$elevation <- as.numeric(train.data$elevation)

    test.data$ppt_normal_annual <- as.numeric(test.data$ppt_normal_annual)
    test.data$slope <- as.numeric(test.data$slope)
    test.data$aspect <- as.numeric(test.data$aspect)
    test.data$elevation <- as.numeric(test.data$elevation)

    # Build the model
    model <- mgcv::gam(data = train.data,
                       VALUE ~ s(LATITUDE, LONGITUDE, bs = "sos", k = 25) +
                         s(ppt_normal_annual) + s(slope) + s(aspect) +
                         s(elevation),
                       method = "REML")

    # Make predictions and compute the R2, RMSE and MAE
    # predictions <- model %>% predict(test.data)
    prediction <- predict(model, newdata = test.data)
    predictions <- c(predictions, prediction)
    # j = j + 1
  }

  # organize the predictions to be back in the correct order
  april_1_snotel[tf_vector, "CV_GAM_MODEL"] <- predictions[order(randomized)]

  #### Replacing all negative values with 0.
  april_1_snotel[tf_vector, "CV_GAM_MODEL"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_MODEL"] < 0,
                                                      0, april_1_snotel[tf_vector, "CV_GAM_MODEL"])
}

################################################################################
#############        Extracting information at the places          #############
################################################################################

april_1_snotel$CV_GAM_RASTER_PREDS <- 99999

april_1_snotel[tf_vector, "CV_GAM_SNODAS_50_50"] <- 99999
april_1_snotel[tf_vector, "CV_GAM_SNODAS_60_40"] <- 99999
april_1_snotel[tf_vector, "CV_GAM_SNODAS_70_30"] <- 99999
april_1_snotel[tf_vector, "CV_GAM_SNODAS_80_20"] <- 99999
april_1_snotel[tf_vector, "CV_GAM_SNODAS_90_10"] <- 99999
april_1_snotel[tf_vector, "CV_GAM_SNODAS_100"] <- 99999

# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_50_50"] <- 99999
# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_60_40"] <- 99999
# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_70_30"] <- 99999
# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_80_20"] <- 99999
# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_90_10"] <- 99999
# april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_100"] <- 99999
# set.seed(1234)

# Cross-Validation for all the years
for (i in 1:(length(snodas_april_maps))) {
  # Create a logical vector of all that are in a specific year
  tf_vector <- april_1_snotel$DATE == dates[i]

  # Get the data for just a specific year and calculate the length
  data <- april_1_snotel[tf_vector, ]
  x <- 1:length(data$VALUE)

  # Create a vector that will be the same length
  predictions <- numeric()
  predictions2 <- numeric()
  predictions3 <- numeric()
  predictions4 <- numeric()
  predictions5 <- numeric()
  predictions6 <- numeric()
  predictions7 <- numeric()
  predictions8 <- numeric()
  predictions9 <- numeric()
  predictions10 <- numeric()
  predictions11 <- numeric()
  predictions12 <- numeric()
  predictions13 <- numeric()
  randomized <- sample(x)

  #### Split the station data into 10 parts.  We are going to predict on 10%
  n <- 10
  chunk <- function(x,n) split(x, factor(sort(rank(x) %% n)))
  numbers <- chunk(randomized, n)
  num <- unlist(numbers)

  for (k in 1:10) {
    #### Creating test and training data set
    train.data  <- data[-numbers[[k]], ]
    test.data <- data[numbers[[k]], ]

    train.data$ppt_normal_annual <- as.numeric(train.data$ppt_normal_annual)
    train.data$slope <- as.numeric(train.data$slope)
    train.data$aspect <- as.numeric(train.data$aspect)
    train.data$elevation <- as.numeric(train.data$elevation)

    test.data$ppt_normal_annual <- as.numeric(test.data$ppt_normal_annual)
    test.data$slope <- as.numeric(test.data$slope)
    test.data$aspect <- as.numeric(test.data$aspect)
    test.data$elevation <- as.numeric(test.data$elevation)

    test_df <- gam_to_df(model_data = train.data,
                         model_x = c("ppt_normal_annual", "elevation",
                                     "slope", "aspect"),
                         model_y = c("VALUE"),
                         raster_template = snodas_april_maps[[i]],
                         coords = c("LONGITUDE", "LATITUDE"),
                         path_to_prism = "/Users/loganschneider/Desktop/PRISM")

    gam_model <- mgcv::gam(data = train.data,
                           VALUE ~ s(LONGITUDE, LATITUDE, bs = "sos", k = 25) +
                             s(ppt_normal_annual) + s(elevation) + s(slope) +
                             s(aspect), method = "REML")

    test_gam <- df_to_raster(model = gam_model,
                             data_fram = test_df,
                             raster_template = snodas_april_maps[[1]])

    test.data <- sf::st_as_sf(test.data, coords = c("LONGITUDE", "LATITUDE"),
                              crs = sf::st_crs(snodas_april_maps[[i]]))


    # Extract at the test data points (these are the predictions)
    prediction <- stars::st_extract(test_gam, test.data)$MODEL_PRED
    predictions <- c(predictions, prediction)

    # Create the density plot
    pd_star <- points_to_density_stars(sp_points = train.data,
                                       coords = c("LONGITUDE","LATITUDE"),
                                       raster_template = snodas_april_maps[[i]],
                                       sigma = 15000,
                                       max_weight = .5,
                                       flat_crs =
                                         "+proj=utm + zone=12 + datum=WGS84")

    pd_star2 <- points_to_density_stars(sp_points = train.data,
                                        coords = c("LONGITUDE","LATITUDE"),
                                        raster_template = snodas_april_maps[[i]],
                                        sigma = 15000,
                                        max_weight = .60,
                                        flat_crs =
                                          "+proj=utm + zone=12 + datum=WGS84")

    pd_star3 <- points_to_density_stars(sp_points = train.data,
                                        coords = c("LONGITUDE","LATITUDE"),
                                        raster_template = snodas_april_maps[[i]],
                                        sigma = 15000,
                                        max_weight = .70,
                                        flat_crs =
                                          "+proj=utm + zone=12 + datum=WGS84")

    pd_star4 <- points_to_density_stars(sp_points = train.data,
                                        coords = c("LONGITUDE","LATITUDE"),
                                        raster_template = snodas_april_maps[[i]],
                                        sigma = 15000,
                                        max_weight = .80,
                                        flat_crs =
                                          "+proj=utm + zone=12 + datum=WGS84")

    pd_star5 <- points_to_density_stars(sp_points = train.data,
                                        coords = c("LONGITUDE","LATITUDE"),
                                        raster_template = snodas_april_maps[[i]],
                                        sigma = 15000,
                                        max_weight = .9,
                                        flat_crs =
                                          "+proj=utm + zone=12 + datum=WGS84")

    pd_star6 <- points_to_density_stars(sp_points = train.data,
                                        coords = c("LONGITUDE","LATITUDE"),
                                        raster_template = snodas_april_maps[[i]],
                                        sigma = 15000,
                                        max_weight = 1,
                                        flat_crs =
                                          "+proj=utm + zone=12 + datum=WGS84")

    # combine all of the maps
    # SNODAS_GAM_UA_50_50
    # comb_map <- ((pd_star) * test_gam) +
    #   ((1 - pd_star) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_50_50
    comb_map2 <- ((pd_star) * test_gam) +
      ((1 - pd_star) * (snodas_april_maps[[i]]))

    # SNODAS_GAM_UA_60_40
    # comb_map3 <- ((pd_star2) * test_gam) +
    #   ((1 - pd_star2) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_UA_60_40
    comb_map4 <- ((pd_star2) * test_gam) +
      ((1 - pd_star2) * (snodas_april_maps[[i]]))

    #SNODAS_GAM_UA_70_30
    # comb_map5 <- ((pd_star3) * test_gam) +
    #   ((1 - pd_star3) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_70_30
    comb_map6 <- ((pd_star3) * test_gam) +
      ((1 - pd_star3) * (snodas_april_maps[[i]]))

    # SNODAS_GAM_UA_80_20
    # comb_map7 <- ((pd_star4) * test_gam) +
    #   ((1 - pd_star4) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_80_20
    comb_map8 <- ((pd_star4) * test_gam) +
      ((1 - pd_star4) * (snodas_april_maps[[i]]))

    # SNODAS_GAM_UA_90_10
    # comb_map9 <- ((pd_star5) * test_gam) +
    #   ((1 - pd_star5) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_90_10
    comb_map10 <- ((pd_star5) * test_gam) +
      ((1 - pd_star5) * (snodas_april_maps[[i]]))

    # SNODAS_GAM_UA_100_0
    # comb_map11 <- ((pd_star6) * test_gam) +
    #   ((1 - pd_star6) * (comb_ua_snodas_maps[[i]]))

    # SNODAS_GAM_100_0
    comb_map12 <- ((pd_star6) * test_gam) +
      ((1 - pd_star6) * (snodas_april_maps[[i]]))


    # Make predictions and compute the R2, RMSE and MAE
    # predictions <- model %>% predict(test.data)
    # prediction <- stars::st_extract(comb_map, test.data)$v
    # predictions <- c(predictions2, prediction2)

    prediction3 <- stars::st_extract(comb_map2, test.data)$v
    predictions3 <- c(predictions3, prediction3)

    # prediction4 <- stars::st_extract(comb_map3, test.data)$v
    # predictions4 <- c(predictions4, prediction4)

    prediction5 <- stars::st_extract(comb_map4, test.data)$v
    predictions5 <- c(predictions5, prediction5)

    # prediction6 <- stars::st_extract(comb_map5, test.data)$v
    # predictions6 <- c(predictions6, prediction6)

    prediction7 <- stars::st_extract(comb_map6, test.data)$v
    predictions7 <- c(predictions7, prediction7)

    # prediction8 <- stars::st_extract(comb_map7, test.data)$v
    # predictions8 <- c(predictions8, prediction8)

    prediction9 <- stars::st_extract(comb_map8, test.data)$v
    predictions9 <- c(predictions9, prediction9)

    # prediction10 <- stars::st_extract(comb_map9, test.data)$v
    # predictions10 <- c(predictions9, prediction9)

    prediction11 <- stars::st_extract(comb_map10, test.data)$v
    predictions11 <- c(predictions11, prediction11)

    # prediction12 <- stars::st_extract(comb_map11, test.data)$v
    # predictions12 <- c(predictions12, prediction12)

    prediction13 <- stars::st_extract(comb_map12, test.data)$v
    predictions13 <- c(predictions13, prediction13)
    # j = j + 1
  }

  # organize the predictions to be back in the correct order
  april_1_snotel[tf_vector, "CV_GAM_RASTER_PREDS"] <- predictions[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_50_50"] <- predictions2[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_50_50"] <- predictions3[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_60_40"] <- predictions4[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_60_40"] <- predictions5[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_70_30"] <- predictions6[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_70_30"] <- predictions7[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_80_20"] <- predictions8[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_80_20"] <- predictions9[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_90_10"] <- predictions10[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_90_10"] <- predictions11[order(randomized)]
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_100"] <- predictions12[order(randomized)]
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_100"] <- predictions13[order(randomized)]

  #### Replacing all negative values with 0.
  april_1_snotel[tf_vector, "CV_GAM_RASTER_PREDS"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_RASTER_PREDS"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_RASTER_PREDS"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_50_50"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_50_50"] < 0,
  #                                                               0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_50_50"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_50_50"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_50_50"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_50_50"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_60_40"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_60_40"] < 0,
  #                                                               0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_60_40"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_60_40"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_60_40"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_60_40"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_70_30"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_70_30"] < 0,
  #                                                               0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_70_30"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_70_30"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_70_30"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_70_30"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_80_20"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_80_20"] < 0,
  # 0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_80_20"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_80_20"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_80_20"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_80_20"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_90_10"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_90_10"] < 0,
  #                                                               0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_90_10"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_90_10"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_90_10"] < 0,
                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_90_10"])
  # april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_100"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_100"] < 0,
  #                                                             0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_UA_100"])
  april_1_snotel[tf_vector, "CV_GAM_SNODAS_100"] <- ifelse(april_1_snotel[tf_vector, "CV_GAM_SNODAS_100"] < 0,
                                                           0, april_1_snotel[tf_vector, "CV_GAM_SNODAS_100"])
}

################################################################################
#############        Extracting information at the places          #############
################################################################################


table(april_1_snotel[, "CV_GAM_RASTER_PREDS"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_50_50"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_50_50"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_60_40"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_60_40"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_70_30"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_70_30"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_80_20"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_80_20"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_90_10"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_90_10"] == 99999)
# table(april_1_snotel[, "CV_GAM_SNODAS_UA_100"] == 99999)
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == 99999)
table(april_1_snotel$DATE > "2003-04-01" & april_1_snotel$DATE < "2022-05-01")
april_1_snotel[april_1_snotel$DATE == "2021-04-01", ]

table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_UA_100"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_90_10"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_UA_90_10"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_80_20"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_UA_80_20"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_70_30"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_UA_70_30"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_60_40"])
table(april_1_snotel[, "CV_GAM_SNODAS_100"] == april_1_snotel[, "CV_GAM_SNODAS_UA_60_40"])


# GAM_PREDS - CV results of the gam model

# GAM_RASTER_PREDS - CV results of the GAM Rasters
# GAM_RASTER_COMB_PREDS CV- CV results of 80% SNODAS 20% UA and GAM
#GAM_RASTER_COMB_PREDS2_CV - cross validated results of half gam half SNODAS


april_1_snotel[april_1_snotel$GAM_PREDS != 99999, ]



################################################################################
#############        Extracting information at the places          #############
################################################################################

april_1_snotel$SNODAS_VALUE <- 99999
# april_1_snotel$UA_VALUE <- 99999
# april_1_snotel$UA_VALUE <- ifelse(april_1_snotel$UA_VALUE == 99999, NA, april_1_snotel$UA_VALUE)
# april_1_snotel[, "FULL_GAM_VALUE"] <- 99999
# april_1_snotel[, "FULL_COMB_VALUE"] <- 99999
# april_1_snotel[, "FULL_GAM_SNODAS_VALUE"] <- 99999

# set.seed(123)
for (i in 1:(length(snodas_april_maps))) {
  # Create a logical vector of all that are in a specific year
  tf_vector <- april_1_snotel$DATE == dates[i]

  # Get the data for just a specific year and calculate the length
  data <- april_1_snotel[tf_vector, ]

  # Create an SF object with extract data values
  data <- sf::st_as_sf(data, coords = c("LONGITUDE", "LATITUDE"),
                       crs = sf::st_crs(snodas_april_maps[[i]]))

  # Create a combined map with just SNODAS
  # sno_gam <- ((density_map[[i]]) * gam_raster[[i]]) +
  # ((1 - density_map[[i]]) * (snodas_april_maps[[i]]))

  # Adding SNODAS
  names(snodas_april_maps[[i]]) <- "SNODAS"
  april_1_snotel[tf_vector, "SNODAS_VALUE"] <- stars::st_extract(snodas_april_maps[[i]], data)$SNODAS

  # april_1_snotel[tf_vector, "FULL_GAM_VALUE"] <- stars::st_extract(gam_raster[[i]], data)$MODEL_PRED
  # april_1_snotel[tf_vector, "FULL_COMB_VALUE"] <- stars::st_extract(comb_maps[[i]], data)$v
  # april_1_snotel[tf_vector, "FULL_GAM_SNODAS_VALUE"] <- stars::st_extract(sno_gam, data)$v
  #
  # april_1_snotel[tf_vector, "UA_VALUE"] <- stars::st_extract(ua_april_maps[, , , paste0("SWE_04_01_", years[i])], data)$ua_ut_april1.tif

}

# write.csv(april_1_snotel, file = "station_info_123_2.csv")
april_1_snotel <- read.csv("station_info_123.csv")
# april_1_snotel <- read.csv("station_info_123_2.csv")
# april_1_snotel <- read.csv("station_info_1234.csv")

april_1_snotel_2003 <- april_1_snotel[(april_1_snotel$DATE > "2003-04-01") &
                                        (april_1_snotel$DATE < "2022-04-01"), ]

# 2023 observations in total
table(april_1_snotel_2003$CV_GAM_MODEL != 99999) # GOOD! 2023
table(is.na(april_1_snotel_2003$CV_GAM_MODEL))

table(april_1_snotel_2003$CV_GAM_RASTER_PREDS != 99999) # GOOD, 2023
table(is.na(april_1_snotel_2003$CV_GAM_RASTER_PREDS))

table(april_1_snotel_2003$CV_GAM_SNODAS_UA_50_50 != 99999) # BAD, 2023
table(is.na(april_1_snotel_2003$CV_GAM_SNODAS_UA_50_50)) # 10 are NA's
table(april_1_snotel[is.na(april_1_snotel_2003$CV_GAM_SNODAS_UA_50_50) == TRUE, ]$NAME)

# Why are there NA's?
april_1_snotel_2003[april_1_snotel_2003$NAME == "Gardner Peak", ] #2005-2018
april_1_snotel_2003[april_1_snotel_2003$NAME == "Gutz Peak", ]
april_1_snotel_2003[april_1_snotel_2003$NAME == "Klondike Narrows", ]
april_1_snotel_2003[april_1_snotel_2003$NAME == "Lasal Mountain", ] # Jus


# mutate(prcp_tmax_PRISM_m8_y09, ppt = ppt * 0.0393701)

# table(april_1_snotel$CV_WEIGHTED_RASTER_PREDS != 99999) # has 88 NA's
# table(is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS))
# april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]
# april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$FULL_COMB_VALUE <- (.5 * april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$SNODAS_VALUE) +
#   (.5 * april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$FULL_GAM_VALUE)
#
# april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$CV_WEIGHTED_RASTER_PREDS <- (.5 * april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$SNODAS_VALUE) +
#   (.5 * april_1_snotel[is.na(april_1_snotel$CV_WEIGHTED_RASTER_PREDS) == TRUE, ]$CV_GAM_RASTER_PREDS)
# # april_1_snotel$CV_GAM_RASTER_COMB_PREDS <- ifelse(is.na(april_1_snotel$CV_GAM_RASTER_COMB_PREDS) == TRUE,
# #                                                   0.01, april_1_snotel$CV_GAM_RASTER_COMB_PREDS)
#
# table(april_1_snotel$UA_VALUE == 99999) # missing 130 for year 2021
# table(is.na(april_1_snotel$UA_VALUE)) # 11 + 130 for 2021
# april_1_snotel[is.na(april_1_snotel$UA_VALUE) == TRUE, ]
# table(april_1_snotel[(is.na(april_1_snotel$UA_VALUE) == TRUE) &
#                        april_1_snotel$DATE != "2021-04-01", ]$NAME)
# april_1_snotel[(is.na(april_1_snotel$UA_VALUE) == TRUE) &
#                        april_1_snotel$DATE != "2021-04-01", ]
# april_1_snotel[(is.na(april_1_snotel$UA_VALUE) == TRUE) &
#                  april_1_snotel$DATE != "2021-04-01", ]$UA_VALUE <- 0
# # april_1_snotel$UA_VALUE <- ifelse(is.na(april_1_snotel$UA_VALUE) == TRUE,
# #                                   0.01, april_1_snotel$UA_VALUE)
#
# table(april_1_snotel$SNODAS_VALUE != 99999)
# table(is.na(april_1_snotel$SNODAS_VALUE))
#
# table(april_1_snotel$FULL_GAM_VALUE != 99999)
# table(is.na(april_1_snotel$FULL_GAM_VALUE))
#
# table(april_1_snotel$FULL_GAM_SNODAS_VALUE != 99999)
# table(is.na(april_1_snotel$FULL_GAM_SNODAS_VALUE)) # 23 are na
# april_1_snotel[is.na(april_1_snotel$FULL_GAM_SNODAS_VALUE) == TRUE, ]
# table(april_1_snotel[is.na(april_1_snotel$FULL_GAM_SNODAS_VALUE) == TRUE, ]$NAME)
#
# table(april_1_snotel$FULL_COMB_VALUE != 99999)
# table(is.na(april_1_snotel$FULL_COMB_VALUE)) # 33 NA's
# table(april_1_snotel[is.na(april_1_snotel$FULL_COMB_VALUE) == TRUE, ]$NAME)


# # april_1_snotel <- read.csv("station_info.csv")

# april_1_snotel2 <- april_1_snotel


# Replace NA's with 0.01
# april_1_snotel$CV_GAM_RASTER_COMB_PREDS <- ifelse(is.na(april_1_snotel$CV_GAM_RASTER_COMB_PREDS) == TRUE,
#                                                   0.01, april_1_snotel$CV_GAM_RASTER_COMB_PREDS)
#
# april_1_snotel$CV_GAM_RASTER_SNODAS_PREDS <- ifelse(is.na(april_1_snotel$CV_GAM_RASTER_SNODAS_PREDS) == TRUE,
#                                                     0.01, april_1_snotel$CV_GAM_RASTER_SNODAS_PREDS)
#
# april_1_snotel[, "FULL_COMB_VALUE"] <- ifelse(is.na(april_1_snotel[, "FULL_COMB_VALUE"]) == TRUE,
#                                               0.01, april_1_snotel[, "FULL_COMB_VALUE"])
#
# april_1_snotel[, "FULL_GAM_SNODAS_VALUE"] <- ifelse(is.na(april_1_snotel[, "FULL_GAM_SNODAS_VALUE"]) == TRUE,
#                                                     0.01, april_1_snotel[, "FULL_GAM_SNODAS_VALUE"])
#
#
# all(april_1_snotel2 == april_1_snotel)
#
# dim(april_1_snotel)
# dim(april_1_snotel2)
#
# table(april_1_snotel[, "FULL_GAM_SNODAS_VALUE"])
