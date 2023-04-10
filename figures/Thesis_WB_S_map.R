basin <- sf::st_read("/Users/loganschneider/Desktop/GitHub/MISC/hydrologic_units/wbdhu8_a_ut.shp")


basin <- basin[-c(1,2,3,4,5,6,7,8,9,11, 12)]

basin_simp <- sf::st_simplify(basin)

library(dplyr)

# Convert to sf objects
for (i in 1:length(snodas_april_maps)) {

  # Create a logical vector of all that are in a specific year
  tf_vector <- april_1_snotel$DATE == dates[i]

  # Get the data for just a specific year and calculate the length
  data <- april_1_snotel[tf_vector, ]

  data_sf <- sf::st_as_sf(data, coords = c("LONGITUDE", "LATITUDE"),
                          crs = sf::st_crs(4326))

  data_sf <- st_join(data_sf, basin_simp, join = st_intersects)

  try <- data_sf %>%
    group_by(name) %>%
    summarize(g_median_bias = median(VALUE - CV_GAM_RASTER_PREDS),
              geometry = geometry,
              s_median_bias = median(VALUE - SNODAS_VALUE),
              ua_median_bias = median(VALUE - UA_VALUE),
              gs_median_bias = median(VALUE - CV_GAM_RASTER_SNODAS_PREDS),
              fg_median_bias = median(VALUE - FULL_GAM_VALUE),
              fgs_median_bias = median(VALUE - FULL_GAM_SNODAS_VALUE)) %>%
    filter(row_number()==1)

  # hist(try$mean_bias)
  # hist(try$median_bias, breaks = 20)
  # try[try$mean_bias == "-105.6", ]

  # Create a sf object htat has the mean bias
  # try2 <- st_join(basin_simp, try)

  c4 <- ggplot() +
    geom_sf(data = try2, aes(fill = g_median_bias)) +
    geom_sf(data = ut_map, fill = "NA", size = 1, color = "black") +
    geom_sf(data = data_sf) +
    ggtitle("Average Bias in Water Basins",
            paste("for the year", year[i])) +
    # scale_fill_steps(n.breaks = 8) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         #limits = c(-400, 400),
                         na.value = "gray") +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          text = element_text(size = 16))
  # theme_void()
  ggsave(filename = paste0("Average_", year[i], ".png"),
         plot = c4,
         path = paste0(getwd(), "/figures")
  )
}

# adding the names of the watersheds of each station
for (i in 1:length(april_df_ghcnd)) {
  april_df_ghcnd[[i]]$watershed_name <- april_df_ghcnd_spatial[[i]]$name
}

library(dplyr)
# df <-
april_df_ghcnd_spatial <- april_df_ghcnd

for(i in 1:length(april_df_ghcnd)) {
  april_df_ghcnd_spatial[[i]] <- sf::st_as_sf(april_df_ghcnd_spatial[[i]],
                                              coords = c("LONGITUDE", "LATITUDE"),
                                              crs = sf::st_crs(4326))
}


# Create a shape of utah that I have put on the outside of the figure
utstate <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

# Create a plot for each year
for (i in 1:length(april_df_ghcnd)) {
  try <- april_df_ghcnd_spatial[[i]] %>%
    group_by(watershed_name) %>%
    summarize(mean_bias = mean(SNODAS_VALUE - VALUE), geometry = geometry,
              median_bias = median(SNODAS_VALUE - VALUE)) %>%
    filter(row_number()==1)

  # hist(try$mean_bias)
  # hist(try$median_bias, breaks = 20)
  # try[try$mean_bias == "-105.6", ]

  # Create a sf object htat has the mean bias
  try2 <- st_join(basin_simp, try)

  c4 <- ggplot() +
    geom_sf(data = try2, aes(fill = mean_bias)) +
    geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
    geom_sf(data = april_df_ghcnd_spatial[[i]]) +
    ggtitle("Average Bias in Water Basins",
            paste("for the year", year[i])) +
    # scale_fill_steps(n.breaks = 8) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         #limits = c(-400, 400),
                         na.value = "gray") +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          text = element_text(size = 16))
  # theme_void()
  ggsave(filename = paste0("Average_", year[i], ".png"),
         plot = c4,
         path = paste0(getwd(), "/figures")
  )


  c4 <- ggplot() +
    geom_sf(data = try2, aes(fill = median_bias)) +
    geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
    geom_sf(data = april_df_ghcnd_spatial[[i]]) +
    ggtitle("Median Bias in Water Basins",
            paste("for the year", year[i])) +
    # scale_fill_steps(n.breaks = 8) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         #limits = c(-400, 400),
                         na.value = "gray") +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          text = element_text(size = 16))
  # theme_void()

  ggsave(filename = paste0("Median", year[i], ".png"),
         plot = c4,
         path = paste0(getwd(), "/figures")
  )
}

