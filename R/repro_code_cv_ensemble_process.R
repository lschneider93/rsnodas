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

################################################################################
#############                  Creating UA maps                    #############
################################################################################

# read in the University of Arizona
ua_april_maps <- stars::read_stars("data-raw/ua_ut_april1.tif")
ua_april_maps <- split(ua_april_maps)

################################################################################
#############              Combining SNODAS and UA MAPS            #############
################################################################################
ua_april_maps <- stars::st_warp(ua_april_maps, snodas_april_maps[[1]])

comb_ua_snodas_maps <- vector("list", length(snodas_april_maps))

years <- 2004:2022
for(i in 1:length(snodas_april_maps)) {
  if (i <= (length(snodas_april_maps) - 2)) {
    comb_ua_snodas_maps[[i]] <- (.80 * snodas_april_maps[[i]]) +
      (.20 * ua_april_maps[paste0("SWE_04_01_", years[i])])
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
  gam_raster[[i]] <- gam_to_raster(model_data = april_1_snotel[
    april_1_snotel$DATE == dates[i], ],
    raster_template = snodas_april_maps[[i]],
    path_to_prism = "C:/Users/Logan/Desktop/PRISM")
}

################################################################################
#############           Creating station density maps              #############
################################################################################
density_map <- vector("list", length(snodas_april_maps))
density_map_100 <- vector("list", length(snodas_april_maps))

# Create the density map
for (i in 1:length(snodas_april_maps)) {
  density_map[[i]] <- points_to_density_stars(sp_points = april_1_snotel[april_1_snotel$DATE == dates[i], ],
                                              coords = c("LONGITUDE","LATITUDE"),
                                              raster_template = snodas_april_maps$`swe_2020-04-01`,
                                              sigma = 15000,
                                              max_weight = .5,
                                              flat_crs =
                                                "+proj=utm + zone=12 + datum=WGS84")

  density_map_100[[i]] <- points_to_density_stars(sp_points = april_1_snotel[april_1_snotel$DATE == dates[i], ],
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
comb_maps_100 <- vector("list", 19)

for (i in 1:19) {
  comb_maps[[i]] <- ((density_map[[i]]) * gam_raster[[i]]) +
    ((1 - density_map[[i]]) * snodas_april_maps[[i]])

  comb_maps_100[[i]] <- ((density_map_100[[i]]) * gam_raster[[i]]) +
    ((1 - density_map_100[[i]]) * snodas_april_maps[[i]])
}



