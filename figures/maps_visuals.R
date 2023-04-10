# SNODAS
names(snodas_april_maps[[12]]) <- "Value"
snodas_april_maps[[12]]
g <- ggplot() +
  stars::geom_stars(data = snodas_april_maps[[12]]) +
  ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g
ggsave(filename = paste0("Thesis SNODAS map for 2015", ".png"),
       plot = g,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

# UNIVERSITY of Arizona
names(ua_april_maps) <- "Value"
g <- ggplot() +
  stars::geom_stars(data = ua_april_maps[, , , paste0("SWE_04_01_", years[12])]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle(paste("2015 UA SWE predictions")) +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))
g
ggsave(filename = paste0("Thesis UA map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
)

# DAYMET
# desktop/graduate studies/graduate classes 2022 spring/spatial_project_stat5410/data-raw/daymet/swe_daily_2015_ncss.nc
#
yearly_brick <- raster::brick(paste0("/Users/loganschneider/Desktop/Graduate Studies/",
                                     "Graduate classes 2022 spring/spatial_project_stat5410/data-raw/Daymet/swe_daily_", years[12],
                                     "_ncss.nc"))
# # get April 1st
# # +proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60
# # +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
yearly_brick <- yearly_brick[[paste0("X", years[12], ".04.01")]]
# yearly_brick <- raster::projectRaster(yearly_brick, crs = "+init=epsg:4326")
#
# # change to a stars object
yearly_brick <- stars::st_as_stars(yearly_brick)
#
#
# # Set the Crs to be the one provided on the Daymet Website
# # sf::st_crs(ua_april_swe_star) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# # Change to match the crs of WGS 84
yearly_brick <- stars::st_warp(yearly_brick,
                               # snodas_2015[[i]])
                               # crs = sf::st_crs(ua_april_swe_stars[[i]]))
                               crs = sf::st_crs(density_map[[12]]))

# s_ut <- sf::st_crop(yearly_brick, ut_map)
#
# #
names(yearly_brick) <- "Daymet_Pred"
g <- ggplot() +
  stars::geom_stars(data = yearly_brick) +
  # geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle("Daymet SWE predictions") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g
#
# ggsave(filename = paste0("Thesis Daymet map for 2015", ".png"),
#        plot = g,
#        width = 8.46,
#        height = 8.42,
#        path = paste0(getwd(), "/figures")
#        # width = 8.07,
#        # height = 6.87
# )

#
names(comb_ua_snodas_maps[[12]]) <- "Pred"
g <- ggplot() +
  stars::geom_stars(data = comb_ua_snodas_maps[[12]]) +
  ggtitle("SNODAS and UA SWE predictions") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        text = element_text(size = 16))

ggsave(filename = paste0("SNODAS and UA map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)


################################################################################
# plot the stars density
names(density_map[[12]]) <- "weights"
g <- ggplot() +
  stars::geom_stars(data = density_map[[12]]) +
  ggtitle("2015 Station Density") +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_fill_viridis_c(option = "D") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g

ggsave(filename = paste0("Station Density weights for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)


names(density_map[[12]]) <- "weights"
offset_weight <- (1 - density_map[[12]])
g <- ggplot() +
  stars::geom_stars(data = offset_weight) +
  # ggtitle("2015 Station Density") +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_fill_viridis_c(option = "D") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g
ggsave(filename = paste0("1-weights density", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
)


# plot the University of Arizona 2020
g <- ggplot() +
  stars::geom_stars(data = ua_april_swe_stars[[17]]) +
  ggtitle("UA Model SWE predictions for 2020") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g

ggsave(filename = paste0("UA Model SWE predictions for 2020", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

# plot the GAM predictions for 2015
gam_raster[[i]]
names(gam_raster[[12]]) <- "Value"
g <- ggplot() +
  stars::geom_stars(data = gam_raster[[12]]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle("2015 GAM SWE predictions") +
  scale_fill_viridis_c(option = "A", limits = c(0, 600)) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g


ggsave(filename = paste0("GAM Model SWE predictions for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)


# Plot the density for 2015
# density_map[[i]]) * gam_raster[[i]]
s <- density_map[[12]] * gam_raster[[12]]
names(s) <- "W_Preds"
g <- ggplot() +
  stars::geom_stars(data = (s)) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "black") +
  ggtitle("Weighted GAM SWE predictions for 2015") +
  scale_fill_viridis_c(option = "B") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        text = element_text(size = 16))

g

ggsave(filename = paste0("Weighted GAM Model SWE predictions for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

#################################################################################

# Plot Combined

temp <- (snodas_april_maps[[12]] * (1 - density_map[[12]])) +
  (gam_raster[[12]] * (density_map[[12]]))
names(temp) <- "Value"

g <- ggplot() +
  stars::geom_stars(data = temp) +
  ggtitle("2015 Ensemble SWE predictions") +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        text = element_text(size = 16))
g
ggsave(filename = paste0("Ensemble map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)


# t <- stars::st_extract(temp, april_1_snotel)
