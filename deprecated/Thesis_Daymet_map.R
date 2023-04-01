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
names(yearly_brick) <- "Value"
g2 <- ggplot() +
  stars::geom_stars(data = yearly_brick) +
  # geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle("Daymet SWE predictions") +
  scale_fill_viridis_c(option = "D",  limits = c(0, 750)) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g2

#
ggsave(filename = paste0("Thesis Daymet map for 2015_NOV202022", ".png"),
       plot = g2,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)


g <- ggplot() +
  stars::geom_stars(data = yearly_brick) +
  # geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle("Daymet SWE predictions") +
  scale_fill_viridis_c(option = "A",  limits = c(0, 750)) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 24))

g
#
ggsave(filename = paste0("Thesis Daymet map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

