t <- stars::read_stars(paste0("/Users/loganschneider/Desktop/GitHub/rsnodas/PRISM_us_dem_4km_asc/PRISM_us_dem_4km_asc.asc"))
t2 <- sf::st_transform(t, 4326)
t3_ut <- sf::st_crop(t2, ut_map)

plot(t3_ut)
plot(t2)
ggplot() +
  stars::geom_stars(data = t3_ut) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  # ggtitle(" ") +
  scale_fill_viridis_c(option = "A") +
  labs(x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 13))


s <- stars::read_stars(paste0("/Users/loganschneider/Desktop/GitHub/rsnodas/PRISM_us_dem_4km_bil/PRISM_us_dem_4km_bil.bil"))
s2 <- sf::st_transform(s, 4326)
s3_ut <- sf::st_crop(s2, ut_map)

names(s3_ut) <- "Elevation"
plot(s3_ut)
plot(s2)
g <- ggplot() +
  stars::geom_stars(data = s3_ut) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  # ggtitle(" ") +
  scale_fill_viridis_c(option = "A", limits = c(500, 4000)) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 28)) +
  labs(x = "Longitude", y = "Latitude")


ggsave(filename = paste0("Thesis_Elevation2", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)


# plot the sf object of utah.  We have the elevation and lets plot this.

library(ggplot2)
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




# Trying with chat --------------------------------------------------------

elevation_data <- rgdal::readGDAL("/Users/loganschneider/Desktop/GitHub/rsnodas/PRISM_us_dem_4km_asc/PRISM_us_dem_4km_asc.asc")
elevation_stars <- stars::st_as_stars(elevation_data)

elevation_stars_WGS84 <- st_transform(elevation_stars, 4326)
elevation_stars_WGS84 <- sf::st_crop(elevation_stars_WGS84, ut_map)


names(elevation_stars_WGS84) <- "Elevation"

g <- ggplot() +
  stars::geom_stars(data = elevation_stars_WGS84) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  # scale_fill_viridis_c(option = "A") +
  scale_fill_viridis_c(option = "A", limits = c(500, 4000)) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 22))
g

ggsave(filename = paste0("Thesis_Elevation", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub/rsnodas/")
)
