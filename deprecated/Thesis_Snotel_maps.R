install.packages("snotelr")
library(mapdata)

snotel_sites_meta <- snotelr::snotel_info()
dim(snotel_sites_meta)

# plot all the Snotel Sites
state_maps <- sf::st_as_sf(maps::map("state",
                                     c("utah", "arizona", "california",
                                                "colorado", "idaho", "montana",
                                                "new mexico", "nevada", "oregon",
                                                "washington", "wyoming"),
                                     plot = FALSE, fill = TRUE))
ak_map <- sf::st_as_sf(maps::map("world2Hires", "USA:Alaska",
                                 fill = FALSE, plot = FALSE))
plot(state_maps)
plot(ak_map)

# change into a sf object
snotel_sites_meta_sf <- sf::st_as_sf(snotel_sites_meta,
                                     coords = c("longitude", "latitude"),
                                     crs = sf::st_crs(state_maps))


g <- ggplot() +
  # geom_sf(aes(fill = state_maps)) +
  # geom_sf(data = state_maps, size = 1, aes(fill = ID)) +
  geom_sf(data = state_maps, size = 1, fill = "white") +
  ggtitle("SNOTEL Stations") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
                text = element_text(size = 22)) +
  geom_sf(data = snotel_sites_meta_sf[(snotel_sites_meta_sf$state != "AK") #&
                                      #(snotel_sites_meta_sf$state != "SD")
                                      , ])

g

ggsave(filename = paste0("SNOTEL stations", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
)



citation("snotelr")

# g <- ggplot() +
#   stars::geom_stars(data = snodas_april_maps[[12]]) +
#   ggtitle(paste("2015 SNODAS SWE predictions")) +
#   geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
#   scale_fill_viridis_c(option = "A", limits = c(0, 750)) +
#   theme(plot.title = element_text(hjust = 0.5, size = 28),
#         text = element_text(size = 28),
#         legend.title = element_text(size = 32))
#
# g
# ggsave(filename = paste0("Thesis SNODAS map for 2015", ".png"),
#        plot = g,
#        width = 8.46,
#        height = 8.42,
#        path = paste0(getwd(), "/figures")
#        # width = 8.07,
#        # height = 6.87
# )











# ggplot(try2) +
#   geom_sf(aes(fill = med_bias)) +
#   geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
#   geom_sf(data = april_df_ghcnd_spatial[[17]]) +
#   ggtitle("SNODAS Median Bias in Water Basins",
#           paste("for the year", year[17])) +
#   theme(plot.title = element_text(hjust = 0.5, size = 20),
#         text = element_text(size = 16)) +
#   # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
#   scale_fill_manual(values = c("#dfc27d", "#f6e8c3", "#f5f5f5",
#                                "#c7eae5", "#80cdc1", "#35978f")
#   )






plot(snotel_sites_meta_sf[snotel_sites_meta$state == "AK",])
table(snotel_sites_meta_sf$state)


#
table(snotel_sites_meta$state == "UT") # 135 sites in Utah
ut_snotel_sites <- snotel_sites_meta[snotel_sites_meta$state == "UT", ]


#
ut_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = sf::st_crs(snodas_april_maps[[1]]))


#
table(snotel_sites_meta$state == "UT") # 135 sites in Utah
ut_snotel_sites <- snotel_sites_meta[snotel_sites_meta$state == "UT", ]





























# try <- april_1_snotel[april_1_snotel$DATE == "2020-04-01", ] %>%
#   group_by(name) %>%
#   summarize(mean_bias = mean(SNODAS_VALUE - VALUE), geometry = geometry,
#             median_bias = median(SNODAS_VALUE - VALUE)) %>%
#   filter(row_number() == 1)
#
# # Create a sf object that has the mean bias
# try2 <- st_join(basin_simp, try)
# try2$med_bias <- cut(try2$median_bias,
#                      breaks = c(-300, -125, -70, -30, -10, 10, 30, 70, 125, 300),
#                      include.lowest = TRUE)
#
# # unique(try2$med_bias)
#
# try2$m_col <- RColorBrewer::brewer.pal(11, "BrBG")[try2$med_bias]
#
# c4 <- ggplot(try2) +
#   geom_sf(aes(fill = med_bias)) +
#   geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
#   geom_sf(data = april_df_ghcnd_spatial[[17]]) +
#   ggtitle("SNODAS Median Bias in Water Basins",
#           paste("for the year", year[17])) +
#   theme(plot.title = element_text(hjust = 0.5, size = 20),
#         text = element_text(size = 16)) +
#   # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
#   scale_fill_manual(values = c("#dfc27d", "#f6e8c3", "#f5f5f5",
#                                "#c7eae5", "#80cdc1", "#35978f")
#   )
#
