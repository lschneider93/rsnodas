# Crop the map to just the state of utah and name the values to be "SWE Value"
comb_map <- sf::st_crop(comb_map, ut_map)
names(comb_map) <- "SWE\nValue\n(mm)"


ggplot() +
  stars::geom_stars(data = comb_map) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  ggtitle("2021 Ensemble Map") +
  scale_fill_viridis_c(option = "B", limits = c(0, 850), na.value = "white") +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() +
  theme(legend.position = "none")



# Indiviual Maps 5/29/2023 ----------------------------------------------------------

# SNODAS ----------------------------------

names(snodas_april_maps[[17]]) <- "SWE\nValue\n(mm)"
snodas_april_maps[[17]]

plot(snodas_april_maps[[17]])

g <- ggplot() +
  stars::geom_stars(data = snodas_april_maps[[17]]) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = NA, size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  # scale_fill_continuous(na.value = "white") +
  scale_fill_viridis_c(option = "A",
                       limits = c(0, 1200),
                       na.value = "white") +
  # labs(x = "", y = "") +
  # theme(plot.title = element_text(hjust = 0.5, size = 24),
  #       text = element_text(size = 1),
  #       plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
  #       legend.position = "none",
  #       axis.ticks = element_blank()) +
  theme_void() +
  theme(legend.position = "bottom")


g


ggsave(filename = paste0("Thesis SNODAS map for 2020", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)


# UA map ------------------------------------------------------------------


# UNIVERSITY of Arizona
names(ua_april_maps) <- "SWE \nValue"
g2 <- ggplot() +
  stars::geom_stars(data = ua_april_maps[, , , paste0("SWE_04_01_", years[17])]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "A",
                       limits = c(0, 1200),
                       na.value = "white") +
  # labs(x = "", y = "") +
  # theme(plot.title = element_text(hjust = 0.5, size = 24),
  #       text = element_text(size = 1),
  #       plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
  #       legend.position = "none",
  #       axis.ticks = element_blank())
  theme_void() +
  theme(legend.position = "bottom")
g2

ggsave(filename = paste0("Thesis UA map for 2020", ".png"),
       plot = g2,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)

