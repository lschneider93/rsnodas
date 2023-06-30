# Crop the map to just the state of utah and name the values to be "SWE Value"
comb_map <- sf::st_crop(comb_map, ut_map)
names(comb_map) <- "SWE\nValue"


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

names(snodas_april_maps[[17]]) <- "SWE \nValue"
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



# Creating a Map using GAM and Density of 2020 ------------------------------------------------

gam_raster[[17]]


names(gam_raster[[17]]) <- "SWE\nValue\n(mm)"
gam_raster[[17]]

plot(gam_raster[[17]])

# crop
gam_utah <- sf::st_crop(gam_raster[[17]], ut_map)
names(gam_utah) <- "SWE\nValue\n(mm)"

g <- ggplot() +
  stars::geom_stars(data = gam_utah) +
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
  theme(legend.position = "none")


g


# UNIVERSITY of Arizona
names(density_map[[17]]) <- "Weights"

# crop
density_utah <- sf::st_crop(density_map[[17]], ut_map)
names(density_utah) <- "Weights"


g2 <- ggplot() +
  stars::geom_stars(data = density_utah) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "D",
                       limits = c(0, 1),
                       na.value = "white") +
  # labs(x = "", y = "") +
  # theme(plot.title = element_text(hjust = 0.5, size = 24),
  #       text = element_text(size = 1),
  #       plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
  #       legend.position = "none",
  #       axis.ticks = element_blank())
  theme_void() +
  theme(legend.position = "none")
g2


g3 <- ggplot() +
  stars::geom_stars(data = gam_utah*density_utah) +
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
  theme(legend.position = "none")
g3

# combine the plots
comb_thesis_plots <- grid.arrange(g, g2, g3, ncol = 3)


# shared legend
g_legend <- ggplot() +
  stars::geom_stars(data = gam_utah) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "A", limits = c(0, 1200)) +
  # labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(0.85, "cm"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  guides(colour = guide_legend(title.position = "top", label.position = "top",
                               title.hjust = 0.5, label.hjust = 0.5,
                               keywidth = unit(1, "npc"), keyheight = unit(1, "lines"),
                               default.unit = "lines"))

g_legend
# theme(legend.position = "bottom",
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 14))  # negative bottom margin to remove empty space


tmp <- ggplotGrob(g_legend)
legend <- tmp$grobs[[which(tmp$layout$name == "guide-box")]]

# shared legend
g_legend2 <- ggplot() +
  stars::geom_stars(data = density_utah) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "D", limits = c(0, 1)) +
  # labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(0.85, "cm"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  guides(colour = guide_legend(title.position = "top", label.position = "top",
                               title.hjust = 0.5, label.hjust = 0.5,
                               keywidth = unit(1, "npc"), keyheight = unit(1, "lines"),
                               default.unit = "lines"))

g_legend2
# theme(legend.position = "bottom",
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 14))  # negative bottom margin to remove empty space


tmp2 <- ggplotGrob(g_legend2)
legend2 <- tmp2$grobs[[which(tmp2$layout$name == "guide-box")]]

comb_legend <- grid.arrange(legend, legend2, ncol = 2)


plots_together <- grid.arrange(comb_thesis_plots, comb_legend,
                               nrow = 2,
                               heights = c(10, 1))

# grid.arrange(g, g2, legend,
#              ncol = 2,
#              heights = c(4, 4, 0.5))

ggsave(filename = paste0("GAM_Density_6_13", ".png"),
       plot = plots_together,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)

# Combined Maps ----------------------------------

names(snodas_april_maps[[17]]) <- "SWE\nValue\n(mm)"
snodas_april_maps[[12]]

plot(snodas_april_maps[[12]])

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
  theme(legend.position = "none")


g


# UNIVERSITY of Arizona
names(ua_april_maps) <- "SWE\nValue\n(mm)"
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
  theme(legend.position = "none")
g2


# combine the plots
comb_thesis_plots <- grid.arrange(g, g2, ncol = 2)


# shared legend
g_legend <- ggplot() +
  stars::geom_stars(data = snodas_april_maps[[17]]) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "A", limits = c(0, 1200)) +
  # labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(0.85, "cm"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  guides(colour = guide_legend(title.position = "top", label.position = "top",
                               title.hjust = 0.5, label.hjust = 0.5,
                               keywidth = unit(1, "npc"), keyheight = unit(1, "lines"),
                               default.unit = "lines"))

g_legend
# theme(legend.position = "bottom",
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 14))  # negative bottom margin to remove empty space


tmp <- ggplotGrob(g_legend)
legend <- tmp$grobs[[which(tmp$layout$name == "guide-box")]]



plots_together <- grid.arrange(comb_thesis_plots, legend,
                               nrow = 2,
                               heights = c(10, 1))

# grid.arrange(g, g2, legend,
#              ncol = 2,
#              heights = c(4, 4, 0.5))

ggsave(filename = paste0("SNODAS_UA_6_13", ".png"),
       plot = plots_together,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)



