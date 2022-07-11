

names(density_map[[12]]) <- "weights"
offset_weight <- (1 - density_map[[12]])
g <- ggplot() +
  stars::geom_stars(data = offset_weight) +
  ggtitle("2015 Station Density") +
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

