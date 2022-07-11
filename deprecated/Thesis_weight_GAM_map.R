
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
