names(rf_raster[[12]]) <- "Value"
g <- ggplot() +
  stars::geom_stars(data = rf_raster[[12]]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  # ggtitle("2015 GAM SWE predictions") +
  scale_fill_viridis_c(option = "A", limits = c(0, 750)) +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        text = element_text(size = 28),
        legend.title = element_text(size = 28)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
g


ggsave(filename = paste0("Thesis_RF_NOV_20_2022", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd())
       # width = 8.07,
       # height = 6.87
)
