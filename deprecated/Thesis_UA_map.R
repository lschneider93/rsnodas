# UNIVERSITY of Arizona
names(ua_april_maps) <- "SWE \nValue"
g2 <- ggplot() +
  stars::geom_stars(data = ua_april_maps[, , , paste0("SWE_04_01_", years[12])]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  # ggtitle(paste("2015 UA SWE predictions")) +
  scale_fill_viridis_c(option = "A", limits = c(0, 750)) +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        text = element_text(size = 28),
        legend.title = element_text(size = 20)) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



g2
ggsave(filename = paste0("Thesis_UA_NOV_20_2022", ".png"),
       plot = g2,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd())
)

g
ggsave(filename = paste0("Thesis UA map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
)
