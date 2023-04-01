# SNODAS map
# SNODAS
names(snodas_april_maps[[12]]) <- "SWE \nValue"
snodas_april_maps[[12]]
g2 <- ggplot() +
  stars::geom_stars(data = snodas_april_maps[[12]]) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_fill_viridis_c(option = "A", limits = c(0, 750)) +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        text = element_text(size = 28),
        legend.title = element_text(size = 20)) + #,
  # legend.position="bottom"
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

g2
ggsave(filename = paste0("Thesis_SNODAS_NOV_26_2022", ".png"),
       plot = g2,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd())
)

g
ggsave(filename = paste0("Thesis SNODAS map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)




max(snodas_april_maps[[12]])
