#
names(comb_ua_snodas_maps[[12]]) <- "Pred"
g <- ggplot() +
  stars::geom_stars(data = comb_ua_snodas_maps[[12]]) +
  ggtitle("SNODAS and UA SWE predictions") +
  scale_fill_viridis_c(option = "A") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        text = element_text(size = 16))

ggsave(filename = paste0("SNODAS and UA map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

