
# Shared UA and SNODAS graphic --------------------------------------------

# combined SNODAS and UA map (5-20-2023) (5/29/2023) ----------------------------------
# names(ua_april_maps) <- "SWE \nValue"
names(snodas_april_maps[[12]]) <- "SWE \nValue"
snodas_april_maps[[12]]

plot(snodas_april_maps[[12]])

g <- ggplot() +
  stars::geom_stars(data = snodas_april_maps[[12]]) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = NA, size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  # scale_fill_continuous(na.value = "white") +
  scale_fill_viridis_c(option = "A", limits = c(0, 750), na.value = "white") +
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
names(ua_april_maps) <- "SWE \nValue"
g2 <- ggplot() +
  stars::geom_stars(data = ua_april_maps[, , , paste0("SWE_04_01_", years[12])]) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "A", limits = c(0, 750), na.value = "white") +
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
  stars::geom_stars(data = snodas_april_maps[[12]]) +
  # ggtitle(paste("2015 SNODAS SWE predictions")) +
  geom_sf(data = ut_map, fill = "NA", size = 1, color = "blue") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(option = "A", limits = c(0, 750)) +
  # labs(x = "", y = "") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(0.85, "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
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



ggsave(filename = paste0("Thesis UA map for 2015", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)

ggsave(filename = paste0("combined_map", ".png"),
       plot = plots_together,
       width = 8.46,
       height = 8.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub")
)


# Trying stack overflow ---------------------------------------------------

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend < -g_legend(g)

p3 <- grid.arrange(arrangeGrob(g + theme(legend.position="none"),
                               g2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

