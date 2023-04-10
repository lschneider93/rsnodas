### Step 3: Combine then compare boxplots for every year
# for (i in 1:length(years)) {
testdata <- data.frame(cbind(
  april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")] -
    april_1_snotel_2003$CV_GAM_RASTER_PREDS[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")],
  april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")] -
    april_1_snotel_2003$SNODAS_VALUE[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")],
  april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")] -
    april_1_snotel_2003$CV_GAM_SNODAS_100[april_1_snotel_2003$DATE == paste0(years[12], "-04-01")]
)
)

colnames(testdata) <- c("GAM", "SNODAS", "SNODAS_GAM")

ldf <- tidyr::gather(testdata, "SNODAS", "GAM", "SNODAS_GAM",
                     key = "Model Type",
                     value = "Model Value")

# level(ldf$`Model Type`)

# Website:
#   https://ggplot2.tidyverse.org/reference/scale_brewer.html
#   https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
p <- ldf %>%
  mutate(name = forcats::fct_relevel(`Model Type`,
                                     "SNODAS_GAM", "GAM", "SNODAS")) %>%
  ggplot( aes(x = name, y = `Model Value`)) +
  geom_boxplot() +
  ggtitle(paste0(years[12], "'s Residual Distribution ")) +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +#, legend.position = "bottom") +
  ylab(paste0("Error Value")) + xlab("") +
  geom_hline(yintercept=0, linetype="dashed",
             color = "blue", size=1.) +
  # scale_y_continuous(limits = c(-500, 500)) +
  scale_y_continuous("Residuals \n (Value - Prediction)", breaks = c(-500, -400, -300, -200,
                                         -100, 0, 100, 200, 300, 400, 500), limits = c(-500, 500)) +
  coord_flip()
# xlab("")
p

ggsave(filename = paste0("Residual Distribution for ", years[12], ".png"),
       plot = p,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd())
)

# }














  # for (i in 1:length(years)) {
  testdata <- data.frame(cbind(
    april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")] -
      april_1_snotel_2003$CV_GAM_RASTER_PREDS[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")],
    april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")] -
      april_1_snotel_2003$SNODAS_VALUE[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")],
    april_1_snotel_2003$VALUE[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")] -
      april_1_snotel_2003$CV_GAM_SNODAS_100[april_1_snotel_2003$DATE == paste0(years[17], "-04-01")]
  )
  )
  #bmod9[[i]]$residuals)
  colnames(testdata) <- c("GAM", "SNODAS", "SNODAS_GAM")

  ldf <- tidyr::gather(testdata, "SNODAS", "GAM", "SNODAS_GAM",
                       key = "Model Type",
                       value = "Model Value")

  # level(ldf$`Model Type`)

  # Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
  # g <- ggplot(ldf, aes(y = `Model Value`, x = reorder(`Model Type`, `Model Value`))) +
  #   geom_boxplot() +
  #   # scale_color_manual(values=c("#FFAA1D", "#3D8C40", "#0088CC")) +
  #   # scale_color_manual(values=c("#FFAA1D", "#0088CC", "#3D8C40")) +
  #   # scale_fill_manual(values=c("#B34E24", "#3D8C40", "#0088CC")) +
  #   # scale_fill_brewer(palette = "Dark2") +
  #   ggtitle(paste0("Residual Distribution for ")) +
  #   theme(plot.title = element_text(hjust = 0.5, size = 20),
  #         text = element_text(size = 20)) +#, legend.position = "bottom") +
  #   ylab(paste0("Error Value")) +
  #   coord_flip()
  # # scale_fill_brewer(direction = -1) +
  # # theme_bw()
  # # theme_gray()

  # https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
  p <- ldf %>%
    mutate(name = forcats::fct_relevel(`Model Type`,
                              "SNODAS_GAM", "GAM", "SNODAS")) %>%
    ggplot( aes(x = name, y = `Model Value`)) +
    geom_boxplot() +
    ggtitle(paste0(years[17], "'s Residual Distribution")) +
    theme(plot.title = element_text(hjust = 0.5, size = 30),
          text = element_text(size = 28),
          legend.title = element_text(size = 32)) + #, legend.position = "bottom") +
    ylab(paste0("Error Value")) + xlab("") +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "blue", size=1.) +
    scale_y_continuous("Residuals \n (Value - Prediction)", breaks = c(-500, -400, -300, -200,
                                                                       -100, 0, 100, 200, 300, 400, 500), limits = c(-500, 500)) +
    coord_flip()
  # xlab("")
  p

  ggsave(filename = paste0("Residual Distribution for ", years[17], ".png"),
         plot = p,
         width = 12.46,
         height = 8.42,
         path = paste0(getwd())
  )

  # }
