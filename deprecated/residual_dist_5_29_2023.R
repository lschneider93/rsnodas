# residual distribution of 2015 and 2020 ----------------------------------
##### Trying to combine and compare the GAM and SNODAS
years[12]
years[17]
i = 12
df <- as.data.frame(cbind((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                             april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_VALUE),
                          (april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                             april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_RASTER_PREDS),
                          # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          #   april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$UA_VALUE,
                          (april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                             april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_100)
                          # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          #   april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_WEIGHTED_RASTER_PREDS,
                          # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_GAM_UA_VALUE_RESIDUALS2
))
colnames(df) <- c("SNODAS", "GAM", #"UA",
                  "SNODAS_GAM" #"SNO_GAM_UA"
)

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",#"UA",
                     "SNODAS_GAM", #"SNO_GAM_UA",
                     key = "model_type",
                     value = "residual_value")

ldf$year <- "2015"

i = 17
df2 <- as.data.frame(cbind((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_VALUE),
                           (april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_RASTER_PREDS),
                           # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                           #   april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$UA_VALUE,
                           (april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_100)
                           # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                           #   april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_WEIGHTED_RASTER_PREDS,
                           # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                           # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_GAM_UA_VALUE_RESIDUALS2
))

colnames(df2) <- c("SNODAS", "GAM", #"UA",
                   "SNODAS_GAM" #"SNO_GAM_UA"
)

##### JUST the medians
ldf2 <- tidyr::gather(df2, "SNODAS", "GAM",#"UA",
                      "SNODAS_GAM", #"SNO_GAM_UA",
                      key = "model_type",
                      value = "residual_value")

ldf2$year <- "2020"



ldf3 <- rbind(ldf, ldf2)

ldf3$model_type_factor <- factor(ldf3$model_type, level = c("SNODAS", "GAM", "SNODAS_GAM"))

g <- ggplot(ldf3, aes(y = residual_value, x = model_type_factor)) +
  geom_boxplot() +
  scale_y_continuous("Residual Value",
                     breaks = c(seq(-500, 500, 200)),
                     limits = c(-500, 500)) +
  scale_x_discrete("Model") +
  # ggtitle(paste("Residual Distribution in", years[i])) +
  #scale_color_manual(values = c("black", "blue")) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        # axis.text = element_text(size = 16),
        # plot.margin = unit(c(.5,.5,.5,.5), 'cm'),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16)) +
  # geom_hline(yintercept = 0, "dashed", "blue") +
  geom_hline(yintercept = 0,
             color = "blue",
             linetype = "dashed") +
  # ylim(c(-850, 850)) +
  coord_flip() +
  facet_wrap(~year, ncol = 1)
g

ggsave(filename = paste0("residual_boxplots_100", years[i], ".png"),
       plot = g,
       width = 8.46,
       height = 14,
       path = paste0("/Users/loganschneider/Desktop/GitHub/rsnodas"))


