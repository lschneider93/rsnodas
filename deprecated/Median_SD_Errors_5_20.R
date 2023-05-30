# t <- stars::st_extract(temp, april_1_snotel_2003)
# /Users/loganschneider/Desktop/GitHub/rsnodas_04-02-2023
april_1_snotel_2003a <- read.csv("/Users/loganschneider/Desktop/GitHub/rsnodas_04-02-2023/deprecated_04-02-2023/station_info_123.csv")

april_1_snotel_2003b <- april_1_snotel_2003a[april_1_snotel_2003a$DATE > "2003-04-01", ]

# april_1_snotel <- read.csv("deprecated/station_info_123_2.csv")
# april_1_snotel <- read.csv("deprecated/station_info_1234.csv")


# colnames(adata)
# adata <- adata[, c(1, 2, 8, 9, 10, 11:30)]
# april_1_snotel$CV_GAM_RASTER_COMB_PREDS2 <- april_1_snotel$CV_GAM_RASTER_COMB_PREDS
# april_1_snotel$CV_GAM_RASTER_COMB_PREDS2 <- ifelse(is.na(april_1_snotel$CV_GAM_RASTER_COMB_PREDS2) == TRUE,
#                                                    0.01, april_1_snotel$CV_GAM_RASTER_COMB_PREDS2)

### Step 4:  Saving the Standard Deviation and
sd_CV_GM <- numeric()
sd_CV_GR <- numeric()
sd_CV_S_G_50 <- numeric()
# sd_CV_S_G_UA_50 <- numeric()
sd_CV_S_G_60 <- numeric()
# sd_CV_S_G_UA_60 <- numeric()
sd_CV_S_G_70 <- numeric()
# sd_CV_S_G_UA_70 <- numeric()
sd_CV_S_G_80 <- numeric()
# sd_CV_S_G_UA_80 <- numeric()
# sd_CV_S_G_UA_90 <- numeric()
sd_CV_S_G_90 <- numeric()
# sd_CV_S_G_UA_100 <- numeric()
sd_CV_S_G_100 <- numeric()
# sd_UA <- numeric()
sd_S <- numeric()
# sd_F_G <- numeric()
# sd_F_G_S <- numeric()
# sd_F_G_S_UA <- numeric()

# sd_S_G_UA2 <- numeric()

median_CV_GM <- numeric()
median_CV_GR <- numeric()
median_CV_S_G_50 <- numeric()
# median_CV_S_G_UA_50 <- numeric()
median_CV_S_G_60 <- numeric()
# median_CV_S_G_UA_60 <- numeric()
median_CV_S_G_70 <- numeric()
# median_CV_S_G_UA_70 <- numeric()
median_CV_S_G_80 <- numeric()
# median_CV_S_G_UA_80 <- numeric()
# median_CV_S_G_UA_90 <- numeric()
median_CV_S_G_90 <- numeric()
# median_CV_S_G_UA_100 <- numeric()
median_CV_S_G_100 <- numeric()
# median_UA <- numeric()
median_S <- numeric()
# median_F_G <- numeric()
# median_F_G_S <- numeric()
# median_F_G_S_UA <- numeric()

april_1_snotel_2003b[, "CV_GAM_RASTER_PREDS"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_50_50"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_50_50"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_60_40"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_60_40"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_70_30"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_70_30"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_80_20"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_80_20"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_90_10"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_90_10"]
# april_1_snotel_2003b[, "CV_GAM_SNODAS_UA_100"]
april_1_snotel_2003b[, "CV_GAM_SNODAS_100"]

# One problem is dealing with NA's
dim(april_1_snotel_2003b[is.na(april_1_snotel_2003b$UA_VALUE), ]) # 141 observations are NA

# What sites are NA's for UA value?
table(april_1_snotel_2003b[is.na(april_1_snotel_2003b$UA_VALUE), ]$NAME) # 141 observations are NA

# table(april_1_snotel_2003b$CV_GAM_RASTER_PREDS == april_1_snotel_2003b$CV_GAM_RASTER_COMB_PREDS)
# april_1_snotel_2003b$GAM_PREDS
# table(april_1_snotel_2003b$CV_GAM_RASTER_PREDS == NA)
# table(is.na(april_1_snotel_2003b$CV_GAM_RASTER_COMB_PREDS))


for (i in 1:(length(years))) {
  sd_CV_GM[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_MODEL))

  sd_CV_GR[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))

  sd_CV_S_G_50[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_50_50))
  # sd_CV_S_G_UA_50[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                         april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_50_50)
  sd_CV_S_G_60[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_60_40))
  # sd_CV_S_G_UA_60[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_60_40)
  sd_CV_S_G_70[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_70_30))
  # sd_CV_S_G_UA_70[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  # april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_70_30)
  sd_CV_S_G_80[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_80_20))
  # sd_CV_S_G_UA_80[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_80_20)
  sd_CV_S_G_90[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_90_10))
  # sd_CV_S_G_UA_90[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_90_10)
  sd_CV_S_G_100[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_100))
  # sd_CV_S_G_UA_100[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_100)

  # sd_UA[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                  april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$UA_VALUE))
  sd_S[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$SNODAS_VALUE))
  # sd_F_G[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_GAM_VALUE)
  # sd_F_G_S[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                     april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_GAM_SNODAS_VALUE))
  # sd_F_G_S_UA[i] <- sd((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                        april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_COMB_VALUE)

  median_CV_GM[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                               april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_MODEL))
  median_CV_GR[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                               april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))

  median_CV_S_G_50[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_50_50))
  # median_CV_S_G_UA_50[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_50_50)
  median_CV_S_G_60[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_60_40))
  # median_CV_S_G_UA_60[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_60_40)
  median_CV_S_G_70[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_70_30))
  # median_CV_S_G_UA_70[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_70_30)
  median_CV_S_G_80[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_80_20))
  # median_CV_S_G_UA_80[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_80_20)
  median_CV_S_G_90[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                   april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_90_10))
  # median_CV_S_G_UA_90[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_90_10)
  median_CV_S_G_100[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                                    april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_100))
  # median_CV_S_G_UA_100[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                             april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$CV_GAM_SNODAS_UA_100)


  # median_UA[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                          april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$UA_VALUE)
  median_S[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$SNODAS_VALUE))
  # median_F_G[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                           april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_GAM_VALUE)
  # median_F_G_S[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  #                             april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_GAM_SNODAS_VALUE)
  # median_F_G_S_UA[i] <- median((april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$VALUE -
  # april_1_snotel_2003b[april_1_snotel_2003b$DATE == dates[i], ]$FULL_COMB_VALUE)
}

#
df <- as.data.frame(cbind(years,
                          median_CV_GR,
                          median_CV_S_G_100,
                          #median_UA,
                          median_S
))

colnames(df) <- c("years", "GAM", "SNODAS_GAM",
                  # "SNO_GAM_UA",
                  # "UA",
                  "SNODAS")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")

# changing the colors to be (blue, green and purple)
my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 4, 10)]

# reorder so that it follows the
ldf$`Model Type` <- factor(ldf$`Model Type`, levels = c("SNODAS", "GAM", "SNODAS_GAM"))

display.brewer.all(colorblindFriendly = TRUE)


# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  # ggtitle("Median of Errors") +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        text = element_text(size = 26),
        legend.title = element_text(size = 20),
        legend.position = "bottom") +
  xlab("") + ylab("") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank()) +
  ylab("Residual Value") +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(name = "Value",
                     breaks = c(-10, 0, 10, 20, 30, 40, 50),
                     limits = c(-10, 50)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))

g


# ggsave(filename = paste0("Time_Series__Median_Errors_SNODAS_GAM_SNOGAM", ".png"),
#        plot = g,
#        width = 12.46,
#        height = 8.42,
#        path = paste0(getwd())
#        # width = 8.07,
#        # height = 6.87
# )

df <- as.data.frame(cbind(years,
                          sd_CV_GR, # length(sd_CV_GR)
                          sd_CV_S_G_100, # length(sd_CV_S_G)
                          # sd_UA, # length(sd_UA)
                          sd_S # length(sd_S)
))

colnames(df) <- c("years", "GAM", "SNODAS_GAM",
                  #"UA",
                  "SNODAS" )#, "SNO_GAM_UA")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")

# SNODAS = 2
# UA = 6
# Daymet = 8

# RF = 12
# GAM = 4

# SNODAS_GAM = 10

# changing the colors to be (blue, green and purple)
my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 4, 10)]

# reorder so that it follows the
ldf$`Model Type` <- factor(ldf$`Model Type`, levels = c("SNODAS", "GAM", "SNODAS_GAM"))

display.brewer.all(colorblindFriendly = TRUE)


# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g2 <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  geom_line(size = .9) +
  # ggtitle("Standard Deviation of Errors") +
  # ylab("Residual Value") +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        text = element_text(size = 26),
        legend.title = element_text(size = 20)) +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank()) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(name = "Value",
                     breaks = c(seq(50, 250, 50)),
                     limits = c(50, 250)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022)) +
  # guides(color = FALSE)
  guides(color = "none")

g2


library(gridExtra)
library(cowplot)
# combine the two plots side by side
combined_plot <- grid.arrange(g, g2, ncol = 1, heights = c(0.55, 0.45))

ggsave(filename = paste0("Time_Series__Median_Errors_SNODAS_GAM_SNOGAM", ".png"),
       plot = combined_plot,
       # width = 12.46,
       width = 8.42,
       height = 9.42,
       path = paste0("/Users/loganschneider/Desktop/GitHub/rsnodas")
       # width = 8.07,
       # height = 6.87
)



