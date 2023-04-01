# t <- stars::st_extract(temp, april_1_snotel_2003)
april_1_snotel_2003 <- april_1_snotel[(april_1_snotel$DATE > "2003-04-01") &
                          (april_1_snotel$DATE < "2022-05-01"), ]

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

april_1_snotel_2003[, "CV_GAM_RASTER_PREDS"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_50_50"]
april_1_snotel_2003[, "CV_GAM_SNODAS_50_50"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_60_40"]
april_1_snotel_2003[, "CV_GAM_SNODAS_60_40"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_70_30"]
april_1_snotel_2003[, "CV_GAM_SNODAS_70_30"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_80_20"]
april_1_snotel_2003[, "CV_GAM_SNODAS_80_20"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_90_10"]
april_1_snotel_2003[, "CV_GAM_SNODAS_90_10"]
# april_1_snotel_2003[, "CV_GAM_SNODAS_UA_100"]
april_1_snotel_2003[, "CV_GAM_SNODAS_100"]

# One problem is dealing with NA's
dim(april_1_snotel_2003[is.na(april_1_snotel_2003$UA_VALUE), ]) # 141 observations are NA

# What sites are NA's for UA value?
table(april_1_snotel_2003[is.na(april_1_snotel_2003$UA_VALUE), ]$NAME) # 141 observations are NA

# table(april_1_snotel_2003$CV_GAM_RASTER_PREDS == april_1_snotel_2003$CV_GAM_RASTER_COMB_PREDS)
# april_1_snotel_2003$GAM_PREDS
# table(april_1_snotel_2003$CV_GAM_RASTER_PREDS == NA)
# table(is.na(april_1_snotel_2003$CV_GAM_RASTER_COMB_PREDS))


for (i in 1:(length(years))) {
  sd_CV_GM[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                      april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_MODEL))

  sd_CV_GR[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                      april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))



  sd_CV_S_G_50[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_50_50))
  # sd_CV_S_G_UA_50[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                         april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_50_50)
  sd_CV_S_G_60[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_60_40))
  # sd_CV_S_G_UA_60[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                           april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_60_40)
  sd_CV_S_G_70[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_70_30))
  # sd_CV_S_G_UA_70[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                             # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_70_30)
  sd_CV_S_G_80[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_80_20))
  # sd_CV_S_G_UA_80[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_80_20)
  sd_CV_S_G_90[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_90_10))
  # sd_CV_S_G_UA_90[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_90_10)
  sd_CV_S_G_100[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_100))
  # sd_CV_S_G_UA_100[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_100)

  # sd_UA[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                  april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$UA_VALUE))
  sd_S[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                  april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_VALUE))
  # sd_F_G[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                   april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_GAM_VALUE)
  # sd_F_G_S[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                     april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_GAM_SNODAS_VALUE))
  # sd_F_G_S_UA[i] <- sd((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                        april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_COMB_VALUE)

  median_CV_GM[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_MODEL))
  median_CV_GR[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))

  median_CV_S_G_50[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_50_50))
  # median_CV_S_G_UA_50[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_50_50)
  median_CV_S_G_60[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_60_40))
  # median_CV_S_G_UA_60[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_60_40)
  median_CV_S_G_70[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_70_30))
  # median_CV_S_G_UA_70[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_70_30)
  median_CV_S_G_80[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_80_20))
  # median_CV_S_G_UA_80[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_80_20)
  median_CV_S_G_90[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_90_10))
  # median_CV_S_G_UA_90[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                            april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_90_10)
  median_CV_S_G_100[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_100))
  # median_CV_S_G_UA_100[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                             april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_SNODAS_UA_100)


  # median_UA[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$UA_VALUE)
  median_S[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                          april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$SNODAS_VALUE))
  # median_F_G[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                           april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_GAM_VALUE)
  # median_F_G_S[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                             april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_GAM_SNODAS_VALUE)
  # median_F_G_S_UA[i] <- median((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
                                 # april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$FULL_COMB_VALUE)
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

median(df$SNODAS)
median(df$GAM)
median(df$SNODAS_GAM)

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")


library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  ggtitle("Median of Errors") +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +
  xlab("") + ylab("") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank()) +
  ylab("Residual Value") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(name = "Residual Value",
                     breaks = c(-10, 0, 10, 20, 30, 40, 50),
                     limits = c(-10, 50)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))

# scale_color_manual(values = c("royalblue", "tan4",
  #                               # "lightsalmon2",
  #                               "gray0", "green4")) # +
  # # ylim(c(-50, 210))
g
  # "gray0", "springgreen4", lightsalmon2, green4
  # scale_color_brewer() +
  # theme_bw()

g

ggsave(filename = paste0("Time Series of Median of Errors_100", ".png"),
       plot = g,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd())
       # width = 8.07,
       # height = 6.87
)

################################################################################
################################################################################
################################################################################

df <- as.data.frame(cbind(years,
                          sd_CV_GR, # length(sd_CV_GR)
                          sd_CV_S_G_100, # length(sd_CV_S_G)
                          # sd_UA, # length(sd_UA)
                          sd_S # length(sd_S)
))

colnames(df) <- c("years", "GAM", "SNODAS_GAM",
                  #"UA",
                  "SNODAS" )#, "SNO_GAM_UA")

median(df$SNODAS)
median(df$GAM)
median(df$SNODAS_GAM)

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  geom_line(size = .9) +
  ggtitle("Standard Deviation of Errors") +
  # ylab("Residual Value") +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank()) +
  ylab("Residual Value") +
  scale_color_brewer(palette = "Dark2") +
  # xlab("") + ylab("") +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))
g

ggsave(filename = paste0("TimeSeries_SD_comparison_100.png"),
       plot = g,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd()))


# Comparing the residual distribution of each.

##### Trying to combine and compare the GAM and SNODAS
for (i in 1:(length(years))) {
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
                    "SNO_GAM" #"SNO_GAM_UA"
                    )

  ##### JUST the medians
  ldf <- tidyr::gather(df, "SNODAS", "GAM",#"UA",
                       "SNO_GAM", #"SNO_GAM_UA",
                       key = "model_type",
                       value = "residual_value")

  g <- ggplot(ldf, aes(y = residual_value, x = model_type)) +
    geom_boxplot() +
    ggtitle(paste("Residual Distribution in", years[i])) +
    #scale_color_manual(values = c("black", "blue")) +
    theme(plot.title = element_text(hjust = 0.5, size = 30),
          axis.text = element_text(size = 28),
          # plot.margin = unit(c(.5,.5,.5,.5), 'cm'),
          axis.title = element_text(size = 32)) +
    geom_hline(yintercept = 0) +
    ylim(c(-850, 850)) +
    coord_flip()
  g

  ggsave(filename = paste0("residual_boxplots_100", years[i], ".png"),
         plot = g,
         width = 12.46,
         height = 8.42,
         path = paste0(getwd(), "/figures"))

}


#
df <- as.data.frame(cbind(years[-19],
                          median_CV_GR,
                          median_CV_S_G_50,
                          # median_CV_S_G_60,
                          # median_CV_S_G_80,
                          median_CV_S_G_100,
                          median_S
))

colnames(df) <- c("years", "GAM",
                  "SNO_GAM_50",
                  #"SNO_GAM_60",
                  #"SNO_GAM_80",
                  "SNO_GAM_100",
                  # "SNO_GAM_UA", "UA",
                  "SNODAS")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "SNO_GAM_60",
                     "SNO_GAM_50",
                     "SNO_GAM_100",
                     # "SNO_GAM_UA",
                     key = "model_type",
                     value = "residual_value")

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = model_type)) +
  ggtitle("Time Series of Median of Errors") +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        text = element_text(size = 20)) +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("royalblue",
                                "lightsalmon2", #"green4",
                                "tan4",
                                "gray0")) # +
# ylim(c(-50, 210))
g
# "gray0", "springgreen4", lightsalmon2, green4
# scale_color_brewer() +
# theme_bw()

ggsave(filename = paste0("Thesis Time Series of Median of Errors2", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
       # width = 8.07,
       # height = 6.87
)

# Standard Deviations Time Series plots
df <- as.data.frame(cbind(years,
                          sd_CV_GR,
                          sd_CV_S_G_50,
                          # median_CV_S_G_60,
                          # sd_CV_S_G_80,
                          sd_CV_S_G_100,
                          sd_S
))

colnames(df) <- c("years", "GAM", "SNO_GAM_50",
                  # "SNO_GAM_60",
                  # "SNO_GAM_80",
                  "SNO_GAM_100",
                  # "SNO_GAM_UA", "UA",
                  "SNODAS")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM", "SNO_GAM_50",
                     #"SNO_GAM_60",
                     #"SNO_GAM_80",
                     "SNO_GAM_100",
                     # "SNO_GAM_UA",
                     key = "model_type",
                     value = "residual_value")

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
# https://r-graph-gallery.com/index.html
# https://www.datanovia.com/en/blog/awesome-list-of-657-r-color-names/
g <- ggplot(ldf, aes(y = residual_value, x = years, color = model_type)) +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 20)) +
  ggtitle("Time Series of SD of Errors") +
  ylab("Median Residual Value") +
  scale_color_manual(values = c("royalblue",
                                "lightsalmon2", #"green4",
                                "tan4",
                                "gray0")) # +
# ylim(c(-25, 210))
g

ggsave(filename = paste0("Thesis_TimeSeries_SD_comparison2.png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures"))


