# t <- stars::st_extract(temp, april_1_snotel_2003)
april_1_snotel_2004

# One problem is dealing with NA's
dim(april_1_snotel_2004[is.na(april_1_snotel_2004$UA_VALUE), ]) # 11 observations are NA

# What sites are NA's for UA value?
table(april_1_snotel_2004[is.na(april_1_snotel_2004$UA_VALUE), ]$NAME) # 11 observations are NA
# Burts Miller Ranch are all NA...

# Put all of the missing data points
april_1_snotel_2004$UA_VALUE[is.na(april_1_snotel_2004$UA_VALUE)] <- 0


# Calculate the SD and Medians of the Errors for each year -----------------------------


### Step 1:  Saving the Standard Deviation and Median of each Model
sd_CV_GAM_MODEL <- numeric()
sd_CV_GAM_RASTER <- numeric()
sd_CV_RF_MODEL <- numeric()
sd_CV_RF_RASTER <- numeric()
sd_CV_SNODAS <- numeric()
sd_CV_UA <- numeric()
sd_CV_DAYMET <- numeric()


# Save the Median of the errors
median_CV_GAM_MODEL <- numeric()
median_CV_GAM_RASTER <- numeric()
median_CV_RF_MODEL <- numeric()
median_CV_RF_RASTER <- numeric()
median_CV_SNODAS <- numeric()
median_CV_UA <- numeric()
median_CV_DAYMET <- numeric()

# RMSE
rmse_CV_GR <- as.numeric()
rmse_CV_RF <- as.numeric()

april_1_snotel_2004[, "CV_GAM_RASTER_PREDS"]
april_1_snotel_2004[, "CV_RF_RASTER_PREDS"]

april_1_snotel_2004[, "UA_VALUE"]
april_1_snotel_2004[, "SNODAS_VALUE"]
april_1_snotel_2004[, "DAYMET_VALUE"]

for (i in 1:(length(years))) {
  sd_CV_GAM_MODEL[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_GAM_MODEL))

  sd_CV_GAM_RASTER[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))

  sd_CV_RF_RASTER[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_RF_RASTER_PREDS))

  sd_CV_SNODAS[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$SNODAS_VALUE))


  sd_CV_UA[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$UA_VALUE))

  sd_CV_DAYMET[i] <- sd((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$DAYMET_VALUE))


  # rmse_CV_GR[i] <- ((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                        april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_GAM_RASTER_PREDS)^2) /
  #   sum(as.numeric(april_1_snotel_2003$DATE == dates[i]))
  #
  # rmse_CV_RF[i] <- ((april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$VALUE -
  #                      april_1_snotel_2003[april_1_snotel_2003$DATE == dates[i], ]$CV_RF_RASTER_PREDS)^2) /
  #   sum(as.numeric(april_1_snotel_2003$DATE == dates[i]))


  # Median's of All the different models
  median_CV_GAM_MODEL[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_GAM_MODEL))

  median_CV_GAM_RASTER[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                               april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_GAM_RASTER_PREDS))

  median_CV_RF_RASTER[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                              april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$CV_RF_RASTER_PREDS))

  median_CV_SNODAS[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$SNODAS_VALUE))

  median_CV_UA[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                       april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$UA_VALUE))

  median_CV_DAYMET[i] <- median((april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$VALUE -
                           april_1_snotel_2004[april_1_snotel_2004$DATE == dates[i], ]$DAYMET_VALUE))

}


# 10 fold CV Standard deviation time series GAM and RF Raster Visualization ----

#
df <- as.data.frame(cbind(years,
                          sd_CV_GAM_RASTER,
                          sd_CV_RF_RASTER
                          # median_UA,
                          # median_S
))

colnames(df) <- c("years", "GAM", "RF"
                  # "SNO_GAM_UA",
                  # "UA",
                  )

##### JUST the medians
ldf <- tidyr::gather(df, "GAM", "RF",
                     # "UA",
                     # "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")


# library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

# SNODAS = 2
# UA = 6
# Daymet = 8

# RF = 12
# GAM = 4

# SNODAS_GAM = 10

my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(4, 12)]

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  ggtitle("") +
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
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = my_colors) +
  # # ylim(c(-50, 210))
  scale_y_continuous(name = "Residual Value",
                     breaks = c(50, 100, 150, 200, 250),
                     limits = c(50, 250)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))


# my_colors <- RColorBrewer::brewer.pal(5, "Dark2")[3:5]
#
# ggplot(df, aes(petal.colour,Sepal.Length))+
#   geom_bar(stat = "identity",aes(fill=country))+
#   facet_wrap(~Species, ncol=3)+
#   scale_fill_manual(values = my_colors,
#                     labels = c("French Flower", "German Flower","UK Flower"))+


g

ggsave(filename = paste0("Time Series of SD of GAM and RF", ".png"),
       plot = g,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd())
       # width = 8.07,
       # height = 6.87
)

# 10 fold CV Standard deviation time series SNODAS DAYMET and UA Visualization ----

# median_CV_SNODAS <- numeric()
# median_CV_UA <- numeric()
# median_CV_DAYMET <- numeric()

# Create a data frame with the vectors of interest
df <- as.data.frame(cbind(years,
                          sd_CV_SNODAS,
                          sd_CV_DAYMET,
                          sd_CV_UA
                          # median_UA,
                          # median_S
))

df <- df[c(-18, -19), ]

colnames(df) <- c("years", "SNODAS", "Daymet", "UA"
                  # "SNO_GAM_UA",
                  # "UA",
)

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "Daymet", "UA",
                     # "UA",
                     # "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")

display.brewer.all(colorblindFriendly = TRUE)

# SNODAS = 2
# UA = 6
# Daymet = 8

# RF = 12
# GAM = 4

# SNODAS_GAM = 10


# changing the colors to be (blue, green and purple)
my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 6, 8)]

# reorder so that it follows the
ldf$`Model Type` <- factor(ldf$`Model Type`, levels = c("SNODAS", "UA", "Daymet"))

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = `Model Type`)) +
  ggtitle("Standard Deviation of Errors") +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = my_colors) +
  # # ylim(c(-50, 210))
  scale_y_continuous(name = "Residual Value",
                     breaks = c(50, 100, 150, 200, 250, 300),
                     limits = c(50, 300)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))





g

ggsave(filename = paste0("Time Series of SD Daymet SNODAS, UA", ".png"),
       plot = g,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd())
       # width = 8.07,
       # height = 6.87
)

# 10 fold CV median time series SNODAS DAYMET and UA Visualization ----

# median_CV_SNODAS <- numeric()
# median_CV_UA <- numeric()
# median_CV_DAYMET <- numeric()

# Create a data frame with the vectors of interest
df <- as.data.frame(cbind(years,
                          median_CV_SNODAS,
                          median_CV_DAYMET,
                          median_CV_UA
                          # median_UA,
                          # median_S
))

df <- df[c(-18, -19), ]

colnames(df) <- c("years", "SNODAS", "Daymet", "UA"
                  # "SNO_GAM_UA",
                  # "UA",
)

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "Daymet", "UA",
                     # "UA",
                     # "SNODAS_GAM",
                     # "SNO_GAM_UA",
                     key = "Model Type",
                     value = "residual_value")

display.brewer.all(colorblindFriendly = TRUE)

# SNODAS = 2
# UA = 6
# Daymet = 8

# RF = 12
# GAM = 4

# SNODAS_GAM = 10

# changing the colors to be (blue, green and purple)
my_colors <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 6, 8)]

# reorder so that it follows the
ldf$`Model Type` <- factor(ldf$`Model Type`, levels = c("SNODAS", "UA", "Daymet"))

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
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = my_colors) +
  # # ylim(c(-50, 210))
  scale_y_continuous(name = "Residual Value",
                     breaks = c(-50, 0, 50, 100, 150, 200, 250, 300, 350, 400),
                     limits = c(-50, 400)) +
  scale_x_continuous("Years", breaks = c(2004, 2006,
                                         2008, 2010,
                                         2012, 2014,
                                         2016, 2018,
                                         2020, 2022))





g

ggsave(filename = paste0("Time Series of Median Daymet SNODAS, UA", ".png"),
       plot = g,
       width = 12.46,
       height = 8.42,
       path = paste0(getwd())
       # width = 8.07,
       # height = 6.87
)

