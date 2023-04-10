# Making UA maps data tif object

# making UA maps data into a tif object -----------------------------------
year <- 1982:2020
base_file <- "data-raw/ua_data/4km_SWE_Depth_WY"
end_file <- "_v01.nc"

ua_us_maps <- vector("list", length(year))
"%>%" <- magrittr::"%>%"

ut_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = sf::st_crs(4326))

# get the university maps for each year, can crop to a different state...
for (i in 1:length(year)) {

  ua_map <- terra::rast(paste0(base_file, year[i], end_file))

  # Find the layer with april 1st by checking it it is a leap year
  april_1 <- ifelse(terra::nlyr(ua_map) == 730, 183, 184)
  ua_map <- terra::subset(ua_map, april_1)

  # write a tif object. note this will be deleted
  #   for some reason the terra won't let me change to the correct crs
  terra::writeRaster(ua_map, "data-raw/swe.tif", overwrite=TRUE)
  ua_map <- stars::read_stars("data-raw/swe.tif")

  ua_map <- stars::st_warp(ua_map, crs = sf::st_crs(4326))

  # crop to the state of utah
  ua_ut <- sf::st_crop(ua_map, ut_map)

  # store in a  list
  ua_us_maps[[i]] <- ua_ut # ua_map
}

# go through the list of maps and combine to a stars stack
for (i in 2:length(ua_us_maps)) {
  # Combine/Concatenate the first 2 maps
  if (i == 2) {
    combined_ua <- c(ua_us_maps[[(i - 1)]], ua_us_maps[[i]])
  } else {
    # Combine the later maps
    combined_ua <- c(combined_ua, ua_us_maps[[i]])
  }
}

# Rename each object to contain the dates by creating a vector of SWE_MM_DD_YYYY
a <- vector("character", length(combined_ua))
for (i in 1:length(combined_ua)) {
  a[i] <- paste0("SWE_04_01_", year[i])
}

# Renaming and combine
names(combined_ua) <- a
combined_ua_us <- merge(combined_ua)
combined_ua_us <- stars::st_set_dimensions(combined_ua_us,
                                           names = c("x", "y", "time"))

# Write as an object
# stars::write_stars(combined_ua_us, "data/ua_april1.tif")
stars::write_stars(combined_ua_us, "data-raw/ua_ut_april1.tif")
# usethis::use_data(utah2, overwrite = TRUE)






















# new ---------------------------------------------------------------------

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

colnames(df) <- c("years", "GAM", "SNO_GAM",
                  # "SNO_GAM_UA",
                  # "UA",
                  "SNODAS")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNO_GAM",
                     # "SNO_GAM_UA",
                     key = "model_type",
                     value = "residual_value")

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = model_type)) +
  ggtitle("Time Series of Median of Errors") +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        text = element_text(size = 20)) +
  ylab("Residual Value") +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("royalblue", "tan4",
                                # "lightsalmon2",
                                "gray0", "green4")) # +
# ylim(c(-50, 210))
g
# "gray0", "springgreen4", lightsalmon2, green4
# scale_color_brewer() +
# theme_bw()

g

ggsave(filename = paste0("Time Series of Median of Errors_100", ".png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures")
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

colnames(df) <- c("years", "GAM", "SNO_GAM",
                  #"UA",
                  "SNODAS" )#, "SNO_GAM_UA")

##### JUST the medians
ldf <- tidyr::gather(df, "SNODAS", "GAM",
                     # "UA",
                     "SNO_GAM",
                     # "SNO_GAM_UA",
                     key = "model_type",
                     value = "residual_value")

# Website: https://ggplot2.tidyverse.org/reference/scale_brewer.html
g <- ggplot(ldf, aes(y = residual_value, x = years, color = model_type)) +
  geom_line(size = .9) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        text = element_text(size = 20)) +
  ggtitle("Time Series of SD of Errors") +
  ylab("Residual Value") +
  # ylim(c(-25, 210)) +
  scale_color_manual(values = c("royalblue", "tan4",
                                #"lightsalmon2",
                                "gray0", "green4")) # +
# ylim(c(-25, 210))
g

ggsave(filename = paste0("TimeSeries_Median_comparison_100.png"),
       plot = g,
       width = 8.46,
       height = 8.42,
       path = paste0(getwd(), "/figures"))


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

  ldf3 <- rbind(ldf, ldf2)

  ldf3$year <- c(rep("2015", 396), rep("2020", 390))

  g <- ggplot(ldf3, aes(y = residual_value, x = model_type)) +
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
          axis.text.y = element_text(size = 14)) +
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


# variogram ---------------------------------------------------------------

# other things to try:
# Convert SNODAS and see if they are spatially correlated? (Create a Variogram)

#### Step 0: Get all of the rasters and Convert to a spatial points data frame
april_1_snotel
april_1_snotel_2003_2 <- april_1_snotel[april_1_snotel$DATE > "2003-04-01", ]
april_1_snotel_2003
table(april_1_snotel_2003$DATE)
table(april_1_snotel_2003_2$DATE)

library(gstat)

y15 <- april_1_snotel_2003[april_1_snotel_2003$DATE == "2015-04-01", ]

# make it a spatial points
sf_y15 <- sf::st_as_sf(y15,
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = sf::st_crs(snodas_april_maps[[1]]))
sf_y15_2 <- sf::st_as_sf(y15_2, coords = c("LONGITUDE", "LATITUDE"))

sf_y15$DIFF <- sf_y15$VALUE - sf_y15$SNODAS_VALUE

# Create a Variogram
gstat_variogram <- gstat::variogram(DIFF ~ 1, data = sf_y15)

gstat_variogram2 <- gstat::variogram(log(VALUE+1) ~ ELEVATION, data = sf_y15)
plot(gstat_variogram, main = "Variogram of SNODAS errors")
plot(gstat_variogram2, main = "Variogram using Elevation")

# Fit a variogram of a bunch of different models
#    *NOTE: No Convergence
variogram.fit <- gstat::fit.variogram(gstat_variogram,
                                      model = gstat::vgm(c("Sph", "Exp", "Gau")))
variogram.fit
plot(gstat_variogram, variogram.fit, main = "fitting variogram for SNODAS")


citation("gstat")



