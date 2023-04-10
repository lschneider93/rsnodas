# Thesis plots Water Sheds
adata <- april_1_snotel[april_1_snotel$DATE > "2003-04-01", ]
adata <- april_1_snotel[april_1_snotel$DATE == "2021-04-01", ]

################################################################################
#  Introduction plots of each type of gridded product
################################################################################
ut_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::filter(ID == "utah") %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

# Create a choropleth plot for each year 2020
try <- april_1_snotel[april_1_snotel$DATE == "2020-04-01", ] %>%
  group_by(name) %>%
  summarize(mean_bias = mean(SNODAS_VALUE - VALUE), geometry = geometry,
            median_bias = median(SNODAS_VALUE - VALUE)) %>%
  filter(row_number() == 1)

# Create a sf object that has the mean bias
try2 <- st_join(basin_simp, try)
try2$med_bias <- cut(try2$median_bias,
                     breaks = c(-300, -125, -70, -30, -10, 10, 30, 70, 125, 300),
                     include.lowest = TRUE)

# unique(try2$med_bias)

try2$m_col <- RColorBrewer::brewer.pal(11, "BrBG")[try2$med_bias]

c4 <- ggplot(try2) +
  geom_sf(aes(fill = med_bias)) +
  geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
  geom_sf(data = april_df_ghcnd_spatial[[17]]) +
  ggtitle("SNODAS Median Bias in Water Basins",
          paste("for the year", year[17])) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 16)) +
  # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
  scale_fill_manual(values = c("#dfc27d", "#f6e8c3", "#f5f5f5",
                               "#c7eae5", "#80cdc1", "#35978f")
  )
# -300 = 8c510a, -125 = bf812d, -70 = dfc27d
# -30 = f6e8c3, -10 = f5f5f5, 10 = c7eae5,
# 30 = 80cdc1, 70 = 35978f, 125 = 01665e

# All the Colors
# scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d",
#                              "#dfc27d", "#f6e8c3", "#f5f5f5",
#                              "#c7eae5", "#80cdc1", "#35978f",
#                              "#01665e", "#003c30")
# )

# scale_fill_steps(n.breaks = 8) +
# scale_fill_gradient2(low = "blue", mid = "white", high = "red",
#                      #limits = c(-400, 400),
#                      na.value = "gray") +

# theme_void()

ggsave(filename = paste0("SNODAS_Median_", year[17], ".png"),
       plot = c4,
       path = paste0(getwd(), "/figures")
)

# Create a choropleth plot for each year 2015
try <- april_1_snotel[april_1_snotel$DATE == "2015-04-01", ] %>%
  group_by(name) %>%
  summarize(mean_bias = mean(SNODAS_VALUE - VALUE), geometry = geometry,
            median_bias = median(SNODAS_VALUE - VALUE)) %>%
  filter(row_number() == 1)

# Create a sf object that has the mean bias
try2 <- st_join(basin_simp, try)

try2$med_bias <- cut(try2$median_bias,
                     breaks = c(-200, -125, -70, -30, -10,
                                10, 30, 70, 125, 200),
                     include.lowest = TRUE)

unique(try2$med_bias)

c4 <- ggplot(try2) +
  geom_sf(aes(fill = med_bias)) +
  geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
  geom_sf(data = april_df_ghcnd_spatial[[12]]) +
  ggtitle("SNODAS Median Bias in Water Basins",
          paste("for the year", year[12])) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 16)) +
  # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
  scale_fill_manual(values = c("#8c510a", "#bf812d",  "#dfc27d",
                               "#f6e8c3", "#f5f5f5",
                               "#c7eae5")
  )


ggsave(filename = paste0("SNODAS_Median_", year[12], ".png"),
       plot = c4,
       path = paste0(getwd(), "/figures")
)


# Create a choropleth plot for UA for year 2020
try <- april_df_ghcnd_spatial[[17]] %>%
  group_by(name) %>%
  summarize(mean_bias = mean(UA_RESIDUALS), geometry = geometry,
            median_bias = median(UA_RESIDUALS)) %>%
  filter(row_number() == 1)

# Create a sf object that has the mean bias
try2 <- st_join(basin_simp, try)

try2$med_bias <- cut(try2$median_bias,
                     breaks = c(-300, -200, -125, -70, -30, -10, 10, 30, 70, 125, 300),
                     include.lowest = TRUE)

unique(try2$med_bias)

try2$m_col <- RColorBrewer::brewer.pal(11, "BrBG")[try2$med_bias]

# cols <- scales::brewer_pal("div")(5)
# scales::show_col(scales::gradient_n_pal(cols)(seq(0, 1, length.out = 11)))

c4 <- ggplot(try2) +
  geom_sf(aes(fill = med_bias)) +
  geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
  geom_sf(data = april_df_ghcnd_spatial[[17]]) +
  ggtitle("UA Median Bias in Water Basins",
          paste("for the year", year[17])) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 16)) +
  # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
  scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d",
                               "#dfc27d", "#f6e8c3", "#f5f5f5",
                               "#c7eae5", "#80cdc1")
  )
# -300 = 543005, -200 = 8c510a, -125 = bf812d, -70 = dfc27d
# -30 = f6e8c3, -10 = f5f5f5, 10 = c7eae5,
# 30 = 80cdc1, 70 = 35978f, 125 = 01665e

# All the Colors
# scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d",
#                              "#dfc27d", "#f6e8c3", "#f5f5f5",
#                              "#c7eae5", "#80cdc1", "#35978f",
#                              "#01665e", "#003c30")
# )


# scale_fill_steps(n.breaks = 8) +
# scale_fill_gradient2(low = "blue", mid = "white", high = "red",
#                      #limits = c(-400, 400),
#                      na.value = "gray") +

# theme_void()

ggsave(filename = paste0("UA_Median_", year[17], ".png"),
       plot = c4,
       path = paste0(getwd(), "/figures")
)

# Create a choropleth plot for each year 2015
try <- april_df_ghcnd_spatial[[12]] %>%
  group_by(name) %>%
  summarize(mean_bias = mean(UA_RESIDUALS), geometry = geometry,
            median_bias = median(UA_RESIDUALS)) %>%
  filter(row_number() == 1)

# Create a sf object that has the mean bias
try2 <- st_join(basin_simp, try)

try2$med_bias <- cut(try2$median_bias,
                     breaks = c(-300, -200, -125, -70, -30, -10,
                                10, 30, 70, 125, 200),
                     include.lowest = TRUE)

unique(try2$med_bias)

c4 <- ggplot(try2) +
  geom_sf(aes(fill = med_bias)) +
  geom_sf(data = utstate, fill = "NA", size = 1, color = "black") +
  geom_sf(data = april_df_ghcnd_spatial[[12]]) +
  ggtitle("UA Median Bias in Water Basins",
          paste("for the year", year[12])) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 16)) +
  # scale_fill_brewer(palette = "BrBG", na.value = "grey50")
  scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d",
                               "#dfc27d", "#f6e8c3", "#f5f5f5",
                               "#c7eae5", "#80cdc1")
  )
# scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d",
#                              "#dfc27d", "#f6e8c3", "#f5f5f5",
#                              "#c7eae5", "#80cdc1", "#35978f",
#                              "#01665e", "#003c30")
# )


ggsave(filename = paste0("UA_Median_", year[12], ".png"),
       plot = c4,
       path = paste0(getwd(), "/figures")
)


