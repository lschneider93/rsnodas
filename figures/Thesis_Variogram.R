# other things to try:
# Convert SNODAS and see if they are spatially correlated? (Create a Variogram)

#### Step 0: Get all of the rasters and Convert to a spatial points data frame
april_1_snotel
april_1_snotel_2003_2 <- april_1_snotel_data[april_1_snotel_data$DATE > "2003-04-01", ]
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
gstat_variogram2 <- gstat::variogram(VALUE ~ ELEVATION, data = sf_y15)
plot(gstat_variogram, main = "Variogram of SNODAS errors")
plot(gstat_variogram2, main = "Variogram using Elevation")

# Fit a variogram of a bunch of different models
#    *NOTE: No Convergence
variogram.fit <- gstat::fit.variogram(gstat_variogram,
                                     model = gstat::vgm(c("Sph", "Exp", "Gau")))
variogram.fit
plot(gstat_variogram, variogram.fit, main = "fitting variogram for SNODAS")


citation("gstat")


# I fold another file and I don't know which code it is -------------------
# other things to try:
# Convert SNODAS and see if they are spatially correlated? (Create a Variogram)

#### Step 0: Get all of the rasters and Convert to a spatial points data frame
april_1_snotel
april_1_snotel_2003_2 <- april_1_snotel_data[april_1_snotel_data$DATE > "2003-04-01", ]
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
gstat_variogram2 <- gstat::variogram(VALUE ~ ELEVATION, data = sf_y15)

sf_y15$log_elevation <- log(sf_y15$ELEVATION + 1)
sf_y15$log_value <- log(sf_y15$VALUE + 1)
gstat_variogram3 <- gstat::variogram(VALUE ~ log_elevation, data = sf_y15)
gstat_variogram4 <- gstat::variogram(log_value ~ ELEVATION, data = sf_y15)


plot(gstat_variogram, main = "Variogram of SNODAS errors")
plot(gstat_variogram2, main = "Variogram using Elevation")
plot(gstat_variogram3, main = "Variogram using log(Elevation)")
plot(gstat_variogram4, main = "Variogram using log(Elevation)")


# Fit a variogram of a bunch of different models
#    *NOTE: No Convergence
variogram.fit <- gstat::fit.variogram(gstat_variogram,
                                      model = gstat::vgm(c("Sph", "Exp", "Gau")))
variogram.fit
plot(gstat_variogram, variogram.fit, main = "fitting variogram for SNODAS")



