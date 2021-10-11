################################################################################
###### Creates a map of utah but is a class == "map"   #########
################################################################################

maps::map(database = 'county', regions = c('utah'))
# raster::plot(nonzero_stations_spatial[nonzero_stations_spatial$MSE_VECTOR > 150, ], add = TRUE)
raster::plot(error_stations_spatial[error_stations_spatial$MSE_VECTOR > 150, ], add = TRUE)


###
nonzero_stations_spatial
error_stations5_spatial

################################################################################
###### This works as well but using spatial polygons   #########
################################################################################

?getData()
USA <- raster::getData("GADM", country = "USA", level = 2)
# raster::plot(USA)

utah <- subset(USA, NAME_1 == "Utah")
# raster::plot(nonzero_stations_spatial[nonzero_stations_spatial$MSE_VECTOR > 150, ])
# raster::plot(utah, add = TRUE)
# class(utah)
# tcrs

#### All the stations that have more than 5 observations
raster::plot(utah, main = "All Stations with 5+ obs")
# raster::plot(error_stations5_spatial, add = TRUE)
# raster::plot(error_stations_spatial, add = TRUE)
raster::plot(error_stations_spatial[error_stations_spatial$REPORTS > 5, ], add = TRUE)
error_stations5_spatial<- error_stations_spatial[error_stations_spatial$REPORTS > 5, ]

####  this order makes more sense and works better
raster::plot(utah, main = "MSE over 50")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 50, ], add = TRUE)

raster::plot(utah, main = "MSE over 100")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 100, ], add = TRUE)

raster::plot(utah, main = "MSE over 125")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 125, ], add = TRUE)

raster::plot(utah, main = "MSE over 150")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 150, ], add = TRUE)

raster::plot(utah, main = "MSE over 200")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 200, ], add = TRUE)

raster::plot(utah, main = "MSE over 250")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 250, ], add = TRUE)

raster::plot(utah, main = "MSE over 300")
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 300, ], add = TRUE)

raster::plot(utah)
raster::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 300, ], add = TRUE)

raster::plot(utah)
sp::plot(error_stations5_spatial[error_stations5_spatial$MSE_VECTOR > 300, ], add = TRUE)


################################################################################
###### Plotting stations when the model underpredicts   #########
################################################################################

dim(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 50, ])

####  this order makes more sense and works better
raster::plot(utah, main = "ave bias over 25, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 25, ], 
             add = TRUE)

raster::plot(utah, main = "ave bias over 50, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 50, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 75, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 75, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 100, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 100, ], 
             add = TRUE)

raster::plot(utah, main = "ave bias over 125, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 125, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 150, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 150, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 200, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 200, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 250, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 250, ],
             add = TRUE)

raster::plot(utah, main = "ave bias over 275, model underpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 275, ],
             add = TRUE)

dim(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 200, ])

################################################################################
###### plotting when the model overpredicts  #########
################################################################################
raster::plot(utah, main = "ave bias under -25, model overpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -25, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias under -50, model overpredicts") 
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -50, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias under -75, model overpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -75, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias under -100, model overpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -100, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias under -125, model overpredicts")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -125, ],
             add = TRUE, col = "red")

################################################################################
###### plotting over and under predicting together   #########
################################################################################
raster::plot(utah, main = "ave bias over 25 \n underpredicts - blue, over - red")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 25, ], 
             add = TRUE, col = "blue")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -25, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias over 50, underpredicts - blue, over - red")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 50, ],
             add = TRUE, col = "blue")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -50, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias over 75, underpredicts - blue, over - red")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 75, ],
             add = TRUE, col = "blue")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -75, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias over 100, underpredicts - blue, over - red")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 100, ],
             add = TRUE, col = "blue")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -100, ],
             add = TRUE, col = "red")

raster::plot(utah, main = "ave bias over 125, underpredicts - blue, over - red")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS > 125, ],
             add = TRUE, col = "blue")
raster::plot(error_stations5_spatial[error_stations5_spatial$AVE_BIAS < -125, ],
             add = TRUE, col = "red")

################################################################################
###### This works as well but using spatial polygons   #########
################################################################################
ut_stat_info[ut_stat_info$ID == high_stations, ]
high_stations 

individ_years
individ_years_spatial <- individ_years

individ_years_spatial <- individ_years_spatial[vapply(individ_years_spatial, Negate(is.null), NA)]

for (i in 1:length(individ_years_spatial)){
  sp::coordinates(individ_years_spatial[[i]]) <- c("LONGITUDE", "LATITUDE")
  sp::proj4string(individ_years_spatial[[i]]) <- ("+proj=longlat +datum=WGS84 +no_defs")
}

################################################################################
###### This works as well but using spatial polygons   #########
################################################################################

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 250, underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 250, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -250, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 200, underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 200, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -200, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 150, underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 150, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -150, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 100, underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 100, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -100, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 50, underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 50, ],
               add = TRUE, col = "blue")
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < 50, ],
               add = TRUE, col = "red")
  Sys.sleep(2)
}
# raster::plot(utah, main = paste(year[i], "Difference over 50, underpredicts - blue, over - red"))
# raster::plot(individ_years_spatial[[1]][individ_years_spatial[[1]]$DIFFERENCE > 100, ],
#              add = TRUE, col = "blue")
# raster::plot(individ_years_spatial[[1]][individ_years_spatial[[1]]$DIFFERENCE < -100, ],
#              add = TRUE, col = "red")

for (i in 1:length(individ_years_spatial)){
  print(c(year[i], mean(individ_years_spatial[[i]]$DIFFERENCE)))
  Sys.sleep(2)
}
