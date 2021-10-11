for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 250, \n underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 250, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -250, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 200, \n underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 200, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -200, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 150, \n underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 150, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -150, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 100, \n underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 100, ],
               add = TRUE, col = "blue", cex = 1.5)
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < -100, ],
               add = TRUE, col = "red", cex = 1.5)
  Sys.sleep(2)
}

for (i in 1:length(individ_years_spatial)){
  raster::plot(utah, main = paste(year[i], "Difference over 50,\n underpredicts - blue, over - red"))
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE > 50, ],
               add = TRUE, col = "blue")
  raster::plot(individ_years_spatial[[i]][individ_years_spatial[[i]]$DIFFERENCE < 50, ],
               add = TRUE, col = "red")
  Sys.sleep(2)
}


### THINGS TO SHOW

pdp <- vector("list", length(individ_years))

for (i in 1:length(individ_years)){
  #### Random Forest using the DIFFERENCE
  # Elevation
  rf.elevation_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("ELEVATION"), chull = TRUE)
  plot.rf.elevation_annual <- autoplot(rf.elevation_annual , contour = TRUE)
  
  rf.elevation_march <- partial(random_forest_march_diff[[i]], pred.var = c("ELEVATION"), chull = TRUE)
  plot.rf.elevation_march <- autoplot(rf.elevation_march , contour = TRUE)
  
  
  # LATITUDE
  rf.latitude_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LATITUDE"), chull = TRUE)
  plot.rf.latitude_annual <- autoplot(rf.latitude_annual , contour = TRUE)
  
  rf.latitude_march <- partial(random_forest_march_diff[[i]], pred.var = c("LATITUDE"), chull = TRUE)
  plot.rf.latitude_march <- autoplot(rf.latitude_march , contour = TRUE)
  
  # LONGITUDE
  rf.longitude_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE"), chull = TRUE)
  plot.rf.longitude_annual <- autoplot(rf.longitude_annual , contour = TRUE)
  
  rf.longitude_march <- partial(random_forest_march_diff[[i]], pred.var = c("LONGITUDE"), chull = TRUE)
  plot.rf.longitude_march <- autoplot(rf.longitude_march , contour = TRUE)
  
  
  # Two Variables
  rf.lat.long_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE", "LATITUDE"), chull = TRUE)
  plot.rf.lat.long_annual <- autoplot(rf.lat.long_annual, contour = TRUE, 
                                      legend.title = "Partial\ndependence")
  
  
  g <- grid.arrange(plot.rf.latitude_annual, plot.rf.longitude_annual, plot.rf.elevation_annual, plot.rf.lat.long_annual)
  pdp[[i]] <- g
}
plot(pdp[[3]])
