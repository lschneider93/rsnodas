## plotting those that are positive
##### POSITIVE
plot(as.factor(individ_station[[34]]$DATE), individ_station[[34]]$DIFFERENCE, 
     cex = .5) #17
plot(individ_station[[34]]$DIFFERENCE) #17
plot(individ_station[[48]]$DIFFERENCE) #17
plot(individ_station[[69]]$DIFFERENCE) #17
plot(individ_station[[118]]$DIFFERENCE) #10
plot(individ_station[[135]]$DIFFERENCE) #8
plot(individ_station[[148]]$DIFFERENCE) #6

#####  NEGATIVE
plot(individ_station[[35]]$DIFFERENCE) #17
plot(individ_station[[41]]$DIFFERENCE) #17
plot(individ_station[[85]]$DIFFERENCE) #7
plot(individ_station[[93]]$DIFFERENCE) #1
plot(individ_station[[116]]$DIFFERENCE) #10
plot(individ_station[[128]]$DIFFERENCE) #8
plot(individ_station[[149]]$DIFFERENCE) #6
plot(individ_station[[151]]$DIFFERENCE) #1

####


plot(individ_station[[35]][error_stations17$uniq_stations[1]] error_stations17$uniq_stations[1])
error_stations17
error_stations17
error_stations17
error_stations17
error_stations17

####

coordinates(new_boundary)
raster::plot(new_boundary[ ,1])
class(new_boundary[, 1])

sp::SpatialPolygons(list(new_boundary[, 1]))
# install.packages("mapproj")
maps::map(database = 'state', regions = c('utah'),
          projection = raster::crs(map[[1]])
          )

maps::map(database = 'state', regions = c('utah'),
          projection = "+proj=longlat"
          )


?crs()

?maps::map()

#class(maps::map(database = 'county', regions = c('utah')))

raster::plot(nonzero_stations_spatial[nonzero_stations_spatial$MSE_VECTOR > 150, ], add = TRUE)
#, add = TRUE)
nonzero_stations$MSE_VECTOR
