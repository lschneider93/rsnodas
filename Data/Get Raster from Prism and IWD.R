################################################################################
###### Creating a raster with resolution of 800 m so fill in from prism   #########
################################################################################
?get_prism_normals()
prism::get_prism_normals(type = 'tmean', resolution = "800m", keepZip = TRUE,
                         mon = 12)

prism::ls_prism_data(name=TRUE)

new_file <- 1

RS <- prism::prism_stack(prism::ls_prism_data()[1, ])

#proj4string(RS) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#tcrs
#?proj4string()
#raster::proj4string(RS) <- tcrs
sp::proj4string(RS) <- "+proj=longlat +datum=WGS84 +no_defs"
RS  
raster::plot(RS)
#raster::plot(raster::raster(RS, layer = 3))
#raster::raster(raster::raster(utah_april_swe_brick, layer = 2)

t3 <- raster::crop(RS, new_boundary)
t3 <- raster::mask(t3, new_boundary)
raster::plot(t3)

prism <- raster::raster(t3)

raster_of_utah <- prism
raster::plot(raster_of_utah)
################################################################################
###### Creating a raster with resolution of 800 m so fill in from prism   #########
################################################################################
# prism <- raster::raster("PRISM.png")
# 
# prism <- raster::raster(raster::raster(utah_april_swe_brick, layer = 2))
# 
# # need to crop prism
# raster::plot(prism)

# t3 <- raster::crop(prism, new_boundary)
# t3 <- raster::mask(t3, new_boundary)
# raster::plot(t3)
# 
# prism <- raster::raster(t3)

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################

april_df_ghcnd_spatial[[1]]$ANNUAL_elevation
gs <- gstat::gstat(formula = MODEL_DIFFERENCE ~ 1,
                    locations = april_df_ghcnd_spatial[[7]])

# gs

v <- gstat::variogram(gs)
head(v)

plot(v)
?interpolate()
prism
raster::plot(prism)
idw <- raster::interpolate(prism, gs)
idw <- raster::mask(idw, new_boundary)
raster::plot(idw)

utah_april_error_rasters <- vector("list", length(years))

utah_april_error_rasters_elevation <- vector("list", length(years))
for (i in 1:length(years)){
  gs <- gstat::gstat(formula = MODEL_DIFFERENCE ~ 1 ,
                     locations = april_df_ghcnd_spatial[[i]])
  idw <- raster::interpolate(prism, gs)
  idw <- raster::mask(idw, new_boundary)
  utah_april_error_rasters[[i]] <- idw
  
  gs <- gstat::gstat(formula = MODEL_DIFFERENCE ~ ANNUAL_elevation ,
                     locations = april_df_ghcnd_spatial[[i]])
  idw <- raster::interpolate(prism, gs)
  idw <- raster::mask(idw, new_boundary)
  utah_april_error_rasters_elevation[[i]] <- idw
  
}

# for(i in years){
#   #print(paste0(i, '-04-01'))
#   raster::plot(utah_april_error_rasters[[i]])
#   Sys.sleep(2) 
# }

cuts = c(-450, -350, -250, -150, -50, 50, 150, 250, 350, 450) #set breaks
pal <- colorRampPalette(c("red", "gray","blue"))
#pal <- colorRampPalette(c("blue", "gray","black"))

# raster::plot(utah_april_error_rasters[[4]], 
#              breaks = cuts, 
#              col = pal(9))

for(i in 1:length(years)){
  #print(paste0(i, '-04-01'))
  raster::plot(utah_april_error_rasters[[i]], 
               breaks = cuts,
               main = paste(years[i], "Errors"),
               col = pal(9))
  Sys.sleep(2) 
}
################################################################################
###### Trying to predict the Average Bias of the stations  #########
################################################################################

individ_station_df
individ_station

dim(error_stations_spatial)
dim(error_stations)
colnames(error_stations_spatial)

error_stations_spatial2 <- error_stations[, c("LONGITUDE", "LATITUDE",
                                              "ELEVATION", "AVE_BIAS",
                                              "ANNUAL_maxtemp",
                                              "ANNUAL_mintemp",
                                               "ANNUAL_meantemp",
                                              "ANNUAL_maxvapor",
                                              "ANNUAL_minvapor",
                                              "ANNUAL_meandew",
                                              "ANNUAL_precip")]

sp::coordinates(error_stations_spatial2) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations_spatial2) <- ("+proj=longlat +datum=WGS84 +no_defs")
                                              
                        

                              
# gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
#                    locations = april_df_ghcnd_spatial[[7]])
gs <- gstat::gstat(formula = AVE_BIAS ~ 1,
                   #   ANNUAL_maxtemp + ANNUAL_mintemp + ANNUAL_meantemp + 
                   #   ANNUAL_maxvapor + ANNUAL_minvapor + ANNUAL_meandew + 
                   #   ANNUAL_precip,
                   locations = error_stations_spatial2)

gs
# error_stations_spatial

# gs
# ?interpolate()
# idw <- raster::interpolate(prism, gs)
# idw <- raster::mask(idw, new_boundary)
# raster::plot(idw)

gs
?interpolate()
idw <- raster::interpolate(prism, gs)
idw <- raster::mask(idw, new_boundary)
cuts = c(-125, -100, -75, -50, -25,  25, 50, 75, 125, 175, 225) #set breaks
pal <- colorRampPalette(c("red", "gray","blue"))
raster::plot(idw, 
             breaks = cuts,
             main = paste("Average", "Errors"),
             col = pal(9))

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################


df_of_all_years

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################

