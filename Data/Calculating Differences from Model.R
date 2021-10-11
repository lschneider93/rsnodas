################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################
april_df_ghcnd_spatial <- april_df_ghcnd
tcrs <- raster::crs(map[[1]])

for (j in 1:length(april_df_ghcnd_spatial)){
  sp::coordinates(april_df_ghcnd_spatial[[j]]) <- c("LONGITUDE", "LATITUDE")
  sp::proj4string(april_df_ghcnd_spatial[[j]]) <- "+proj=longlat +datum=WGS84 +no_defs"
}

# sp::coordinates(station_info) <- c("LONGITUDE", "LATITUDE")
# sp::proj4string(station_info) <- tcrs

################################################################################
######  Storing the values of the and Extracting values from the model  ########
################################################################################

#station_info
test_values <- vector("list", length(years))
bilinear_values <- vector("list", length(years))
# test_values1 <- test_values
# bilinear_values1 <- bilinear_values

# for (i in 1:length(years)){
#   test_values1[[i]] <- raster::extract(map[[i]], station_info)
#   bilinear_values1[[i]] <- raster::extract(map[[i]], station_info,
#                                            method = "bilinear")
# }

# for each list (year) we will go through and extract values at the stations
for (i in 1:length(years)){
  
  # extracting the values at the stations in the april_df_ghcnd_spatial
  test_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]])
  bilinear_values[[i]] <- raster::extract(map[[i]], april_df_ghcnd_spatial[[i]], 
                                          method = "bilinear")
}

################################################################################
######## Calculating the difference and storing that information     ###########
################################################################################
# dim(april_df_ghcnd[[1]])
for (j in 1:length(years)){
  april_df_ghcnd_spatial[[j]]$MODEL_WESD <- test_values[[j]] 
  april_df_ghcnd[[j]]$MODEL_WESD <- test_values[[j]]
  
}

################################################################################
######## Calculating the difference and storing that information     ###########
################################################################################

for (j in 1:length(years)){
  april_df_ghcnd_spatial[[j]]$DIFFERENCE <- april_df_ghcnd_spatial[[j]]$VALUE - 
    april_df_ghcnd_spatial[[j]]$MODEL_WESD
  april_df_ghcnd[[j]]$DIFFERENCE <- april_df_ghcnd[[j]]$VALUE - 
    april_df_ghcnd[[j]]$MODEL_WESD
}

# PLotting the difference vs Elevation
plot(april_df_ghcnd[[1]]$ELEVATION, april_df_ghcnd[[1]]$DIFFERENCE)
#april_df_ghcnd[[1]]$ID[april_df_ghcnd[[1]]$DIFFERENCE > 800]  # USS0011H08S
#april_df_ghcnd[[13]]$ID[april_df_ghcnd[[13]]$DIFFERENCE < -500] # USS0011H08S # USS0011J12S
#april_df_ghcnd[[14]]$ID[april_df_ghcnd[[14]]$DIFFERENCE < -200] # USS0011J12S

# Plotting elevation with difference for each year
for (j in 1:length(years)){
  plot(april_df_ghcnd[[j]]$ELEVATION, april_df_ghcnd[[j]]$DIFFERENCE,
       main = paste("Residuals for the year", years[j]))
}

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################

# site_id_long <- c(april_df_ghcnd[[1]]$ID, april_df_ghcnd[[2]]$ID, april_df_ghcnd[[3]]$ID, 
#   april_df_ghcnd[[4]]$ID, april_df_ghcnd[[5]]$ID, april_df_ghcnd[[6]]$ID, 
#   april_df_ghcnd[[7]]$ID, april_df_ghcnd[[8]]$ID, april_df_ghcnd[[9]]$ID,
#   april_df_ghcnd[[10]]$ID, april_df_ghcnd[[11]]$ID, april_df_ghcnd[[12]]$ID, 
#   april_df_ghcnd[[13]]$ID, april_df_ghcnd[[14]]$ID, april_df_ghcnd[[15]]$ID,
#   april_df_ghcnd[[16]]$ID, april_df_ghcnd[[17]]$ID)
# 
# length(site_id_long)
# class(april_df_ghcnd[[1]])

df_of_all_years <- rbind(april_df_ghcnd[[1]], april_df_ghcnd[[2]], april_df_ghcnd[[3]],
                         april_df_ghcnd[[4]], april_df_ghcnd[[5]], april_df_ghcnd[[6]], 
                         april_df_ghcnd[[7]], april_df_ghcnd[[8]], april_df_ghcnd[[9]],
                         april_df_ghcnd[[10]], april_df_ghcnd[[11]], april_df_ghcnd[[12]], 
                         april_df_ghcnd[[13]], april_df_ghcnd[[14]], april_df_ghcnd[[15]],
                         april_df_ghcnd[[16]], april_df_ghcnd[[17]])

df_of_all_years$DATE <- as.factor(df_of_all_years$DATE)

df_of_all_years_copy <- df_of_all_years
df_of_all_years_spatial <- df_of_all_years

# sp::proj4string(df_of_all_years_spatial) <- tcrs
sp::coordinates(df_of_all_years_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(df_of_all_years_spatial) <- ("+proj=longlat +datum=WGS84 +no_defs")

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################

for(i in years){
  # print(paste0(i, '-04-01'))
  print(sp::bubble(df_of_all_years_spatial[df_of_all_years_spatial$DATE
                                        == paste0(i, '-04-01'), ],
                   'DIFFERENCE', main = paste0(i)))
  Sys.sleep(1)
}

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################


