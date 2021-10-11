# r8 <- raster::raster(paste0("~/Desktop/PRISM/", mon[i],
#                             "/monthly/", info[8], "/PRISM_",
#                             file_type[8], "asc.asc"))

r8 <- raster::raster(paste0("C:/Users/Logan/Desktop/PRISM/", "annual",
                            "/monthly/", "elevation", "/PRISM_",
                            "us_dem_800m_", "asc.asc"))
r8 <- raster::projectRaster(r8, crs = "+proj=longlat +datum=WGS84 +no_defs")

raster::plot(r8)
################################################################################
######  This is for the GHCND Data Set  #########
################################################################################
getwd()
# ghcnd_station_info <- read.csv("GHCND_FINAL_ALL_STATIONS.csv")

april_df_ghcnd[[1]]

gridpoints <- vector("list", length(ghcnd_station_info$NAME))
gridpoints_final <- vector("list", length(ghcnd_station_info$NAME))

# gridpoints <- vector("list", length(april_df_ghcnd[[1]]$NAME))
# gridpoints_final <- vector("list", length(april_df_ghcnd[[1]]$NAME))

# LATITUDE changes north + .01 and south -.01
# LONGITUTDE change east + .01 and west -.01

raster::xres(map[[1]])

lat <- ghcnd_station_info$LATITUDE
long <- ghcnd_station_info$LONGITUDE

# lat <- april_df_ghcnd[[1]]$LATITUDE
# long <- april_df_ghcnd[[1]]$LONGITUDE


# expand.grid(x = lat, y = lon, stringsAsFactors = FALSE)
map[[1]]

# Get the Resolution from the Raster and then just 1 times or 2 times
nnnnn <- lat + (5 * raster::xres(map[[1]]))
nnnn <- lat + (4 * raster::xres(map[[1]]))
nnn <- lat + (3 * raster::xres(map[[1]]))
nn <- lat + (2 * raster::xres(map[[1]]))
n <- lat + raster::xres(map[[1]])
c <- lat
s <- lat - raster::xres(map[[1]])
ss <- lat - (2 * raster::xres(map[[1]]))
sss <- lat - (3 * raster::xres(map[[1]]))
ssss <- lat - (4 * raster::xres(map[[1]]))
sssss <- lat - (5 * raster::xres(map[[1]]))

eeeee <- long + (5 * raster::yres(map[[1]]))
eeee <- long + (4 * raster::yres(map[[1]]))
eee <- long + (3 * raster::yres(map[[1]]))
ee <- long + (2 * raster::yres(map[[1]]))
e <- long + (raster::yres(map[[1]]))
cc <- long
w <- long - (raster::yres(map[[1]]))
ww <- long - (2 * raster::yres(map[[1]]))
www <- long - (3 * raster::yres(map[[1]]))
wwww <- long - (4 * raster::yres(map[[1]]))
wwwww <- long - (5 * raster::yres(map[[1]]))

i = 1
for (i in 1:length(ghcnd_station_info$NAME)){
# for (i in 1:length(april_df_ghcnd[[1]]$NAME)){
  top5 <- expand.grid(x = nnnnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top4 <- expand.grid(x = nnnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top3 <- expand.grid(x = nnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top2 <- expand.grid(x = nn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top1 <- expand.grid(x = n[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  mid <- expand.grid(x = c[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                     e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot1 <- expand.grid(x = s[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot2 <- expand.grid(x = ss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot3 <- expand.grid(x = sss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot4 <- expand.grid(x = ssss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                       e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot5 <- expand.grid(x = sssss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  
  colnames(top5) <- c("LATITUDE", "LONGITUDE")
  colnames(top4) <- c("LATITUDE", "LONGITUDE")
  colnames(top3) <- c("LATITUDE", "LONGITUDE")
  colnames(top2) <- c("LATITUDE", "LONGITUDE")
  colnames(top1) <- c("LATITUDE", "LONGITUDE")
  
  colnames(mid) <- c("LATITUDE", "LONGITUDE")
  
  colnames(bot1) <- c("LATITUDE", "LONGITUDE")
  colnames(bot2) <- c("LATITUDE", "LONGITUDE")
  colnames(bot3) <- c("LATITUDE", "LONGITUDE")
  colnames(bot4) <- c("LATITUDE", "LONGITUDE")
  colnames(bot5) <- c("LATITUDE", "LONGITUDE")
 
  
  gridpts <- rbind(top5, top4, top3, top2, top1, mid, bot1, bot2, bot3, bot4, bot5)
  Number <- c(1:121)
  gridpts <- cbind(gridpts, Number)
  gridpts_spatial <- gridpts
  
  sp::coordinates(gridpts_spatial) <- c("LONGITUDE", "LATITUDE")
  sp::proj4string(gridpts_spatial) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # elevation
  # extracting the values at the stations in the april_df_ghcnd_spatial
  gridpts$PRISM_ELEVATION <- raster::extract(r8, gridpts_spatial)
  gridpts_spatial$PRISM_ELEVATION <- raster::extract(r8, gridpts_spatial)
  
  # gridpts_spatial
  # station5[1, ]$ELEVATION
  
  gridpts$ELEVATION <- ut_stat_info[i, ]$ELEVATION
  #gridpoints[[i]]$VALUE
  
  gridpts$ELEVATION_DIFF <- abs(gridpts$ELEVATION - gridpts$PRISM_ELEVATION)
  
  gridpts$WESD4 <- raster::extract(map[[1]], gridpts_spatial)
  gridpts$WESD5 <- raster::extract(map[[2]], gridpts_spatial)
  gridpts$WESD6 <- raster::extract(map[[3]], gridpts_spatial)
  gridpts$WESD7 <- raster::extract(map[[4]], gridpts_spatial)
  gridpts$WESD8 <- raster::extract(map[[5]], gridpts_spatial)
  gridpts$WESD9 <- raster::extract(map[[6]], gridpts_spatial)
  gridpts$WESD10 <- raster::extract(map[[7]], gridpts_spatial)
  gridpts$WESD11 <- raster::extract(map[[8]], gridpts_spatial)
  gridpts$WESD12 <- raster::extract(map[[9]], gridpts_spatial)
  gridpts$WESD13 <- raster::extract(map[[10]], gridpts_spatial)
  gridpts$WESD14 <- raster::extract(map[[11]], gridpts_spatial)
  gridpts$WESD15 <- raster::extract(map[[12]], gridpts_spatial)
  gridpts$WESD16 <- raster::extract(map[[13]], gridpts_spatial)
  gridpts$WESD18 <- raster::extract(map[[14]], gridpts_spatial)
  gridpts$WESD19 <- raster::extract(map[[15]], gridpts_spatial)
  gridpts$WESD20 <- raster::extract(map[[16]], gridpts_spatial)
  gridpts$WESD21 <- raster::extract(map[[17]], gridpts_spatial)
  
  # gridpoints[[i]] <- gridpts[gridpts$ELEVATION_DIFF == min(gridpts$ELEVATION_DIFF), ]
  gridpoints[[i]] <- gridpts
}

gridpoints_df_final <- gridpoints[[1]]

for (i in 2:length(gridpoints)){
  gridpoints_df_final <- rbind(gridpoints_df_final, gridpoints[[i]])
}

dim(gridpoints_df_final)

write.csv(gridpoints_df_final, "GHCND Stations with 121 observation.csv")


################################################################################
###### This is for the GHCND Data Set #########
################################################################################

nrcs_other <- read.csv("all_700_nrcs_station_info_wPRISM.csv")
nrcs_other
gridpoints <- vector("list", length(nrcs_other$NAME))
gridpoints_final <- vector("list", length(nrcs_other$NAME))

# LATITUDE changes north + .01 and south -.01
# LONGITUTDE change east + .01 and west -.01

raster::xres(map[[1]])

# lat <- c(41.38, 40.97, 40.83, 39.89, 39.35)
# long <- c(-111.94, -111.81, -111.76, -111.25, -112.33)
lat <- nrcs_other$Latitude
long <- nrcs_other$Longitude

# expand.grid(x = lat, y = lon, stringsAsFactors = FALSE)
map[[1]]

# Get the Resolution from the Raster and then just 1 times or 2 times
nnnnn <- lat + (5 * raster::xres(map[[1]]))
nnnn <- lat + (4 * raster::xres(map[[1]]))
nnn <- lat + (3 * raster::xres(map[[1]]))
nn <- lat + (2 * raster::xres(map[[1]]))
n <- lat + raster::xres(map[[1]])
c <- lat
s <- lat - raster::xres(map[[1]])
ss <- lat - (2 * raster::xres(map[[1]]))
sss <- lat - (3 * raster::xres(map[[1]]))
ssss <- lat - (4 * raster::xres(map[[1]]))
sssss <- lat - (5 * raster::xres(map[[1]]))

eeeee <- long + (5 * raster::yres(map[[1]]))
eeee <- long + (4 * raster::yres(map[[1]]))
eee <- long + (3 * raster::yres(map[[1]]))
ee <- long + (2 * raster::yres(map[[1]]))
e <- long + (raster::yres(map[[1]]))
cc <- long
w <- long - (raster::yres(map[[1]]))
ww <- long - (2 * raster::yres(map[[1]]))
www <- long - (3 * raster::yres(map[[1]]))
wwww <- long - (4 * raster::yres(map[[1]]))
wwwww <- long - (5 * raster::yres(map[[1]]))

for (i in 1:length(nrcs_other$State.Name)){
  top5 <- expand.grid(x = nnnnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                          e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top4 <- expand.grid(x = nnnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top3 <- expand.grid(x = nnn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                        e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top2 <- expand.grid(x = nn[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                       e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  top1 <- expand.grid(x = n[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i]))
  
  mid <- expand.grid(x = c[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                     e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot1 <- expand.grid(x = s[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                      e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot2 <- expand.grid(x = ss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                       e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot3 <- expand.grid(x = sss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                        e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot4 <- expand.grid(x = ssss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                         e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  bot5 <- expand.grid(x = sssss[i], y = c(wwwww[i], wwww[i], www[i], ww[i], w[i], cc[i],
                                          e[i], ee[i], eee[i], eeee[i], eeeee[i] ))
  
  
  colnames(top5) <- c("LATITUDE", "LONGITUDE")
  colnames(top4) <- c("LATITUDE", "LONGITUDE")
  colnames(top3) <- c("LATITUDE", "LONGITUDE")
  colnames(top2) <- c("LATITUDE", "LONGITUDE")
  colnames(top1) <- c("LATITUDE", "LONGITUDE")
  
  colnames(mid) <- c("LATITUDE", "LONGITUDE")
  
  colnames(bot1) <- c("LATITUDE", "LONGITUDE")
  colnames(bot2) <- c("LATITUDE", "LONGITUDE")
  colnames(bot3) <- c("LATITUDE", "LONGITUDE")
  colnames(bot4) <- c("LATITUDE", "LONGITUDE")
  colnames(bot5) <- c("LATITUDE", "LONGITUDE")
  
  
  gridpts <- rbind(top5, top4, top3, top2, top1, mid, bot1, bot2, bot3, bot4, bot5)
  Number <- c(1:121)
  gridpts <- cbind(gridpts, Number)
  gridpts_spatial <- gridpts
  
  sp::coordinates(gridpts_spatial) <- c("LONGITUDE", "LATITUDE")
  sp::proj4string(gridpts_spatial) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # elevation
  # extracting the values at the stations in the april_df_ghcnd_spatial
  gridpts$PRISM_ELEVATION <- raster::extract(r8, gridpts_spatial)
  gridpts_spatial$PRISM_ELEVATION <- raster::extract(r8, gridpts_spatial)
  
  # gridpts_spatial
  # station5[1, ]$ELEVATION
  
  gridpts$ELEVATION <- nrcs_other[i, ]$Elevation * 3.2804 
  #gridpoints[[i]]$VALUE
  
  gridpts$ELEVATION_DIFF <- abs(gridpts$ELEVATION - gridpts$PRISM_ELEVATION)
  
  gridpts$WESD4 <- raster::extract(map[[1]], gridpts_spatial)
  gridpts$WESD5 <- raster::extract(map[[2]], gridpts_spatial)
  gridpts$WESD6 <- raster::extract(map[[3]], gridpts_spatial)
  gridpts$WESD7 <- raster::extract(map[[4]], gridpts_spatial)
  gridpts$WESD8 <- raster::extract(map[[5]], gridpts_spatial)
  gridpts$WESD9 <- raster::extract(map[[6]], gridpts_spatial)
  gridpts$WESD10 <- raster::extract(map[[7]], gridpts_spatial)
  gridpts$WESD11 <- raster::extract(map[[8]], gridpts_spatial)
  gridpts$WESD12 <- raster::extract(map[[9]], gridpts_spatial)
  gridpts$WESD13 <- raster::extract(map[[10]], gridpts_spatial)
  gridpts$WESD14 <- raster::extract(map[[11]], gridpts_spatial)
  gridpts$WESD15 <- raster::extract(map[[12]], gridpts_spatial)
  gridpts$WESD16 <- raster::extract(map[[13]], gridpts_spatial)
  gridpts$WESD18 <- raster::extract(map[[14]], gridpts_spatial)
  gridpts$WESD19 <- raster::extract(map[[15]], gridpts_spatial)
  gridpts$WESD20 <- raster::extract(map[[16]], gridpts_spatial)
  gridpts$WESD21 <- raster::extract(map[[17]], gridpts_spatial)
  
  # gridpoints[[i]] <- gridpts[gridpts$ELEVATION_DIFF == min(gridpts$ELEVATION_DIFF), ]
  gridpoints[[i]] <- gridpts
}

gridpoints_df_final <- gridpoints[[1]]

for (i in 2:length(gridpoints)){
  gridpoints_df_final <- rbind(gridpoints_df_final, gridpoints[[i]])
}

dim(gridpoints_df_final)

write.csv(gridpoints_df_final, "NRCS Stations with 121 observation.csv")
