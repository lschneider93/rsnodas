################################################################################
###### subsetting and looking at those that have all positive errors   #########
################################################################################

error_stations[error_stations$summation == error_stations$total_error
               & error_stations$summation != 0, ]

### Places that we are over
positive_stations <- error_stations[error_stations$summation == error_stations$total_error
                                    & error_stations$summation != 0, ]
# & error_stations$total_error > 60, ]

positive_stations_spatial <- positive_stations

plot(positive_stations$ELEVATION, positive_stations$total_error)
plot(positive_stations$LATITUDE, positive_stations$total_error)
plot(positive_stations$LONGITUDE, positive_stations$total_error)

plot(positive_stations$ELEVATION, positive_stations$MSE_VECTOR)
plot(positive_stations$LATITUDE, positive_stations$MSE_VECTOR)
plot(positive_stations$LONGITUDE, positive_stations$MSE_VECTOR)

################################################################################
###### subsetting and looking at those that have all negative errors   #########
################################################################################
error_stations[error_stations$summation == -error_stations$total_error
               & error_stations$summation != 0, ]

negative_stations <- error_stations[error_stations$summation == -error_stations$total_error
                                    & error_stations$summation != 0, ]
#& error_stations$total_error > 50, ]

negative_stations_spatial <- negative_stations

plot(negative_stations$ELEVATION, negative_stations$total_error)
plot(negative_stations$LATITUDE, negative_stations$total_error)
plot(negative_stations$LONGITUDE, negative_stations$total_error)



################################################################################
###### subsetting and looking at those that have all 0 errors   #########
################################################################################
error_stations[error_stations$summation == -error_stations$total_error
               & error_stations$summation == 0, ]

zero_stations <- error_stations[error_stations$summation == -error_stations$total_error
                                & error_stations$summation == 0, ]
#& error_stations$total_error > 50, ]

zero_stations_spatial <- zero_stations

plot(zero_stations$ELEVATION, zero_stations$total_error)
plot(zero_stations$LATITUDE, zero_stations$total_error)
plot(zero_stations$LONGITUDE, zero_stations$total_error)

################################################################################
###### Seeing the number of observations for each type   #########
################################################################################
##### POSITIVE
length(individ_station[[34]]$DIFFERENCE)
length(individ_station[[48]]$DIFFERENCE)
length(individ_station[[69]]$DIFFERENCE)
length(individ_station[[118]]$DIFFERENCE)
length(individ_station[[135]]$DIFFERENCE)
length(individ_station[[148]]$DIFFERENCE)


#####  NEGATIVE
length(individ_station[[35]]$DIFFERENCE)
length(individ_station[[41]]$DIFFERENCE)
length(individ_station[[85]]$DIFFERENCE)
length(individ_station[[93]]$DIFFERENCE)
length(individ_station[[116]]$DIFFERENCE)
length(individ_station[[128]]$DIFFERENCE)
length(individ_station[[149]]$DIFFERENCE)
length(individ_station[[151]]$DIFFERENCE)

##### 0's Category
length(individ_station[[121]]$DIFFERENCE)
length(individ_station[[125]]$DIFFERENCE)
length(individ_station[[126]]$DIFFERENCE)
length(individ_station[[127]]$DIFFERENCE)
length(individ_station[[146]]$DIFFERENCE)
length(individ_station[[147]]$DIFFERENCE)
length(individ_station[[150]]$DIFFERENCE)
length(individ_station[[152]]$DIFFERENCE)
length(individ_station[[153]]$DIFFERENCE)
length(individ_station[[154]]$DIFFERENCE)
length(individ_station[[155]]$DIFFERENCE)

################################################################################
######    #########
################################################################################

##### POSITIVE
length(individ_station[[34]]$DIFFERENCE) #17
length(individ_station[[48]]$DIFFERENCE) #17
length(individ_station[[69]]$DIFFERENCE) #17
length(individ_station[[118]]$DIFFERENCE) #10
length(individ_station[[135]]$DIFFERENCE) #8
length(individ_station[[148]]$DIFFERENCE) #6

#####  NEGATIVE
length(individ_station[[35]]$DIFFERENCE) #17
length(individ_station[[41]]$DIFFERENCE) #17
length(individ_station[[85]]$DIFFERENCE) #7
length(individ_station[[93]]$DIFFERENCE) #1
length(individ_station[[116]]$DIFFERENCE) #10
length(individ_station[[128]]$DIFFERENCE) #8
length(individ_station[[149]]$DIFFERENCE) #6
length(individ_station[[151]]$DIFFERENCE) #1

##### 0's Category
length(individ_station[[121]]$DIFFERENCE) #1
length(individ_station[[125]]$DIFFERENCE) #1
length(individ_station[[126]]$DIFFERENCE) #5
length(individ_station[[127]]$DIFFERENCE) #2
length(individ_station[[146]]$DIFFERENCE) #2
length(individ_station[[147]]$DIFFERENCE) #2
length(individ_station[[150]]$DIFFERENCE) #4
length(individ_station[[152]]$DIFFERENCE) #1
length(individ_station[[153]]$DIFFERENCE) #3
length(individ_station[[154]]$DIFFERENCE) #3
length(individ_station[[155]]$DIFFERENCE) #2


################################################################################
###### subsetting and looking at those that have all 0 errors   #########
################################################################################
range(MSE_VECTOR)
sort(MSE_VECTOR)
error_stations[order(MSE_VECTOR, decreasing = TRUE), ]

nonzero_stations <- error_stations[error_stations$total_error != 0, ]
nonzero_stations <- nonzero_stations[order(nonzero_stations$total_error, decreasing = TRUE), ]

nonzero_stations_spatial <- nonzero_stations

plot(nonzero_stations$ELEVATION, nonzero_stations$total_error)
plot(nonzero_stations$LATITUDE, nonzero_stations$total_error)
plot(nonzero_stations$LONGITUDE, nonzero_stations$total_error)

plot(nonzero_stations$ELEVATION, nonzero_stations$MSE_VECTOR)
plot(nonzero_stations$LATITUDE, nonzero_stations$MSE_VECTOR)
plot(nonzero_stations$LONGITUDE, nonzero_stations$MSE_VECTOR)

sp::proj4string(nonzero_stations_spatial) <- tcrs
sp::coordinates(nonzero_stations_spatial) <- c("LONGITUDE", "LATITUDE")

raster::plot(nonzero_stations[nonzero_stations$MSE_VECTOR > 200, ]$LONGITUDE)

################################################################################
###### subsetting and looking at those that have all 0 errors   #########
################################################################################
class(error_stations$REPORTS)
#positive_stations <-#
error_stations10 <- error_stations[error_stations$REPORTS > 10, ]
error_stations1 <- error_stations[error_stations$REPORTS > 1, ]
error_stations17 <- error_stations[error_stations$REPORTS > 16, ]
error_stations5 <- error_stations[error_stations$REPORTS > 4, ]
error_stations[error_stations$REPORTS > 2, ]
dim(error_stations[error_stations$REPORTS > 14, ])

error_stations10_spatial <- error_stations10
error_stations17_spatial <- error_stations17
error_stations5_spatial <- error_stations5

sp::coordinates(error_stations5_spatial) <- c("LONGITUDE", "LATITUDE")
sp::proj4string(error_stations5_spatial) <- "+proj=longlat +datum=WGS84 +no_defs"





################################################################################
###### subsetting and looking at those that have all 0 errors   #########
################################################################################
# data("fifty_states") # this line is optional due to lazy data loading
# 





# # Load summarized data set (by month)
# weatherSum <- read.csv("data/summary_city_month.csv")
# locations <- read.csv("data/locationsFinal.csv")
# 
# # Adjust the location of Alaska and Hawaii
# longitude <- c(-117.25, -108.50)
# latitude <- c(27.2, 27)
# locations$latitude[locations$state == "Alaska"] <- latitude[1]
# locations$latitude[locations$state == "Hawaii"] <- latitude[2]
# locations$longitude[locations$state == "Alaska"] <- longitude[1]
# locations$longitude[locations$state == "Hawaii"] <- longitude[2]
# 
# weatherSum$latitude[weatherSum$state == "Alaska"] <- latitude[1]
# weatherSum$latitude[weatherSum$state == "Hawaii"] <- latitude[2]
# weatherSum$longitude[weatherSum$state == "Alaska"] <- longitude[1]
# weatherSum$longitude[weatherSum$state == "Hawaii"] <- longitude[2]
# 
# # Make a temporary data frame of coordinates so that everyting gets identically projected: 
# coordDF <- data.frame(long = c(fifty_states$long, weatherSum$longitude, locations$longitude),
#                       lat = c(fifty_states$lat, weatherSum$latitude, locations$latitude),
#                       id = c(rep(1, nrow(fifty_states)), rep(2, nrow(weatherSum)), rep(3, nrow(locations))))
# 
# tcoords <- mapproject(coordDF$long, coordDF$lat, projection="mercator", 
#                       parameters=NULL, orientation=NULL)
# 
# fifty_states$long <- tcoords$x[coordDF$id == 1]
# fifty_states$lat <- tcoords$y[coordDF$id == 1]
# 
# weatherSum$longitude2 <- tcoords$x[coordDF$id == 2]
# weatherSum$latitude2 <- tcoords$y[coordDF$id == 2]
# 
# locations$longitude2 <- tcoords$x[coordDF$id == 3]
# locations$latitude2 <- tcoords$y[coordDF$id == 3]
# 
# weatherSum$dRad <- (pi/2) - ( ((2*pi)/12) * (weatherSum$month - 1) )
# 
# # Create a scaled version of the BSS ratio
# weatherSum$sBSS <- 1-weatherSum$BSS
# weatherSum$sBSS <- weatherSum$sBSS / max(weatherSum$sBSS)
# 
# # Determine minimum and maximum Brier Skill Scores (unajusted)
# minp <- min(1-weatherSum$BSS)
# maxp <- max(1-weatherSum$BSS)
# 
# weatherSum$Cluster <- locations$Cluster6[match(weatherSum$AirPtCd, locations$AirPtCd)]
# 
# weatherSum.melt <- reshape2::melt(weatherSum, measure.vars = c("mxT_mean_abs", "mnT_mean_abs"))
# 
# # Scale the error values to the maximum of the combined set.
# maxEr <- max(weatherSum.melt$value)
# minEr <- min(weatherSum.melt$value)
# weatherSum.melt$value2 <- weatherSum.melt$value / maxEr
# 
# weatherSum.melt$group <- paste(weatherSum.melt$AirPtCd, weatherSum.melt$variable, sep = "")
# 
# locations$stateABB <- "none"
# for(i in 1:nrow(locations)){locations$stateABB[i] <- state.abb[grep(locations$state[i], state.name)]}
# locations$state <- tolower(locations$state)
# 
# # Use same color syntax for original map
# pal2 <- brewer.pal(8, "Set1")[c(1:5, 8)]
# pal2 <- pal2[c(4, 2, 3, 6, 1, 5)]
# 
# # Map 8 - small dots, different line thickness/intensity for each cluster. 
# #=============================================================================
# set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf")
# set1 <- set1[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
# set2 <- c("#fb9a99", "#a6cee3", "#b2df8a", "#cab2d6", "#fdbf6f", "#fccde5")
# set2 <- set2[c(4, 2, 3, 6, 1, 5)] # Reorder to match the original layout
# 
# weatherSum.melt$col <- "black"
# weatherSum.melt$col2 <- "black"
# weatherSum.melt$col[weatherSum.melt$variable == "mxT_mean_abs"] <- 
#   set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mxT_mean_abs"]]
# weatherSum.melt$col[weatherSum.melt$variable == "mnT_mean_abs"] <- 
#   set2[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]
# weatherSum.melt$col2[weatherSum.melt$variable == "mnT_mean_abs"] <- 
#   set1[weatherSum.melt$Cluster[weatherSum.melt$variable == "mnT_mean_abs"]]
# 
# weatherSum.melt$alpha <- 0.9
# weatherSum.melt$alpha[weatherSum.melt$col == "#fccde5"] <- 1
# 
# # guide = FALSE idea:
# # - https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot/14604540
# locations$Type <- "Temperature"
# map8 <- ggplot(locations, aes(map_id = state)) +
#   # map points to the fifty_states shape data
#   geom_map(
#     fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), 
#     #fill = alpha("gray80", 1), color = alpha("gray60", 1), 
#     map = fifty_states) +
#   expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#   geom_point(aes(x = longitude2, y = latitude2, 
#                  color = factor(Cluster6)), size = .1) + 
#   geom_polygon(data = weatherSum.melt, 
#                aes(x = longitude2 + .025*value2*cos(dRad),
#                    y = latitude2 + .025*value2*sin(dRad),
#                    group = factor(group)),
#                color = alpha(weatherSum.melt$col, weatherSum.melt$alpha), 
#                fill = NA) + 
#   coord_fixed() +
#   facet_grid(~Type) + 
#   scale_color_manual(values =  pal2) + 
#   scale_x_continuous(breaks = NULL) +
#   scale_y_continuous(breaks = NULL) +
#   theme(legend.position = "none",
#         #panel.background = element_rect(fill = NA, color = "gray"),
#         panel.background = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         strip.text = element_text(size = 25),
#         plot.margin = unit(c(0, 0, 0, 0 ), "cm"))



utah.df <- fifty_states[fifty_states$id == "utah",]

individ_station_df_copy <- individ_station_df
individ_station_df_copy$STATE <- "utah"

tcoords <- mapproject(individ_station_df_copy$LONGITUDE, individ_station_df_copy$LATITUDE,
                      projection ="mercator", parameters=NULL, orientation=NULL)

tcoords
# weatherSum$longitude2 <- tcoords$x[coordDF$id == 2]
# weatherSum$latitude2 <- tcoords$y[coordDF$id == 2]
individ_station_df_copy$LATITUDE2 <- tcoords$y
individ_station_df_copy$LONGITUDE2 <- tcoords$x


ggplot(individ_station_df_copy, aes(map_id = STATE)) +
  # map points to the fifty_states shape data
  geom_map(
    fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5), 
    #fill = alpha("gray80", 1), color = alpha("gray60", 1), 
    map = utah.df) +
  expand_limits(x = utah.df$long, y = utah.df$lat) +
  geom_point(aes(x = LONGITUDE2, y = LATITUDE2))#, size = .2))#, 
                 #color = factor(Cluster6)), size = 0.1) + 
  geom_polygon(data = weatherSum, 
               aes(x = longitude2 + .025*sBSS*cos(dRad),
                   y = latitude2 + .025*sBSS*sin(dRad),
                   group = factor(AirPtCd), color = factor(Cluster)), 
               fill = NA) + 
  facet_grid(~Type) + 
  coord_fixed() +
  scale_color_manual(values = alpha(pal2, 0.9)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none",
        #panel.background = element_rect(fill = NA, color = "gray"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 35),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  
  
  
  
  # 
  # 
  # ggplot(locations, aes(map_id = state)) +
  #     # map points to the fifty_states shape data
  #     geom_map(
  #       fill = alpha("gray80", 0.5), color = alpha("gray60", 0.5),
  #       #fill = alpha("gray80", 1), color = alpha("gray60", 1),
  #       map = fifty_states) +
  #     expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  #     geom_point(aes(x = longitude2, y = latitude2,
  #                    color = factor(Cluster6)), size = .1)
