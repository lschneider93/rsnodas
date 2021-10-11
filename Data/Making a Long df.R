################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
try <- station_info_copy[, c("april_2004_WESD", "april_2005_WESD",
                             "april_2006_WESD", "april_2007_WESD",
                             "april_2008_WESD", "april_2009_WESD",
                             "april_2010_WESD", "april_2011_WESD",
                             "april_2012_WESD", "april_2013_WESD",
                             "april_2014_WESD", "april_2015_WESD",
                             "april_2016_WESD", #"april_2017_WESD",
                             "april_2018_WESD", "april_2019_WESD",
                             "april_2020_WESD", "april_2021_WESD")]
test <- data.frame(WESD = unlist(try))

# station ID vector
station_long <- rep(station_info_copy$ID, times = length(years))
test$ID <- station_long

# elevation vector
elevation_long <- rep(station_info_copy$ELEVATION, times = length(years))
test$ELEVATION <- elevation_long

# latitude vector
lat_vector <- rep(station_info_copy$LATITUDE, times = length(years))
test$LATITUDE <- lat_vector

# longitude vector
long_vector <- rep(station_info_copy$LONGITUDE, times = length(years))
test$LONGITUDE <- long_vector

#years vector
y <- rep(years, each = length(station_info_copy$ID))
test$year <- y
test$year <- factor(test$year,
                    levels = c('2004','2005', '2006',
                               '2007','2008', '2009',
                               '2010','2011', '2012',
                               '2013','2014', '2015',
                               '2016',#'2017',
                               '2018',
                               '2019','2020', '2021'))







###################
try <- station_info_copy[, c("april_2004_MODEL", "april_2005_MODEL",
                             "april_2006_MODEL", "april_2007_MODEL",
                             "april_2008_MODEL", "april_2009_MODEL",
                             "april_2010_MODEL", "april_2011_MODEL",
                             "april_2012_MODEL", "april_2013_MODEL",
                             "april_2014_MODEL", "april_2015_MODEL",
                             "april_2016_MODEL", #"april_2017_MODEL",
                             "april_2018_MODEL", "april_2019_MODEL",
                             "april_2020_MODEL", "april_2021_MODEL")]
test1 <- data.frame(MODEL = unlist(try))
test$MODEL <- test1$MODEL


###################
try <- station_info_copy[, c("april_2004_DIFFERENCE", "april_2005_DIFFERENCE",
                             "april_2006_DIFFERENCE", "april_2007_DIFFERENCE",
                             "april_2008_DIFFERENCE", "april_2009_DIFFERENCE",
                             "april_2010_DIFFERENCE", "april_2011_DIFFERENCE",
                             "april_2012_DIFFERENCE", "april_2013_DIFFERENCE",
                             "april_2014_DIFFERENCE", "april_2015_DIFFERENCE",
                             "april_2016_DIFFERENCE", #"april_2017_DIFFERENCE",
                             "april_2018_DIFFERENCE", "april_2019_DIFFERENCE",
                             "april_2020_DIFFERENCE", "april_2021_DIFFERENCE")]
test2 <- data.frame(DIFFERENCE = unlist(try))
test$DIFFERENCE <- test2$DIFFERENCE

long_df <- test
long_df_copy <- test

long_df1 <- test[complete.cases(test), ] 

?data.frame(0)
sp_all1 <- ggplot(data = test, aes(x = ELEVATION, y = DIFFERENCE)) + 
  geom_point(shape = 1) #+ 
#facet_wrap(~ year, nrow = 3)

sp_all1
################################################################################
sp_all2 <- ggplot(data = long_df1, aes(x = ELEVATION, y = DIFFERENCE)) + 
  geom_point(shape = 1) #+ 
#facet_wrap(~ year, nrow = 3)

sp_all2





################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################








################################################################################






################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################




################################################################################







################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
################################################################################





################################################################################



################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
