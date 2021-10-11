################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
colnames(station_info_copy)
try <- station_info_copy[, c("april_2004_WESD",
                        "april_2005_WESD",
                        "april_2006_WESD",
                        "april_2007_WESD",
                        "april_2008_WESD",
                        "april_2009_WESD",
                        "april_2010_WESD",
                        "april_2011_WESD",
                        "april_2012_WESD",
                        "april_2013_WESD",
                        "april_2014_WESD",
                        "april_2015_WESD",
                        "april_2016_WESD",
                        #"april_2017_WESD",
                        "april_2018_WESD",
                        "april_2019_WESD",
                        "april_2020_WESD",
                        "april_2021_WESD")]


#rep(c(1,2,3,4,5), times = 2)
#rep(c(1,2,3,4,5), each = 3)
?rep
years
test <- data.frame(y = unlist(try))
y <- rep(years, each = length(station_info_copy$ID))

station_long <- rep(station_info_copy$ID, times = length(years))
elevation_long <- rep(station_info_copy$ELEVATION, times = length(years))


dim(test)
length(station_long)

test$ID <- station_long
test$year <- y
test$ELEVATION <- elevation_long
test$year <- factor(test$year,
            levels = c('2004','2005', '2006',
                       '2007','2008', '2009',
                       '2010','2011', '2012',
                       '2013','2014', '2015',
                       '2016',#'2017',
                       '2018',
                       '2019','2020', '2021'))


sp_all <-ggplot(data = test, aes(x = ELEVATION, y = y)) + 
  geom_point(shape = 1) + 
  facet_wrap(~ year, nrow = 3)
sp_all

#ggplot(data = test, aes(x = ELEVATION, y = DIFFERENCE, colour = ID)) + 
#  geom_point(shape = 1) + 
#  facet_wrap(~ site. nrow = 18)



try <- stations_2ob[, c("april_2004_WESD",
                        "april_2005_WESD",
                        "april_2006_WESD",
                        "april_2007_WESD",
                        "april_2008_WESD",
                        "april_2009_WESD",
                        "april_2010_WESD",
                        "april_2011_WESD",
                        "april_2012_WESD",
                        "april_2013_WESD",
                        "april_2014_WESD",
                        "april_2015_WESD",
                        "april_2016_WESD",
                        #"april_2017_WESD",
                        "april_2018_WESD",
                        "april_2019_WESD",
                        "april_2020_WESD",
                        "april_2021_WESD")]


#rep(c(1,2,3,4,5), times = 2)
#rep(c(1,2,3,4,5), each = 3)
?rep
test <- data.frame(y = unlist(try))

y <- rep(years, each = length(stations_2ob$ID))

station_long <- rep(stations_2ob$ID, times = length(years))
elevation_long <- rep(stations_2ob$ELEVATION, times = length(years))


dim(test)
length(station_long)

test$ID <- station_long
test$year <- y
test$ELEVATION <- elevation_long
test$year <- factor(test$year,
                    levels = c('2004','2005', '2006',
                               '2007','2008', '2009',
                               '2010','2011', '2012',
                               '2013','2014', '2015',
                               '2016',#'2017',
                               '2018',
                               '2019','2020', '2021'))


sp_wobs <- ggplot(data = test, aes(x = ELEVATION, y = y)) + 
  geom_point(shape = 1) + 
  facet_wrap(~ year, nrow = 3)

sp_wobs

#geom_smooth()

test[(142*17):(142*17)+1, ]
tail(test)
length(years)




################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
station_info_copy


################################################################################
########  Plotting the difference of those stations that have data    ##########
################################################################################
stations_2ob

