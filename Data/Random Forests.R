# ################################################################################
# #### Making a List of all the Stations that have 5 observations ######
# ################################################################################
# # install.packages("caret")
# library(caret)
# set.seed(3456)
# 
# random_df <- individ_station_df[sample(nrow(individ_station_df)), ] 
# 
# random_df5 <- individ_station_df5[sample(nrow(individ_station_df5)), ] 
# 
# dim(random_df)
# dim(test)
# train <- random_df[ 1:1295, ]
# test <- random_df[1296:1850, ]
# 
# 
# try <- ifelse(train$AVE_BIAS >= -25 & train$AVE_BIAS < 25, 0, train$AVE_BIAS )
# try <- ifelse(train$AVE_BIAS >= 25 & train$AVE_BIAS < 75, 1, try)
# try <- ifelse(train$AVE_BIAS >= 75, 2, try)
# try <- ifelse(train$AVE_BIAS >= -75 & train$AVE_BIAS < -25, -1, try)
# try <- ifelse(train$AVE_BIAS < -75, -2, try)
# table(train$AVE_BIAS == 0)
# table(try)
# 
# train$CLASS2 <- try
# 
# try <- ifelse(individ_station_df$AVE_BIAS >= -25 & individ_station_df$AVE_BIAS < 25, 0, individ_station_df$AVE_BIAS )
# try <- ifelse(individ_station_df$AVE_BIAS >= 25 & individ_station_df$AVE_BIAS < 75, 1, try)
# try <- ifelse(individ_station_df$AVE_BIAS >= 75, 2, try)
# try <- ifelse(individ_station_df$AVE_BIAS >= -75 & individ_station_df$AVE_BIAS < -25, -1, try)
# try <- ifelse(individ_station_df$AVE_BIAS < -75, -2, try)
# table(individ_station_df$AVE_BIAS == 0)
# table(try)
# 
# individ_station_df$CLASS2 <- try
# ################################################################################
# #### Making a List of all the Stations that have 5 observations ######
# ################################################################################
# 
# library(rpart)
# install.packages("rattle")
# individ_station_df5
# 
# ?rpart()
# ct <- rpart::rpart(as.factor(CLASS2) ~ ELEVATION + LONGITUDE + LATITUDE + 
#                      ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                      ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                      ANNUAL_maxvapor, data = train)
# 
# ct2 <- rpart::rpart(as.factor(CLASS) ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
#                      MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
#                      MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
#                      MARCH_maxvapor, data = train)
# 
# ct3 <- rpart::rpart(as.factor(CLASS) ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
#                       FEB_maxtemp + FEB_precip + FEB_minvapor + 
#                       FEB_mintemp + FEB_meantemp + FEB_meandew +
#                       FEB_maxvapor, data = train)
# 
# c <-summary(ct)
# c <- summary(ct2)
# c <- summary(ct3)
# c$variable.importance
# rattle::fancyRpartPlot(ct, main = "trial")
# 
# ct_test <- predict(ct, test[, -68])
# ct_test <- as.data.frame(ct_test)
# ct_test <- round(ct_test)
# 
# ct_test[, 3]
# try <- ifelse(ct_test[, 3] == 1, 1, 0)
# try <- ifelse(ct_test[, 1] == 1, -1, try)
# try <- factor(try, levels = c('-1', '0', '1'))
# caret::confusionMatrix(try, as.factor(test$CLASS))
# 
# rpart::plotcp(ct)
# plotcp(ct)
# 
# 
# ################################################################################
# #### Making a List of all the Stations that have 5 observations ######
# ################################################################################
# #install.packages("randomForest")
# rf <- randomForest::randomForest(as.factor(CLASS2) ~ ELEVATION + LONGITUDE + LATITUDE + 
#                ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                ANNUAL_maxvapor, data = train, importance = TRUE)
# 
# rf2 <- randomForest::randomForest(as.factor(CLASS) ~ ELEVATION + 
#                                     MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
#                                     MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
#                                     MARCH_maxvapor, data = train)
# 
# rf3 <- randomForest::randomForest(as.factor(CLASS) ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
#                                     FEB_maxtemp + FEB_precip + FEB_minvapor + 
#                                     FEB_mintemp + FEB_meantemp + FEB_meandew +
#                                     FEB_maxvapor, data = train)
# 
# train
# rf
# rf_train <- predict(rf2, train[, -68])
# caret::confusionMatrix(rf_train, as.factor(train$CLASS))
# 
# rf_test <- predict(rf2, test[, -68])
# caret::confusionMatrix(rf_test, as.factor(test$CLASS))
# 
# rf$importance
#  
# ################################################################################
# #### Making a List of all the Stations that have 5 observations ######
# ################################################################################
# class(train$AVE_BIAS)
# library(dplyr)
# library(tidyverse)
# 
# train_group <- train %>% group_by(ID) %>%
#   summarize(ELEVATION = first(ELEVATION), 
#             AVE_BIAS = first(AVE_BIAS),
#             LONGITUDE = first(LONGITUDE),
#             LATITUDE = first(LATITUDE),
#             ANNUAL_maxtemp = first(ANNUAL_maxtemp) ,
#             ANNUAL_precip = first(ANNUAL_precip) ,
#             ANNUAL_minvapor = first(ANNUAL_minvapor),
#             ANNUAL_mintemp  = first(ANNUAL_mintemp),
#             ANNUAL_meantemp = first(ANNUAL_meantemp),
#             ANNUAL_meandew = first(ANNUAL_meandew) ,
#             ANNUAL_maxvapor = first(ANNUAL_maxvapor),
#             CLASS = first(CLASS2))
# 
# test_group <- test %>% group_by(ID) %>%
#   summarize(ELEVATION = first(ELEVATION), 
#             AVE_BIAS = first(AVE_BIAS),
#             LONGITUDE = first(LONGITUDE),
#             LATITUDE = first(LATITUDE),
#             ANNUAL_maxtemp = first(ANNUAL_maxtemp) ,
#             ANNUAL_precip = first(ANNUAL_precip) ,
#             ANNUAL_minvapor = first(ANNUAL_minvapor),
#             ANNUAL_mintemp  = first(ANNUAL_mintemp),
#             ANNUAL_meantemp = first(ANNUAL_meantemp),
#             ANNUAL_meandew = first(ANNUAL_meandew) ,
#             ANNUAL_maxvapor = first(ANNUAL_maxvapor),
#             CLASS = first(CLASS2))
# 
# rf <- randomForest::randomForest(AVE_BIAS ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = train_group, importance = TRUE)
# 
# rf <- randomForest::randomForest(as.factor(CLASS) ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = train_group, importance = TRUE)
# 
# rf2 <- randomForest::randomForest(DIFFERENCE ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
#                                     MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
#                                     MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
#                                     MARCH_maxvapor, data = train)
# 
# rf3 <- randomForest::randomForest(DIFFERENCE ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
#                                     FEB_maxtemp + FEB_precip + FEB_minvapor + 
#                                     FEB_mintemp + FEB_meantemp + FEB_meandew +
#                                     FEB_maxvapor, data = train)
# View(train)
# rf$predicted[rf$predicted > 200]
# randomForest::varImpPlot(rf)
# rf$predicted
# table(train_group$CLASS)
# 
# rf_train <- predict(rf, train[, -68])
# caret::confusionMatrix(rf_train, as.factor(train$CLASS))
# 
# rf_test <- predict(rf, test[, -68])
# caret::confusionMatrix(rf_test, test$DIFFERENCE)
# 
# try <- rf_test - test$DIFFERENCE
# round(try, digits = 2)
# rf$importance
# 
# dim(test_group)
# dim(train_group)
# rf_test <- predict(rf, test_group[, -13])
# caret::confusionMatrix(rf_test, as.factor(test_group$CLASS))



################################################################################
#### I commented everything before this ######
#### Making a List of all the Stations that have 5 obs per year ######
################################################################################

random_df <- individ_station_df[sample(nrow(individ_station_df)), ]

library(dplyr)
library(tidyverse)

april_df_ghcnd

individ_years <- vector("list", length(years))
# for (i in 1:length(years)){
#   individ_years[[i]] <- random_df[random_df$DATE == paste0(years[i],"-04-01"), ]
# }

individ_station_df

for (i in years){
  individ_years[[(i- years[1] +1)]] <- individ_station_df[individ_station_df$DATE == paste0(i,"-04-01"), ]
}
# for (i in years){
#   individ_years[[(i- years[1] +1)]] <- random_df[random_df$DATE == paste0(i,"-04-01"), ]
# }

individ_years[[10]]
individ_years <- individ_years[vapply(individ_years, Negate(is.null), NA)]
# y04 <- random_df[random_df$DATE == "2004-04-01", ]
# y05 <- random_df[random_df$DATE == "2005-04-01", ]
# y06 <- random_df[random_df$DATE == "2006-04-01", ]
# y07 <- random_df[random_df$DATE == "2007-04-01", ]
# y08 <- random_df[random_df$DATE == "2008-04-01", ]
# y09 <- random_df[random_df$DATE == "2009-04-01", ]
# y10 <- random_df[random_df$DATE == "2010-04-01", ]
# y11 <- random_df[random_df$DATE == "2011-04-01", ]
# y12 <- random_df[random_df$DATE == "2012-04-01", ]
# y13 <- random_df[random_df$DATE == "2013-04-01", ]
# y14 <- random_df[random_df$DATE == "2014-04-01", ]
# y15 <- random_df[random_df$DATE == "2015-04-01", ]
# y16 <- random_df[random_df$DATE == "2016-04-01", ]
# # y17 <- random_df[random_df$DATE == "2017-04-01", ]
# y18 <- random_df[random_df$DATE == "2018-04-01", ]
# y19 <- random_df[random_df$DATE == "2019-04-01", ]
# y20 <- random_df[random_df$DATE == "2020-04-01", ]
# y21 <- random_df[random_df$DATE == "2021-04-01", ]
y04 <- individ_station_df[individ_station_df$DATE == "2004-04-01", ]
y05 <- individ_station_df[individ_station_df$DATE == "2005-04-01", ]
y06 <- individ_station_df[individ_station_df$DATE == "2006-04-01", ]
y07 <- individ_station_df[individ_station_df$DATE == "2007-04-01", ]
y08 <- individ_station_df[individ_station_df$DATE == "2008-04-01", ]
y09 <- individ_station_df[individ_station_df$DATE == "2009-04-01", ]
y10 <- individ_station_df[individ_station_df$DATE == "2010-04-01", ]
y11 <- individ_station_df[individ_station_df$DATE == "2011-04-01", ]
y12 <- individ_station_df[individ_station_df$DATE == "2012-04-01", ]
y13 <- individ_station_df[individ_station_df$DATE == "2013-04-01", ]
y14 <- individ_station_df[individ_station_df$DATE == "2014-04-01", ]
y15 <- individ_station_df[individ_station_df$DATE == "2015-04-01", ]
y16 <- individ_station_df[individ_station_df$DATE == "2016-04-01", ]
# y17 <- individ_station_df[individ_station_df$DATE == "2017-04-01", ]
y18 <- individ_station_df[individ_station_df$DATE == "2018-04-01", ]
y19 <- individ_station_df[individ_station_df$DATE == "2019-04-01", ]
y20 <- individ_station_df[individ_station_df$DATE == "2020-04-01", ]
y21 <- individ_station_df[individ_station_df$DATE == "2021-04-01", ]


any(individ_years[[1]] != y04)
length(y05$ID)

random_forest_march_ave <- vector("list", length(individ_years))
random_forest_annual_ave <- vector("list", length(individ_years))
random_forest_march_diff <- vector("list", length(individ_years))
random_forest_annual_diff <- vector("list", length(years))

svm_march_ave <- vector("list", length(individ_years))
svm_annual_ave <- vector("list", length(individ_years))
svm_march_diff <- vector("list", length(individ_years))
svm_annual_diff <- vector("list", length(years))

for(i in 1:length(years)){
  rf <- try(randomForest::randomForest(AVE_BIAS ~ ELEVATION + LONGITUDE + LATITUDE + 
                                         MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                         MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                         MARCH_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE )
  random_forest_march_ave[[i]] <- rf
  
  rf4 <- try(randomForest::randomForest(AVE_BIAS ~ ELEVATION + LATITUDE + LONGITUDE + 
                                      ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
                                      ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
                                      ANNUAL_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE)
  random_forest_annual_ave[[i]] <- rf4
  
  rf <- try(randomForest::randomForest(DIFFERENCE ~ ELEVATION + LONGITUDE + LATITUDE + 
                                         MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                         MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                         MARCH_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE )
  random_forest_march_diff[[i]] <- rf
  
  rf4 <- try(randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
                                          ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
                                          ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
                                          ANNUAL_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE)
  random_forest_annual_diff[[i]] <- rf4
  
  svm.model_m <- try(e1071::svm(AVE_BIAS ~ ELEVATION + LONGITUDE + LATITUDE + 
                                  MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                  MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                  MARCH_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE )
  svm_march_ave[[i]] <- svm.model_m
  
  svm.model_a <- try(e1071::svm(AVE_BIAS ~ ELEVATION + LATITUDE + LONGITUDE + 
                                  ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
                                  ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
                                  ANNUAL_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE)
  svm_annual_ave[[i]] <- svm.model_a
  
  svm.model_m <- try(e1071::svm(DIFFERENCE ~ ELEVATION + LONGITUDE + LATITUDE + 
                                  MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                  MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                  MARCH_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE )
  svm_march_diff[[i]] <- svm.model_m
  
  svm.model_a <- try(e1071::svm(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
                                  ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
                                  ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
                                  ANNUAL_maxvapor, data = individ_years[[i]], importance = TRUE), silent = TRUE)
  svm_annual_diff[[i]] <- svm.model_a
}

# rf4 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[1]], importance = TRUE)
# 
# rf5 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[2]], importance = TRUE)
# rf6 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[3]], importance = TRUE)
# rf7 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[4]], importance = TRUE)
# rf8 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[5]], importance = TRUE)
# rf9 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[6]], importance = TRUE)
# rf10 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                    ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                    ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                    ANNUAL_maxvapor, data = individ_years[[7]], importance = TRUE)
# rf11 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LATITUDE + LONGITUDE + 
#                                     ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
#                                     ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
#                                     ANNUAL_maxvapor, data = individ_years[[8]], importance = TRUE)
# 


rf_1 <- randomForest::randomForest(DIFFERENCE ~ ELEVATION + LONGITUDE + LATITUDE + 
                                    MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                    MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                    MARCH_maxvapor, data = individ_years[[1]], importance = TRUE)


rf2 <- randomForest::randomForest(as.factor(CLASS) ~ ELEVATION + LATITUDE + LONGITUDE + 
                                   ANNUAL_maxtemp + ANNUAL_precip + ANNUAL_minvapor + 
                                   ANNUAL_mintemp + ANNUAL_meantemp + ANNUAL_meandew +
                                   ANNUAL_maxvapor, data = individ_years[[1]], importance = TRUE)

rf2_2 <- randomForest::randomForest(as.factor(CLASS) ~ ELEVATION + LATITUDE + LONGITUDE + 
                                      MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                      MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                      MARCH_maxvapor, data = individ_years[[1]], importance = TRUE)

rf3 <- randomForest::randomForest(AVE_BIAS ~ DATE + ELEVATION + LONGITUDE + LATITUDE + 
                                    MARCH_maxtemp + MARCH_precip + MARCH_minvapor + 
                                    MARCH_mintemp + MARCH_meantemp + MARCH_meandew +
                                    MARCH_maxvapor, data = individ_years[[1]], importance = TRUE)

rf
rf2
rf3
rf_1
rf2_2

range(individ_years[[1]]$DIFFERENCE)

randomForest::varImpPlot(rf)
randomForest::varImpPlot(rf2)
randomForest::varImpPlot(rf3)
randomForest::varImpPlot(rf_1)
randomForest::varImpPlot(rf2_2)

rf
randomForest::varImpPlot(random_forest[[1]])
randomForest::varImpPlot(random_forest[[2]])
randomForest::varImpPlot(random_forest[[3]])
randomForest::varImpPlot(random_forest[[4]])
randomForest::varImpPlot(random_forest[[5]])
randomForest::varImpPlot(random_forest[[6]])
randomForest::varImpPlot(random_forest[[7]])
randomForest::varImpPlot(random_forest[[8]])
randomForest::varImpPlot(random_forest[[9]])
randomForest::varImpPlot(random_forest[[10]])
randomForest::varImpPlot(random_forest[[11]])
randomForest::varImpPlot(random_forest[[12]])
randomForest::varImpPlot(random_forest[[13]])
randomForest::varImpPlot(random_forest[[14]])
randomForest::varImpPlot(random_forest[[15]])
randomForest::varImpPlot(random_forest[[16]])
randomForest::varImpPlot(random_forest[[17]])
randomForest::varImpPlot(random_forest[[18]])


# random_forest_march_ave <- vector("list", length(individ_years))
# random_forest_annual_ave <- vector("list", length(individ_years))
# random_forest_march_diff <- vector("list", length(individ_years))
# random_forest_annual_diff <- vector("list", length(individ_years))
# 
# svm_march_ave <- vector("list", length(individ_years))
# svm_annual_ave <- vector("list", length(individ_years))
# svm_march_diff <- vector("list", length(individ_years))
# svm_annual_diff <- vector("list", length(individ_years))
 
pdp_a_elevation <- vector("list", length(individ_years))
pdp_a_longitude <- vector("list", length(individ_years))
pdp_a_latitude <- vector("list", length(individ_years))
pdp_a_precip <- vector("list", length(individ_years))
pdp_a_minvapor <- vector("list", length(individ_years))
pdp_a_mintemp <- vector("list", length(individ_years))
pdp_a_meantemp <- vector("list", length(individ_years))
pdp_a_meandew <- vector("list", length(individ_years))
pdp_a_maxvapor  <- vector("list", length(individ_years))
pdp_a_maxtemp <- vector("list", length(individ_years))

partial(random_forest_annual_diff[[1]], pred.var = c("ELEVATION"), chull = TRUE)

library(pdp)
library(ggplot2)
for (i in 1:length(individ_years)){
  # ELEVATION
  rf.elevation_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ELEVATION"), chull = TRUE)
  pdp_a_elevation[[i]] <- autoplot(rf.elevation_annual , contour = TRUE)
  # plot.rf.elevation_annual <- autoplot(rf.elevation_annual , contour = TRUE)
  # 
  # rf.elevation_march <- partial(random_forest_march_diff[[i]], pred.var = c("ELEVATION"), chull = TRUE)
  # plot.rf.elevation_march <- autoplot(rf.elevation_march , contour = TRUE)
  
  # LATITUDE
  rf.latitude_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("LATITUDE"), chull = TRUE)
  pdp_a_latitude[[i]] <- autoplot(rf.latitude_annual , contour = TRUE)
  
  # LONGITUDE
  rf.longitude_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE"), chull = TRUE)
  pdp_a_longitude[[i]] <- autoplot(rf.longitude_annual , contour = TRUE)
  
  # ANNUAL_precip
  rf.precip_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_precip"), chull = TRUE)
  pdp_a_precip[[i]] <- autoplot(rf.precip_annual , contour = TRUE)
  
  # ANNUAL_minvapor
  rf.lminvapor_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_minvapor"), chull = TRUE)
  pdp_a_minvapor[[i]] <- autoplot(rf.lminvapor_annual , contour = TRUE)
  
  # ANNUAL_mintemp
  rf.mintemp_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_mintemp"), chull = TRUE)
  pdp_a_mintemp[[i]] <- autoplot(rf.mintemp_annual , contour = TRUE)
   
    # ANNUAL_meantemp
  rf.meantemp_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_meantemp"), chull = TRUE)
  pdp_a_meantemp[[i]] <- autoplot(rf.meantemp_annual , contour = TRUE)

  # ANNUAL_meandew
  rf.meandew_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_meandew"), chull = TRUE)
  pdp_a_meandew[[i]] <- autoplot(rf.meandew_annual , contour = TRUE)
   
  # ANNUAL_maxvapor
  rf.maxvapor_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_maxvapor"), chull = TRUE)
  pdp_a_maxvapor[[i]] <- autoplot(rf.maxvapor_annual , contour = TRUE)
    
  # ANNUAL_maxtemp
  rf.maxtemp_annual <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_maxtemp"), chull = TRUE)
  pdp_a_maxtemp[[i]] <- autoplot(rf.maxtemp_annual , contour = TRUE)
  
  # Two Variables
  # rf.lat.long_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE", "LATITUDE"), chull = TRUE)
  # plot.rf.lat.long_annual <- autoplot(rf.lat.long_annual, contour = TRUE, 
  #                                     legend.title = "Partial\ndependence")
  # 
  # pdp[[i]] <- grid.arrange(plot.rf.latitude_annual, plot.rf.longitude_annual, plot.rf.elevation_annual, plot.rf.lat.long_annual)
}

# IDENTIFY YEAR OVER YEAR BIAS, in DIMENSION

p <- ggplot(data = rf.latitude_annual, aes(x = ANNUAL_maxtemp, y = yhat)) 
# Basic line plot with points
p + geom_line() 


  


for (i in 1:length(individ_years)){
  rf.elevation_annual <- partial(random_forest_annual_diff[[1]], pred.var = c("ANNUAL_precip"), chull = TRUE)
  plot.rf.elevation_annual <- autoplot(rf.elevation_annual , contour = TRUE)
  
  # LATITUDE
  rf.latitude_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LATITUDE"), chull = TRUE)
  plot.rf.latitude_annual <- autoplot(rf.latitude_annual , contour = TRUE)
  
  # LONGITUDE
  rf.longitude_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE"), chull = TRUE)
  plot.rf.longitude_annual <- autoplot(rf.longitude_annual , contour = TRUE)
  
  # Two Variables
  rf.lat.long_annual <- partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE", "LATITUDE"), chull = TRUE)
  plot.rf.lat.long_annual <- autoplot(rf.lat.long_annual, contour = TRUE, 
                                      legend.title = "Partial\ndependence")
  
  
  pdp[[i]] <- grid.arrange(plot.rf.latitude_annual, plot.rf.longitude_annual, plot.rf.elevation_annual, plot.rf.lat.long_annual)
  
}

#### Random Forest using the DIFFERENCE
# Elevation
rf.elevation_annual <- partial(random_forest_annual_diff[[1]], pred.var = c("ELEVATION"), chull = TRUE)
plot.rf.elevation_annual <- autoplot(rf.elevation_annual , contour = TRUE)

rf.elevation_march <- partial(random_forest_march_diff[[1]], pred.var = c("ELEVATION"), chull = TRUE)
plot.rf.elevation_march <- autoplot(rf.elevation_march , contour = TRUE)


# LATITUDE
rf.latitude_annual <- partial(random_forest_annual_diff[[1]], pred.var = c("LATITUDE"), chull = TRUE)
plot.rf.latitude_annual <- autoplot(rf.latitude_annual , contour = TRUE)

rf.latitude_march <- partial(random_forest_march_diff[[1]], pred.var = c("LATITUDE"), chull = TRUE)
plot.rf.latitude_march <- autoplot(rf.latitude_march , contour = TRUE)

# LONGITUDE
rf.longitude_annual <- partial(random_forest_annual_diff[[1]], pred.var = c("LONGITUDE"), chull = TRUE)
plot.rf.longitude_annual <- autoplot(rf.longitude_annual , contour = TRUE)

rf.longitude_march <- partial(random_forest_march_diff[[1]], pred.var = c("LONGITUDE"), chull = TRUE)
plot.rf.longitude_march <- autoplot(rf.longitude_march , contour = TRUE)


# Two Variables
rf.lat.long_annual <- partial(random_forest_annual_diff[[1]], pred.var = c("LONGITUDE", "LATITUDE"), chull = TRUE)
plot.rf.lat.long_annual <- autoplot(rf.lat.long_annual, contour = TRUE, 
                                  legend.title = "Partial\ndependence")


pdp[[1]] <- grid.arrange(plot.rf.latitude_annual, plot.rf.longitude_annual, plot.rf.elevation_annual, plot.rf.lat.long_annual)

#
# ##### AVE
# rf.elevation_annual <- partial(random_forest_annual_ave[[1]], pred.var = c("ELEVATION"), chull = TRUE)
# plot.rf.elevation_annual <- autoplot(rf.elevation_annual , contour = TRUE)
# 
# rf.elevation_march <- partial(random_forest_march_ave[[1]], pred.var = c("ELEVATION"), chull = TRUE)
# plot.rf.elevation_march <- autoplot(rf.elevation_march , contour = TRUE)
# 
# 
# # LATITUDE
# rf.latitude_annual <- partial(random_forest_annual_ave[[1]], pred.var = c("LATITUDE"), chull = TRUE)
# plot.rf.latitude_annual <- autoplot(rf.latitude_annual , contour = TRUE)
# 
# rf.latitude_march <- partial(random_forest_march_ave[[1]], pred.var = c("LATITUDE"), chull = TRUE)
# plot.rf.latitude_march <- autoplot(rf.latitude_march , contour = TRUE)
# 
# # LONGITUDE
# rf.longitude_annual <- partial(random_forest_annual_ave[[1]], pred.var = c("LONGITUDE"), chull = TRUE)
# plot.rf.longitude_annual <- autoplot(rf.longitude_annual , contour = TRUE)
# 
# rf.longitude_march <- partial(random_forest_march_ave[[1]], pred.var = c("LONGITUDE"), chull = TRUE)
# plot.rf.longitude_march <- autoplot(rf.longitude_march , contour = TRUE)
# 
# 
# # Two Variables
# rf.lat.long_annual <- partial(random_forest_annual_ave[[1]], pred.var = c("LONGITUDE", "LATITUDE"), chull = TRUE)
# plot.rf.lat.long_annual <- autoplot(rf.lat.long_annual, contour = TRUE, 
#                                     legend.title = "Partial\ndependence")
# # grid.arrange(plot.Petal_W, plot.Sepal_W, plot.Petal_L, plot.Sepal_L, plot.rf.lat.long_annual)
# 
# grid.arrange(plot.rf.latitude_annual, plot.rf.longitude_annual, plot.rf.elevation_annual, plot.rf.lat.long_annual)




### SVM
# elevation
svm.elevation_annual <- partial(svm_annual_ave[[1]], pred.var = c("ELEVATION"), chull = TRUE)
plot.svm.elevation_annual <- autoplot(svm.elevation_annual , contour = TRUE)

svm.elevation_march <- partial(svm_march_ave[[1]], pred.var = c("ELEVATION"), chull = TRUE)
plot.svm.elevation_march <- autoplot(par.elevation_march , contour = TRUE)

# latitude
svm.latitude_annual <- partial(svm_annual_ave[[1]], pred.var = c("LATITUDE"), chull = TRUE)
plot.svm.latitude_annual <- autoplot(svm.elevation_annual , contour = TRUE)

svm.latitude_march <- partial(svm_march_ave[[1]], pred.var = c("LATITUDE"), chull = TRUE)
plot.svm.latitude_march <- autoplot(par.elevation_march , contour = TRUE)

# longitude


# Single Variable
# par.Petal_L <- partial(model.svm, pred.var = c("Petal.Length"), chull = TRUE)
# plot.Petal_L <- autoplot(par.Petal_L, contour = TRUE)
# par.Petal_L2 <- partial(model.rf, pred.var = c("Petal.Length"), chull = TRUE)
# plot.Petal_L2 <- autoplot(par.Petal_L2, contour = TRUE)
# 
# # Single Variable
# par.Sepal_L <- partial(model.svm, pred.var = c("Sepal.Length"), chull = TRUE)
# plot.Sepal_L <- autoplot(par.Sepal_L , contour = TRUE)
# par.Sepal_L2 <- partial(model.rf, pred.var = c("Sepal.Length"), chull = TRUE)
# plot.Sepal_L2 <- autoplot(par.Sepal_L2 , contour = TRUE)
# 
# # Two Variables
# par.Petal_W.Sepal_W <- partial(model.svm, pred.var = c("Petal.Width", "Sepal.Width"), chull = TRUE)
# plot.Petal_W.Sepal_W <- autoplot(par.Petal_W.Sepal_W, contour = TRUE, 
#                                  legend.title = "Partial\ndependence")

grid.arrange(plot.Petal_W, plot.Sepal_W, plot.Petal_L, plot.Sepal_L, plot.Petal_W.Sepal_W)

par.Petal_W2.Sepal_W2 <- partial(model.rf, pred.var = c("Petal.Width", "Sepal.Width"), chull = TRUE)
plot.Petal_W2.Sepal_W2 <- autoplot(par.Petal_W2.Sepal_W2, contour = TRUE, 
                                   legend.title = "Partial\ndependence")

grid.arrange(plot.Petal_W2, plot.Sepal_W2, plot.Petal_L2, plot.Sepal_L2, plot.Petal_W2.Sepal_W2)
















range(individ_station_df$AVE_BIAS)
plot(individ_station_df$AVE_BIAS)
random_forest_annual_diff[vapply(utah_april_swe_rasters, Negate(is.null), NA)]

random_forest_annual_diff[-14]
