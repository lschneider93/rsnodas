for (i in 1:length(individ_years)){
  # ELEVATION
  pdp_a_elevation[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ELEVATION"), chull = TRUE)
  
  # LATITUDE
  pdp_a_latitude[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("LATITUDE"), chull = TRUE)
   
  # LONGITUDE
  pdp_a_longitude[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("LONGITUDE"), chull = TRUE)
   
  # PRECIP
  pdp_a_precip[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_precip"), chull = TRUE)
  
  # MINVAPOR 
  pdp_a_minvapor[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_minvapor"), chull = TRUE)
  
  # MIN TEMP
  pdp_a_mintemp[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_mintemp"), chull = TRUE)
   
  # ANNUAL_meantemp
  pdp_a_meantemp[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_meantemp"), chull = TRUE)
  
  # ANNUAL_meandew
  pdp_a_meandew[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_meandew"), chull = TRUE)
  
  # ANNUAL_maxvapor
  pdp_a_maxvapor[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_maxvapor"), chull = TRUE)
   
  # ANNUAL_maxtemp
  pdp_a_maxtemp[[i]] <- pdp::partial(random_forest_annual_diff[[i]], pred.var = c("ANNUAL_maxtemp"), chull = TRUE)
  
}


data.frame(x = c(rf.elevation_annual$ELEVATION, rf.latitude_annual$LATITUDE, 
                 rf.longitude_annual$LONGITUDE, rf.precip_annual$ANNUAL_precip,
                 rf.lminvapor_annual$ANNUAL_minvapor, rf.mintemp_annual$ANNUAL_mintemp,
                 rf.meantemp_annual$ANNUAL_meantemp, rf.meandew_annual$ANNUAL_meandew,
                 rf.maxvapor_annual$ANNUAL_maxvapor, rf.maxtemp_annual$ANNUAL_maxtemp
                 ),
           y = c(rf.elevation_annual$yhat, rf.latitude_annual$yhat, 
                 rf.longitude_annual$yhat, rf.precip_annual$yhat,
                 rf.lminvapor_annual$yhat, rf.mintemp_annual$yhat,
                 rf.meantemp_annual$yhat, rf.meandew_annual$yhat,
                 rf.maxvapor_annual$yhat, rf.maxtemp_annual$yhat
           ),
           type = c(rep("ELEVATION",length(rf.elevation_annual$ELEVATION),
                    rep("LATITUDE",length(rf.latitude_annual$LATITUDE),
                    rep("LONGITUDE",length(rf.longitude_annual$LONGITUDE),
                    rep("ANNUAL_precip",length(rf.precip_annual$ANNUAL_precip),
                    rep("ANNUAL_minvapor",length(rf.lminvapor_annual$ANNUAL_minvapor),
                    rep("ANNUAL_mintemp",length(rf.mintemp_annual$ANNUAL_mintemp),
                    rep("ANNUAL_meantemp",length(rf.meantemp_annual$ANNUAL_meantemp),
                    rep("ANNUAL_meandew",length(rf.meandew_annual$ANNUAL_meandew),
                    rep("ANNUAL_maxvapor",length(rf.maxvapor_annual$ANNUAL_maxvapor),
                    rep("ANNUAL_maxtemp",length(rf.maxtemp_annual$ANNUAL_maxtemp),
                        )) )


rep("c", 3)
