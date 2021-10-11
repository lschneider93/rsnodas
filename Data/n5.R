g <- vector("list", length(april_df_ghcnd))
v <- vector("list", length(april_df_ghcnd))
for (i in 1:length(april_df_ghcnd)){
  gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
                     locations = april_df_ghcnd_spatial[[i]])
  g[[i]] <- gs
  v1 <- gstat::variogram(gs)
  v[[i]] <- v1
}
# gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
#                    locations = april_df_ghcnd_spatial[[1]])
# 
# ?gstat::variogram()
# v1 <- gstat::variogram(gs)
# v
# plot(v)


for (i in 1:length(april_df_ghcnd)){
  print(plot(v[[i]]))
  Sys.sleep(1)
}

df_of_all_years[df_of_all_years$DATE == ]
gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
                   locations = april_df_ghcnd_spatial[[1]])

?gstat::variogram()
v1 <- gstat::variogram(gs)
v
plot(v)




?gstat::fit.variogram()
?gstat::vgm()
fve <- gstat::fit.variogram(v, gstat::vgm(85, "Exp", 75, 20))
fve
##   model    psill    range
## 1   Nug 21.96600  0.00000
## 2   Exp 85.52957 72.31404
plot(gstat::variogramLine(fve, 400), type='l', ylim=c(0,120))
points(v[,2:3], pch=20, col='red')



gs <- gstat::gstat(formula = DIFFERENCE ~ ANNUAL_precip ,
                   locations = april_df_ghcnd_spatial[[16]]$)

?gstat::variogram()
v <- gstat::variogram(gs)
v
plot(v)

?gstat::fit.variogram()
?gstat::vgm()
fve <- gstat::fit.variogram(v, gstat::vgm(85, "Exp", 75, 20))
fve
##   model    psill    range
## 1   Nug 21.96600  0.00000
## 2   Exp 85.52957 72.31404
plot(gstat::variogramLine(fve, 400), type='l', ylim=c(0,120))
points(v[,2:3], pch=20, col='red')


# gs
# ?interpolate()
# idw <- raster::interpolate(prism, gs)
# idw <- raster::mask(idw, new_boundary)
# raster::plot(idw)





# utah_april_error_rasters <- vector("list", length(years))
# for (i in 1:length(years)){
#   gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
#                      locations = april_df_ghcnd_spatial[[i]])
#   idw <- raster::interpolate(prism, gs)
#   idw <- raster::mask(idw, new_boundary)
#   utah_april_error_rasters[[i]] <- idw
#   
# }

# 
# 
# gs <- gstat::gstat(formula = DIFFERENCE ~ 1 ,
#                    locations = april_df_ghcnd_spatial[[16]])
# 
# ?gstat::variogram()
# v <- gstat::variogram(gs)
# 
# plot(v)
# 
# ?gstat::fit.variogram()
# ?gstat::vgm()
# fve <- gstat::fit.variogram(v, gstat::vgm(85, "Exp", 75, 20))
# fve
# ##   model    psill    range
# ## 1   Nug 21.96600  0.00000
# ## 2   Exp 85.52957 72.31404
# plot(gstat::variogramLine(fve, 400), type='l', ylim=c(0,120))
# points(v[,2:3], pch=20, col='red')




k <-  gstat(formula = OZDLYAV ~ 1,
           locations = aq, 
           model = fve)
# predicted values
kp <- predict(k, g)
## [using ordinary kriging]
spplot(kp)

