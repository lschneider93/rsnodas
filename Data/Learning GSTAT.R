# Learning about gstat
####
library(sp)
#sp::
#data(meuse), meuse, class(meuse)
short_df
class(short_df)

# ?image(0)
#data(meuse.grid), meuse.grid, class(meuse.grid)
#summary(meuse), summary(meuse.grid) 

#
#sp::coordinates(meuse.grid) <- c("x", "y")
#sp::coordinates(meuse) <- c("x", "y")
sp::coordinates(long_df1) <- c("LONGITUDE", "LATITUDE")
sp::coordinates(short_df) <- c("LONGITUDE", "LATITUDE")



#
# gridded(meuse.grid) = TRUE
sp::gridded(short_df) 
#sp::gridded(short_df) <- TRUE


# image(meuse.grid["dist"])
#gstat::image(short_df[c("x", "y", "z")])
gstat::image(short_df[c("LATITUDE", "LONGITUDE", "ELEVATION")])

#
gstat::image(meuse.grid["dist"])
title("distance to river (red = 0)")

# zinc.idw = gstat::krige(zinc~1, meuse, meuse.grid)
zinc.idw = gstat::krige(zinc~1, meuse, meuse.grid)
#[inverse distance weighted interpolation]

class(zinc.idw)
#"SpatialPixelsDataFrame"

spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")


#install.packages("gstat")

# INVERSE Distance Weighting
# station_info has a column for each year's difference and is a Spatial Points df
# long_df and short_df can be transformed into a Spatial Points df

#tcrs <- raster::crs(map[[1]])
sp::coordinates(long_df1) <- c("LONGITUDE", "LATITUDE")
sp::coordinates(short_df) <- c("LONGITUDE", "LATITUDE")
#sp::proj4string(station_info) <- tcrs

for(i in 2004:2012){
print(sp::bubble(short_df[short_df$year == i, ], 'DIFFERENCE', main = i))
 Sys.sleep(1) 
}
sp::bubble(station_info, "april_2004_DIFFERENCE")
sp::bubble(long_df1, 'DIFFERENCE')


?na.rm
?bubble(0)


?gstat::krige()
?image(0)
gstat::image(meuse.grid["dist"])
idw <- gstat::krige(DIFFERENCE ~ 1, short_df)



####
library(sp)
#sp::
data(meuse)
meuse
class(meuse)

#
sp::coordinates(meuse) <- c("x", "y")

# ?image(0)
data(meuse.grid)
meuse.grid
class(meuse.grid)

#
sp::coordinates(meuse.grid) <- c("x", "y")


#
gridded(meuse.grid) = TRUE
image(meuse.grid["dist"])

#
gstat::image(meuse.grid["dist"])
title("distance to river (red = 0)")

zinc.idw = gstat::krige(zinc~1, meuse, meuse.grid)
#[inverse distance weighted interpolation]
class(zinc.idw)
#"SpatialPixelsDataFrame"

spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")


################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################

#if (!require("rspatial")) 
#install.packages("devtools")
devtools::install_github('rspatial/rspatial')

library(rspatial)
d <- sp_data('precipitation')

d$prec <- rowSums(d[, c(6:17)])

d

# d[, c("LONG", "LAT")]
dsp <- SpatialPoints(d[, c("LONG", "LAT")], proj4string = tcrs)
dsp <- SpatialPointsDataFrame(dsp, d)

CA <- sp_data("counties")
class(CA)
raster::plot(CA)
list("sp.polygons", )


################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################


################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################


################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################


################################################################################
##########      Creating and Spatial Points Data Frame          ################
################################################################################


