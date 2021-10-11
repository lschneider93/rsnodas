
devtools::install_github("jadonwagstaff/remap")


# C:\Users\Logan\Desktop\hydrologic_units\wbdhu8_a_ut.shp
getwd()

sf::st_read("/Users/loganschneider/Desktop/utah_shp 2/Utah.shp")
basin_boundary <- sf::st_read("/Users/Logan/Desktop/hydrologic_units/wbdhu8_a_ut.shp")
source_basin <- basin_boundary[, 3]
raster::plot(source_basin)

sf::st_crs(source_basin)

raster::plot(utah_april_swe_rasters[[1]])


source_basin
utah_april_swe_rasters[[1]]


pnts_sf <- sf::st_as_sf(ut_stat_info, coords = c('LONGITUDE', 'LATITUDE'), crs = sf::st_crs(map[[1]]))


# pnts <- pnts_sf %>% dplyr::mutate(
#   intersection = as.integer(sf::st_intersects(geometry, basin_boundary))
#   , area = ifelse(is.na(intersection), '', map$SUA_NAME16[intersection])
# ) 

pnts




basin_boundary
raster::crs(basin_boundary)
# CRS arguments: +proj=longlat +datum=WGS84 +no_defs 

raster::crs(map[[1]])
# CRS arguments: +proj=longlat +datum=WGS84 +no_defs 

raster::crs(pnts_sf)
# CRS arguments: +proj=longlat +datum=WGS84 +no_defs 


raster::plot(pnts_sf)



ut_stat_info_spatial <- ut_stat_info
coordinates(ut_stat_info_spatial) <- ~ LONGITUDE + LATITUDE
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(ut_stat_info_spatial) <- proj4string(map[[1]])



?sp::over()

sp::over(basin_boundary, ut_stat_info_spatial)
sp::over(ut_stat_info_spatial, basin_boundary)

sp::over(basin_boundary, pnts_sf)
sp::over(pnts_sf, basin_boundary)




class(pnts_sf)





library(rgeos)
library(sp)
library(rgdal)

wa.map <- readOGR("ZillowNeighborhoods-WA.shp", layer="ZillowNeighborhoods-WA")


dat <- data.frame(Longitude = c(-112.0050,-112.2974,-112.1715),
                  Latitude =c(41.5267 , 41.7025 ,41.7167 ),
                  names = c("BRIGHAM", "THATCHER ", "TREMONTON "))

sp::over(utws, ut_stat_info)

sodo <- wa.map[wa.map$CITY == "Seattle"  & wa.map$NAME == "Industrial District", ]

# Don't use df as name, it is an R function
# Better to set longitudes as the first column and latitudes as the second
dat <- data.frame(Longitude = c(-122.332271,-122.353985,-122.331639),
                  Latitude =c(47.591351, 47.62212,47.595152),
                  names = c("Safeco Field", "Key Arena", "Century Link"))
# Assignment modified according
coordinates(dat) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(dat) <- proj4string(map[[1]])

class(utws)


over(dat, sodo)
#  STATE COUNTY    CITY                NAME REGIONID
#1    WA   King Seattle Industrial District   271892
#2  <NA>   <NA>    <NA>                <NA>       NA
#3  <NA>   <NA>    <NA>                <NA>       NA

over(sodo, dat)
#           names
#122 Safeco Field







