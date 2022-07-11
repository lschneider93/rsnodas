#' Generate a raster from the density of spatial points
#'
#' @param sp_points a data frame of spatial points that must include the columns
#'   longitude, latitude, and other variables
#' @param coords the column names of the coordinates longitude and latitude.
#' @param raster_template a template that the density raster should follow
#' @param sigma changes the distance in which to consider
#' @param max_weight a numeric value between between (0, 1].
#' @param flat_crs a flat (2D) projection that the user specifies.  The user
#'   should consider the location of interest to determine the best projection.
#'
#' @return a stars object that has the density of the spatial point data.
#'
#' @details This function creates a permanent folder and downloads data
#' from the SNODAS website for the inputed date(s), map type(s), and data map
#' that the user provides. This data is stored in the permanent folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list and combines them all into a RasterBrick.
#'
#' @export
points_to_density_stars <- function(sp_points = april_df_ghcnd[[17]],
                                    coords = c("LONGITUDE", "LATITUDE"),
                                    raster_template = snodas_maps_utah[[17]],
                                    sigma = 15000,
                                    max_weight = .5,
                                    flat_crs =
                                      "+proj=utm + zone=12 + datum=WGS84") {

  # Get the Lon and Lat
  LON <- coords[1]
  LAT <- coords[2]

  # load spatstat library and create a SpatialPoints object
  utahMat <- cbind(sp_points[, LON] , sp_points[, LAT])
  utahMat <- as.data.frame(utahMat)

  # Covert to a sf with crs of the raster_template
  utahPP <- sf::st_as_sf(as.data.frame(utahMat), coords = c("V1", "V2"),
                        crs = sf::st_crs(raster_template))

  # Reproject to a flat crs
  utahPP <-  sf::st_transform(utahPP, flat_crs)

  # Create a PPP object by using spatstat
  utahPPP <- spatstat.geom::as.ppp(sf::st_geometry(utahPP))

  # here is where we can change the weights function or max weight
  dens.pts <- max_weight * (spatstat.core::density.ppp(utahPPP, sigma = sigma) /
                       max(spatstat.core::density.ppp(utahPPP, sigma = sigma)))

  # Covert to a sf with crs of the raster_template
  stars_density <- stars::st_as_stars(dens.pts,
                                      crs = sf::st_crs(raster_template))
  # Convert to the flat crs
  sf::st_crs(stars_density) <- sf::st_crs(flat_crs)

  ### Warp flat density so that it follows the raster_template
  stars_density <- stars::st_warp(stars_density, raster_template)

  # WEBSITE: https://tmieno2.github.io/R-as-GIS-for-Economists/dplyr-op.html
  # How to remove NA's and negative values and replace them with 0's.
  stars_density <- dplyr::mutate(stars_density, v = ifelse(is.na(v) == TRUE,
                                                           0, v))
  stars_density <- dplyr::mutate(stars_density, v = ifelse(is.na(v) < 0 ,
                                                           0, v))
  return(stars_density)
}
