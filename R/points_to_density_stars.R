#' Generate a raster from the density of spatial points
#'
#' @param sp_points a data frame of spatial points that must include the columns
#'   longitude, latitude, and other variables
#'
#' @param coords the column names of the coordinates longitude and latitude.
#' @param raster_template a template that the density raster should follow
#'
#' @param sigma changes the kernel density or the how far away from an
#'   observation.
#' @param max_weight a numeric value between between (0, 1].
#' @param flat_crs a flat (2D) projection that the user specifies.  The user
#'   should consider the location of interest to determine the best projection.
#'
#' @return a stars object that has the density of the spatial point data.
#'
#' @details This function creates a permanent folder and downloads data
#' from the SNODAS website for the input date(s), map type(s), and data map
#' that the user provides. This data is stored in the permanent folder.
#' The function unzips the selected data files and stores the specified
#' maps into a list and combines them all into a RasterBrick.
#'
#' @importFrom spatstat.geom as.ppp
#' @importFrom spatstat.core density.ppp
#' @importFrom sf st_as_sf st_crs st_transform st_geometry
#' @importFrom stars read_stars st_warp st_as_stars
#' @importFrom terra terrain rast vect extract
#'
#' @export
points_to_density_stars <- function(sp_points,# = april_df_ghcnd[[17]],
                                    coords = c("LONGITUDE", "LATITUDE"),
                                    raster_template,# = snodas_maps_utah[[17]],
                                    sigma = 15000,
                                    max_weight = .5,
                                    flat_crs =
                                      "+proj=utm + zone=12 + datum=WGS84") {

  # sp_points = april_1_snotel[april_1_snotel$DATE == "2004-04-01", ]
  # coords = c("LONGITUDE", "LATITUDE")
  # raster_template = snodas_april_maps[[1]]
  # sigma = 15000
  # max_weight = 1
  # flat_crs =
  #   "+proj=utm + zone=12 + datum=WGS84"

  # Testing to ensure max weight is between 0 and 1.
  if ((max_weight < 0) | (max_weight > 1)) {
    stop("max weight can't be less than 0 or greater than 1")
  }

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

  values <- "values"
  names(stars_density) <- values

  # WEBSITE: https://tmieno2.github.io/R-as-GIS-for-Economists/dplyr-op.html
  # How to remove NA's and negative values and replace them with 0's.
  stars_density <- dplyr::mutate(stars_density,
                                 values = ifelse(is.na(values) == TRUE,
                                                 0, values))
  stars_density <- dplyr::mutate(stars_density,
                                 values = ifelse(values < 0 ,
                                                 0, values))
  stars_density <- dplyr::mutate(stars_density,
                                 values = ifelse(values > 1 ,
                                                 1, values))
  x <- sf::st_as_sf(stars_density)$values

  x2 <- data.frame(x, breaks = cut(x, breaks = 10, include.lowest = TRUE))
  table(x2$breaks)
  sf::st_as_sf(stars_density)$values
  t <- terra::rast(stars_density)

  stars_density[1, ,  ]
  return(stars_density)
}
