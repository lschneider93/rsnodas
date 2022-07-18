#' Blending Satellite and land-based rasters with weights
#'
#' @param raster_sate A raster object of the satellite source estimates
#' @param raster_land A raster object of estimates from the the land-based
#'   observation
#' @param weights A raster object with weights
#'
#' @return a data frame with the columns with the variables needed to
#'   make predictions of the model.
#'
#' @details This function combines gridded product by taking in two rasters and
#'   the weights applied to both rasters.
#'
#' @export
blend_raster <- function(raster_sate,
                         raster_land,
                         weights) {

  comb_map <- ((weights) * raster_land) +
    ((1 - weights) * raster_sate)

  return(comb_map)
}
