#' Generate a raster from data frame
#'
#' @param model the model object that will be used to predict.
#' @param data_frame a data frame with the columns of the model that will
#'   be used to create the raster
#'
#' @return a stars object that contains the predictions of the model
#'
#' @details This function creates gridded product by taking in a model_data,
#'   raster template, a model object or model x and y's, path to prism variables,
#'
#'
#' @importFrom stats predict
#' @importFrom methods hasArg
#' @importFrom sf st_coordinates st_as_sf st_crs
#' @importFrom stars read_stars st_warp st_as_stars
#' @importFrom terra terrain rast vect extract
#' @importFrom mgcv gam
#'
#' @details This function creates a permanent folder and downloads data
#'   from the SNODAS website for the selected date(s), map type(s), and data map
#'   that the user provides. This data is stored in the permanent folder.
#'   The function unzips the selected data files and stores the specified
#'   maps into a list of star objects.
#'
#' @export
df_to_raster <- function(model,
                         data_frame) {

  #  Finally Make predictions :0
  model_predictions <- stats::predict(model, newdata = df)
  model_predictions <- ifelse(model_predictions < 0, 0,
                              model_predictions)

  # add the predictions to the data frame of longitude and latitude
  df$MODEL_PRED <- model_predictions
  utah <- df[c("LONGITUDE", "LATITUDE", "MODEL_PRED")]

  # creating a stars object with the predictions
  utah_pred_star <- stars::st_as_stars(utah, coords = c("LONGITUDE", "LATITUDE"),
                                       crs = sf::st_crs(raster_template))

  # put the prediction raster into the template of the raster provided
  utah_pred_star <- stars::st_warp(utah_pred_star, raster_template)

  return(utah_pred_star)
}
