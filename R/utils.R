#' Function to convert a data frame with x, y, z variables into an isoline or isoband object
#'
#' @param df
#' @param levs
#' @param levs_high
#' @param z
#'
#' @return
#' @export
#'
#' @examples
df_to_iso <- function(df, levs, levs_high = NULL, x = "X", y = "Y", z = "sdf", buffer = NULL) {
  rast <- raster::rasterFromXYZ(df %>%
                                  dplyr::select(x = .data[[x]], y = .data[[y]], z = .data[[z]]))
  if(!is.null(buffer)) {
    rast <- rast %>%
      raster::extend(c(1, 1))
    raster::values(rast)[is.na(raster::values(rast))] <- buffer
  }

  if(is.null(levs_high)) {
  iso <- isoband::isolines(raster::xFromCol(rast),
                           raster::yFromRow(rast),
                           raster::as.matrix(rast),
                           levs) %>%
    isoband::iso_to_sfg() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(val = levs)
  } else {
    iso <- isoband::isobands(raster::xFromCol(rast),
                             raster::yFromRow(rast),
                             raster::as.matrix(rast),
                             levs,
                             levs_high) %>%
      isoband::iso_to_sfg() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(val = (levs + levs_high) / 2)
  }
  iso
}

#' Function to convert a points based sf to a tibble
#'
#' @param points_sf
#'
#' @return
#' @export
#'
#' @examples
sf_points_to_tibble <- function(points_sf) {
  prediction_df <- points_sf %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble()

  prediction_df
}


#' Title
#'
#' @param fit_model
#'
#' @return
#' @export
#'
#' @examples
get_model <- function(fit_model) {
  if(!is.null(.models$model_list[[fit_model$model_id]])) {
    return(.models$model_list[[fit_model$model_id]])
  } else {
    model <- deepSDF_model(fit_model$model_info$num_vars,
                           net_breadth = fit_model$model_info$net_breadth,
                           dropout_rate = fit_model$model_info$dropout_rate)

    keras::set_weights(model, fit_model$model_weights)

    return(model)

  }
}
