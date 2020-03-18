#' Function to generate a grid across a background polygon on which to make model predictions
#'
#' @param range_sf
#' @param bg
#' @param env
#' @param n_cells
#' @param return_type
#'
#' @return
#' @export
#'
#' @examples
make_prediction_grid <- function(range_sf, bg, env = NULL, n_cells = 10000,
                                 return_type = c("sf", "tibble")) {

  prediction_map <- make_prediction_sf(range_sf, bg, n_cells)
  if(return_type == "sf") {
    return(prediction_map)
  } else {

    prediction_df <- sf_points_to_tibble(prediction_map)

    return(prediction_df)
  }
}

#' Function to generate a grid across a background polygon on which to make model predictions as an sf object
#'
#' @param range_sf
#' @param bg
#' @param n_cells
#' @param include_sdf
#' @param geo_dist
#'
#' @return
#' @export
#'
#' @examples
make_prediction_sf <- function(range_sf, bg, n_cells = 10000) {


  suppressMessages(prediction_map <- bg %>%
                     sf::st_set_crs(NA) %>%
                     sf::st_sample(n_cells, type = "regular")  %>%
                     sf::st_set_crs(sf::st_crs(bg)))

  prediction_map

}




