#' Function to calculate an SDF from a set of points to a polygon
#'
#' @param points_sf
#' @param range_sf
#' @param geo_dist
#'
#' @return
#' @export
#'
#' @examples
calculate_sdf <- function(points_sf, range_sf, geo_dist = TRUE) {


  if(!geo_dist) {
    points_sf <- points_sf %>%
      sf::st_set_crs(NA)
    range_sf <- range_sf %>%
      sf::st_set_crs(NA)
  }

  grid_dists <- sf::st_distance(points_sf, range_sf %>%
                                  sf::st_union() %>%
                                  sf::st_cast("MULTILINESTRING"))
  grid_dists <- as.vector(grid_dists)

  suppressMessages(grid_inside <- sf::st_within(points_sf, range_sf, sparse = FALSE))
  grid_inside <- as.vector(grid_inside)
  grid_dists[grid_inside] <- -grid_dists[grid_inside]

  return(grid_dists)

}


#' Function to generate an SDF on a background grid for a polygon
#'
#' @param range_sf
#' @param bg
#' @param geo_dist
#'
#' @return
#' @export
#'
#' @examples
make_SDF_grid <- function(range_sf, bg, geo_dist = TRUE) {
  prediction_sf <- make_prediction_sf(range_sf, bg)
  prediction_df <- prediction_sf %>%
    sf_points_to_tibble() %>%
    dplyr::mutate(sdf = calculate_sdf(prediction_sf, range_sf, geo_dist = geo_dist))
}
