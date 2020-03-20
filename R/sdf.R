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

  suppressMessages(grid_inside <- points_sf %>%
    sf::st_as_sf() %>%
    sf::st_join(range_sf %>%
                  sf::st_union() %>%
                  sf::st_as_sf() %>%
                  dplyr::mutate(id = 1)))

  grid_inside <- as.vector(!is.na(grid_inside$id))
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


#' Function to generate SDF samples for a range polygon within a background polygon.
#'
#' @param range_polygons
#' @param bg_polygons
#' @param n_sdf_samples
#'
#' @return
#' @export
#'
#' @examples
collect_sdf_samples <- function(range_polygons, bg_polygons, env = NULL, n_pts = 100000, close_scale = 0.025, very_close_scale = 0.0025,
                                geo_dist = FALSE, centrer = NULL, scaler = NULL) {

  if(!is.null(env)) {
    env_vx <- velox::velox(env)
  }

  if(!is.null(centrer)) {

    shape_centroid <- sf::st_centroid(bg_polygons %>% sf::st_union())
    st <- bg_polygons - shape_centroid

    if(!geo_dist) {
      norms <- st %>%
        sf::st_coordinates() %>%
        .[ , 1:2] %>%
        apply(1, function(x) sqrt(sum(x^2)))
    } else {
      norms <- bg_polygons %>%
        sf::st_union() %>%
        sf::st_cast("POLYGON") %>%
        sf::st_cast("POINT") %>%
        sf::st_distance(shape_centroid) %>%
        as.vector()
    }

    scaler <- max(norms)

    centrer <- shape_centroid %>% sf::st_coordinates() %>%
      as.vector()
    }

  range_st <- range_polygons %>%
    sf::st_cast("POLYGON") %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_set_crs(NA)

  line_len <- sf::st_length(range_st %>% sf::st_union())

  n_surface_pts <- ceiling(n_pts * 0.4)

  dens <- n_surface_pts / line_len

  shape_sample <- sf::st_line_sample(range_st, density = dens, type = "random") %>%
    sf::st_union()

  n_pts_final <- nrow(shape_sample[[1]])

  shape_sample_close <- (shape_sample + matrix(rnorm(n_pts_final * 2, sd = close_scale * scaler), ncol = 2)) %>%
    #sf::st_sf() %>%
    sf::st_cast("POINT")
  shape_sample_very_close <- (shape_sample + matrix(rnorm(n_pts_final * 2, sd = very_close_scale  * scaler), nrow = 2)) %>%
    #sf::st_sf() %>%
    sf::st_cast("POINT")
  shape_sample_bg <- sf::st_sample(bg_polygons, ceiling(n_pts * 0.2))

  shape_sample <- c(shape_sample_close, shape_sample_very_close, shape_sample_bg)

  if(geo_dist) {
    shape_sample <- shape_sample %>%
      sf::st_set_crs(sf::st_crs(range_polygons))
  }

  if(!is.null(env)) {
    env_sample <- env_vx$extract_points(shape_sample)
    colnames(env_sample) <- names(env)
  }

  sdfs <- calculate_sdf(shape_sample, range_polygons, geo_dist = geo_dist)

  shape_sample <- (shape_sample - centrer) / scaler
  sdfs <- sdfs / scaler

  sdf_sample <- shape_sample %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(sdf = sdfs) %>%
    dplyr::bind_cols(env_sample %>%
                       dplyr::as_tibble())

  sdf_sample <- sdf_sample %>%
    dplyr::mutate_at(dplyr::vars(-X, -Y, -sdf),
                     ~ ifelse(is.na(.), 0, .))


  sdf_sample

}
