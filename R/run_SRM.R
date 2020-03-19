run_SRM <- function(range_polygons, env_raster, bg_polygons = NULL, n_sdf_samples = 100000,
                    net_breadth = 256L, dropout_rate = 0.5, use_coords = FALSE,
                    validation_type = c("folds", "random", "none"),
                    validation_folds = 5L, validation_split = 4L, epochs = "auto",
                    max_epochs = 500L) {

  validation_type <- match.arg(validation_type)

  if(!is.null(bg_polygons)) {
    message("Converting env raster to background polygon...")
    rast_mat <- env[[1]] %>%
      as.matrix()
    rast_mat[!is.na(rast_mat)] <- 1
    rast_mat[is.na(rast_mat)] <- 0
    rast_sfg <- isoband::isolines(xFromCol(env[[1]]),
                                  yFromRow(env[[1]]),
                                  rast_mat,
                                  0.9) %>%
      isoband::iso_to_sfg()
    bg_polygons <- rast_sfg[[1]] %>%
      sf::st_combine() %>%
      sf::st_sf() %>%
      sf::st_cast("MULTIPOLYGON")
  }

  sdf_samples <- collect_sdf_samples(range_polygons, bg_polygons, env, n_sdf_samples)

}
