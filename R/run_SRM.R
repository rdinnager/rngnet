run_SRM <- function(range_polygons, env_raster, bg_polygon = NULL, n_sdf_samples = 100000,
                    net_breadth = 256L, dropout_rate = 0.5, use_coords = FALSE,
                    validation_type = c("folds", "random", "none"),
                    validation_folds = 5L, validation_split = 4L, epochs = "auto",
                    max_epochs = 500L) {

  validation_type <- match.arg(validation_type)

}
