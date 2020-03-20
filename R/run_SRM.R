#' Title
#'
#' @param range_polygons A polygon or set of polygons as an sf object. The specifies the range boundaries that
#' are being modelled (the response).
#' @param env_raster A \code{RasterLayer} or \code{RasterStack} object with environmental covariates (the predictors).
#' The projection should match the projection of \code{range_polygons}
#' @param bg_polygons The projection should match the projection of \code{range_polygons}
#' @param n_sdf_samples
#' @param net_breadth
#' @param dropout_rate
#' @param use_coords
#' @param standardise_env
#' @param validation_type
#' @param validation_folds
#' @param validation_split
#' @param epochs
#' @param max_epochs
#'
#' @return
#' @export
#'
#' @examples
run_SRM <- function(range_polygons, env_raster, bg_polygons = NULL, n_sdf_samples = 100000,
                    batch_size = floor(n_sdf_samples / 20),
                    net_breadth = 256L, dropout_rate = 0.5, use_coords = FALSE,
                    standardise_env = TRUE, vis_training_progress = TRUE,
                    validation_type = c("folds", "random", "none"),
                    validation_folds = 5L, validation_split = 4L, epochs = "auto",
                    max_epochs = 500L,
                    geo_dist = FALSE) {

  validation_type <- match.arg(validation_type)

  if(is.null(bg_polygons) & is.null(env_raster)) {
    stop("You need to provide either background polygon/s (bg_polygons), or
           an environmental raster (env_raster) from which a background polygon can
           be extracted.")
  }

  if(is.null(bg_polygons) & !is.null(env_raster)) {
    message("Converting env raster to background polygon...")
    rast_mat <- env_raster[[1]] %>%
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


  if(!is.null(env_raster) & standardise_env) {
    env_centre <- apply(raster::values(env_raster), 2, mean, na.rm = TRUE)
    env_scale <- apply(raster::values(env_raster), 2, sd, na.rm = TRUE)

    raster::values(env_raster) <- scale(raster::values(env_raster), center = env_centre, scale = env_scale)
  }

  sdf_samples <- collect_sdf_samples(range_polygons, bg_polygons, env_raster, n_sdf_samples, geo_dist = geo_dist,
                                     centrer = centrer, scaler = scaler)

  prediction_df <- make_prediction_grid(range_polygons, bg_polygons, env_raster, return_type = "tibble", use_coords = TRUE)
  test_dat <- as.matrix(prediction_df %>%
                          dplyr::select(-X, -Y))

  input_len <- ncol(sdf_samples) - 3L

  model <- deepSDF_model(input_len, net_breadth = net_breadth, dropout_rate = dropout_rate)

  model %>% keras::compile(
    optimizer = 'adam',
    loss = 'mean_absolute_error'
  )

  sdf_train <- make_cross_validations(sdf_samples, validation_type, validation_folds, validation_split,
                                      use_coords)
  callbacks <- list()
  if(epochs == "auto") {
    callbacks <- c(callbacks, keras::callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 10, restore_best_weights = TRUE))
    epochs <- max_epochs
  }

  if(length(sdf_train) > 1){
    validations <- lapply(sdf_train, run_model, epoch = epochs, batch_size = batch_size)
  }

  if(vis_training_progress) {
    batches_per_epoch <- floor(n_sdf_samples / batch_size)
    low_freq <- batches_per_epoch
    high_freq <- floor(batches_per_epoch / 5)
    decay_finish <- 25
    test_predictions_callback <- R6::R6Class("TestPredictions",
                               inherit = keras::KerasCallback,

                               public = list(

                                 test_predictions = list(),

                                 current_epoch = 0L,

                                 on_epoch_begin = function(epoch, logs = list()) {
                                   self$current_epoch <- epoch
                                 },

                                 on_batch_end = function(batch, logs = list()) {
                                   vis_train_progress_every <- as.integer(high_freq + self$current_epoch *
                                                                            (1 / decay_finish) *
                                                                            (low_freq - high_freq))

                                   if(self$current_epoch > decay_finish) {
                                     vis_train_progress_every <- low_freq
                                   }
                                   if(batch %% vis_train_progress_every == 0) {
                                      self$test_predictions[[paste(self$current_epoch, batch, sep = "_")]] <-
                                        predict(self$model,
                                                test_dat)
                                   }
                                 }
                               ))

    test_preds <- test_predictions_callback$new()
    callbacks <- c(callbacks, test_preds)
  }

  final_fit <- run_model(sdf_train[[length(sdf_train)]], epoch = epochs, batch_size = batch_size)

  if(vis_training_progress) {
    png_files <- tempfile(paste0("png_", seq_along(test_preds$test_predictions)),
                          fileext = ".png")

    png_files <- lapply(seq_along(png_files), function(x) plot_preds(test_preds$test_predictions[[x]],
                                                                     prediction_df,
                                                                     bg_polygons,
                                                                     file_names = png_files[x]))

    gif_file <- tempfile()
    anim <- gifski::gifski(unlist(png_files), gif_file = gif_file, height = 480, width = 960,
                           delay = 10 / length(png_files))

    rstudioapi::viewer(anim)

  }


}
