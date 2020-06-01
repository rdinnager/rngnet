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
                    batch_size = floor(n_sdf_samples / 20), sdf_cutoff = 0.1,
                    net_breadth = 256L, dropout_rate = 0.5, use_coords = FALSE,
                    standardise_env = TRUE, vis_training_progress = TRUE,
                    validation_type = c("folds", "random", "none"),
                    validation_folds = 5L, validation_split = 4L, validation_prop = 0.2,
                    epochs = "auto",
                    early_stop_patience = 10,
                    keep_sdf_samples = TRUE,
                    max_epochs = 500L,
                    geo_dist = FALSE,
                    drop_NAs = TRUE,
                    verbose = TRUE) {

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

  message("Calculating SDF samples. This could take awhile...")
  sdf_samples <- collect_sdf_samples(range_polygons, bg_polygons, env_raster, n_sdf_samples, geo_dist = geo_dist,
                                     centrer = centrer, scaler = scaler, use_future = use_future,
                                     drop_NAs = drop_NAs)

  prediction_df <- make_prediction_grid(range_polygons, bg_polygons, env_raster, return_type = "tibble", use_coords = TRUE)
  test_dat <- as.matrix(prediction_df %>%
                          dplyr::select(-X, -Y))

  input_len <- ncol(sdf_samples) - 3L

  model <- deepSDF_model(input_len, net_breadth = net_breadth, dropout_rate = dropout_rate)

  loss_func <- function(sdf_cutoff = 0.1) {
    sdf_cutoff <- sdf_cutoff

    function(y_true, y_pred) {
      keras::loss_mean_absolute_error(keras::k_clip(y_true, -sdf_cutoff, sdf_cutoff),
                                      keras::k_clip(y_pred, -sdf_cutoff, sdf_cutoff))
    }
  }

  model %>% keras::compile(
    optimizer = 'adam',
    loss = loss_func(sdf_cutoff = sdf_cutoff)
  )

  sdf_train <- make_cross_validations(sdf_samples, validation_type, validation_folds, validation_split,
                                      validation_prop, use_coords)
  callbacks <- list()
  if(epochs == "auto") {
    callbacks <- c(callbacks, keras::callback_early_stopping(monitor = "loss", min_delta = 0.0001, patience = early_stop_patience, restore_best_weights = TRUE))
    epochs <- max_epochs
  }

  environment(run_model) <- environment()

  if(length(sdf_train) > 1){
    validations <- lapply(sdf_train[-length(sdf_train)], run_model, epoch = epochs, batch_size = batch_size, reset_when_done = TRUE)
  }

  batches_per_epoch <- floor(n_sdf_samples / batch_size)

  if(vis_training_progress) {
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

                                      # p <- plot_preds(self$test_predictions[[paste(self$current_epoch, batch, sep = "_")]],
                                      #                 prediction_df,
                                      #                 range_polygons,
                                      #                 bg_polygons)
                                      # print(p)
                                   }
                                 }
                               ))

    test_preds <- test_predictions_callback$new()
    callbacks <- c(callbacks, test_preds)
  }

  final_fit <- run_model(sdf_train[[length(sdf_train)]], epoch = epochs, batch_size = batch_size)

  model_id <- digest::digest(model)

  .models$model_list[[model_id]] <- model

  model_weights <- final_fit$weights

  history <- list(final_fit = final_fit$history)

  predicted_sf <- predict_to_sf(model, test_dat, prediction_df)

  metrics <- list(final_fit_metrics = NULL)

  if(validation_type != "none") {
    validation_metrics <- NULL
    history$validations <- lapply(validations, function(x) x$history)
    validation_weights <- lapply(validations, function(x) x$weights)
    metrics$validations <- validation_metrics
  }

  model_info <- list(batches_per_epoch = batches_per_epoch,
                     num_vars = input_len,
                     net_breadth = net_breadth,
                     dropout_rate = dropout_rate,
                     scaler = scaler,
                     centrer = centrer)

  if(standardise_env) {
    model_info$env_centre <- env_centre
    model_info$env_scale <- env_scale
  }

  res <- list(model_id = model_id,
              model_weights = model_weights, history = history,
              true_range_polygons = range_polygons,
              predicted_range_polygons = predicted_sf,
              bg_polygons = bg_polygons,
              test_data = prediction_df,
              metrics = metrics,
              model_info = model_info)

  if(vis_training_progress) {
    res$training_progress <- test_preds$test_predictions
  }

  if(keep_sdf_samples) {
    res$sdf_samples <- sdf_samples
  }

  if(validation_type != "none") {
    res$validation_intervals <- lapply(sdf_train[-length(sdf_train)],
                                       function(x) x$intervals)
    res$validation_weights <- validation_weights
  }

  class(res) <- "rngnet"
  res

}


