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
                                 return_type = c("matrix", "tibble"), use_coords = FALSE) {

  return_type <- match.arg(return_type)
  prediction_map <- make_prediction_sf(range_sf, bg, n_cells)

  if(!is.null(env)) {
    env_vals <- extract(env, prediction_map %>% sf::st_as_sf())
    colnames(env_vals) <- names(env)

    prediction_df <- sf_points_to_tibble(prediction_map) %>%
      dplyr::bind_cols(env_vals %>%
                         dplyr::as_tibble())
  } else {
    prediction_df <- sf_points_to_tibble(prediction_map)
  }

  prediction_df <- prediction_df %>%
    dplyr::mutate_at(dplyr::vars(-X, -Y),
                     ~ ifelse(is.na(.), 0, .))

  if(!use_coords) {
    prediction_df <- prediction_df %>%
      dplyr::select(-X, -Y)
  }

  if(return_type == "matrix") {
    return(prediction_df  %>%
             as.matrix())
  } else {

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
                     sf::st_union() %>%
                     sf::st_sample(n_cells, type = "regular")  %>%
                     sf::st_set_crs(sf::st_crs(bg)))

  prediction_map

}

#' Prediction plotter
#'
#' @param predictions
#' @param prediction_df
#'
#' @return
#' @export
#'
#' @examples
plot_preds <- function(predictions, prediction_df, range_polygons, bg,
                       frame_nums = NULL,
                       centrer = NULL, scaler = NULL,
                       file_names = NULL) {
  prediction_results <- prediction_df %>%
    dplyr::mutate(sdf = predictions %>%
                    as.vector())

  prediction_sf <- df_to_iso(prediction_results, levs = 0, buffer = 0.1) %>%
    sf::st_cast("MULTIPOLYGON")

  # if(!is.null(centrer)) {
  #   sf::st_geometry(prediction_sf) <- (sf::st_geometry(prediction_sf) * scaler) + centrer
  # }

  ground_truth <- ggplot2::ggplot(bg %>% sf::st_set_crs(NA)) +
    ggplot2::geom_sf(fill = "white") +
    ggplot2::geom_sf(data = range_polygons %>% sf::st_set_crs(NA), fill = "grey20") +
    ggplot2::theme_void()

  if(!sf::st_is_empty(prediction_sf)) {
    prediction_plot <- ggplot2::ggplot(bg %>% sf::st_set_crs(NA)) +
      ggplot2::geom_sf(fill = "white") +
      ggplot2::geom_sf(data = prediction_sf, fill = "grey20") +
      ggplot2::theme_void()
  } else {
    prediction_plot <- ggplot2::ggplot(bg %>% sf::st_set_crs(NA)) +
      ggplot2::geom_sf(fill = "white") +
      ggplot2::theme_void()
  }

  frame <- patchwork::wrap_plots({ground_truth + ggplot2::ggtitle("Species Range: Ground Truth")} +
                                   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)),
    {prediction_plot + ggplot2::ggtitle("Species Range: Model Prediction") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))})

  if(!is.null(frame_nums)) {
    frame <- frame +
      patchwork::plot_annotation(title = paste("Iteration:", frame_nums))
  }

  if(!is.null(file_names)) {
    ggplot2::ggsave(file_names, frame, device = "png", type = "cairo", height = 5, width = 10)
    return(file_names)
  } else {
    return(frame)
  }
}

#' Function to make predictions from the model and return an sf object
#'
#' @param model
#' @param test_dat
#' @param prediction_df
#'
#' @return
#' @export
#'
#' @examples
predict_to_sf <- function(model, test_dat, prediction_df) {

  prediction_results <- predict_to_sdf(model, test_dat, prediction_df)

  prediction_sf <- df_to_iso(prediction_results, levs = 0, buffer = 0.1) %>%
    sf::st_cast("MULTIPOLYGON")

  prediction_sf
}

#' Title
#'
#' @param model
#' @param test_dat
#' @param prediction_df
#'
#' @return
#' @export
#'
#' @examples
predict_to_sdf <- function(model, test_dat, prediction_df, scaler = NULL) {

  predictions <- predict(model, test_dat)

  prediction_results <- prediction_df %>%
    dplyr::mutate(sdf = predictions %>%
                    as.vector())

  if(!is.null(scaler)) {
    prediction_results <- prediction_results %>%
      dplyr::mutate(sdf = sdf * scaler)
  }

  prediction_results
}

#' Title
#'
#' @param fit_model
#' @param new_env
#'
#' @return
#' @export
#'
#' @examples
predict_range <- function(fit_model, new_env = NULL, plot = TRUE, return_sdf = TRUE) {

  model <- get_model(fit_model)

  if(!is.null(fit_model$model_info$env_centre)) {
    raster::values(new_env) <- scale(raster::values(new_env),
                                     center = fit_model$model_info$env_centre,
                                     scale = fit_model$model_info$env_scale)
  }

  prediction_df <- make_prediction_grid(fit_model$true_range_polygons,
                                        fit_model$bg_polygons,
                                        new_env,
                                        return_type = "tibble",
                                        use_coords = TRUE)
  test_dat <- as.matrix(prediction_df %>%
                          dplyr::select(-X, -Y))

  if(!is.null(fit_model$model_info$scaler)) {
    scale_it <- fit_model$model_info$scaler
  } else {
    scale_it <- 1
  }

  prediction_sdf <- predict_to_sdf(model, test_dat, prediction_df,
                                   scaler = scale_it)
  predicted_sf <- df_to_iso(prediction_sdf, levs = 0, buffer = 0.1) %>%
    sf::st_cast("MULTIPOLYGON")

  if(plot) {
    if(!sf::st_is_empty(predicted_sf)) {
      p <- ggplot2::ggplot(fit_model$bg_polygons %>%
                             sf::st_set_crs(NA)) +
        ggplot2::geom_sf() +
        ggplot2::geom_sf(data = predicted_sf, fill = "grey20") +
        ggplot2::theme_minimal()
    } else {
      p <- ggplot2::ggplot(fit_model$bg_polygons %>%
                             sf::st_set_crs(NA)) +
        ggplot2::geom_sf() +
        ggplot2::theme_minimal()
    }
    plot(p)
  }

  if(return_sdf) {
    list(sf = predicted_sf, sdf = prediction_sdf)
  } else {
    predicted_sf
  }

}
