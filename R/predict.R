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
    env_vx <- velox::velox(env)
    env_vals <- env_vx$extract_points(prediction_map)
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
plot_preds <- function(predictions, prediction_df, bg, centrer = NULL, scaler = NULL,
                       file_names) {
  prediction_results <- prediction_df %>%
    dplyr::mutate(sdf = predictions %>%
                    as.vector())

  prediction_sf <- df_to_iso(prediction_results, levs = 0, buffer = 0.1) %>%
    sf::st_cast("MULTIPOLYGON")

  # if(!is.null(centrer)) {
  #   sf::st_geometry(prediction_sf) <- (sf::st_geometry(prediction_sf) * scaler) + centrer
  # }

  ground_truth <- ggplot(bg %>% sf::st_set_crs(NA)) +
    geom_sf(fill = "white") +
    geom_sf(data = range_polygons %>% sf::st_set_crs(NA), fill = "grey20") +
    theme_void()

  if(!sf::st_is_empty(prediction_sf)) {
    prediction_plot <- ggplot(bg %>% sf::st_set_crs(NA)) +
      geom_sf(fill = "white") +
      geom_sf(data = prediction_sf, fill = "grey20") +
      theme_void()
  } else {
    prediction_plot <- ggplot(bg %>% sf::st_set_crs(NA)) +
      geom_sf(fill = "white") +
      theme_void()
  }

  frame <- patchwork::wrap_plots({ground_truth + ggtitle("Species Range: Ground Truth")} +
                                   theme(plot.title = element_text(hjust = 0.5)),
    {prediction_plot + ggtitle("Species Range: Model Prediction") +
        theme(plot.title = element_text(hjust = 0.5))})



  ggsave(file_names, frame, device = "png", type = "cairo", height = 5, width = 10)
  file_names
}


