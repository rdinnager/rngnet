calculate_metrics <- function(true_sf, pred_sf) {
  geom_true <- sf::st_geometry(true_sf %>%
                                 sf::st_set_crs(NA))
  geom_pred <- sf::st_geometry(pred_sf %>%
                                 sf::st_set_crs(NA))

  if(grepl("POLYGON", class(geom_true)) & grepl("POLYGON", class(geom_true))) {
    polygons <- TRUE
  }

  frechet <- sf::st_distance(geom_true, geom_pred, which = "Frechet")
  haussdorff <- sf::st_distance(geom_true, geom_pred, which = "Haussdorff")

  metrics <- list(Haussdorff = haussdorff,
                  Frechet = frechet)

  if(polygons) {
    metrics$recall <- recall
    metrics$precision <- precision
    metrics$F_metric <- recall * precision
  }

}
