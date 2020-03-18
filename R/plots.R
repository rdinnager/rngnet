#' Function to plot SDF of a polygon
#'
#' @param range_sf
#' @param bg
#' @param type
#'
#' @return
#' @export
#'
#' @examples
plot_SDF <- function(sdf, range_sf = NULL, bg = NULL, type = c("grid", "isolines"), inner_lines = 5) {

  type <- match.arg(type)

  if(type == "grid") {
    pred_plot <- ggplot2::ggplot(sdf, aes(.data$X, .data$Y)) +
      ggplot2::geom_tile(aes(fill = .data$sdf, colour = .data$sdf)) +
      scico::scale_fill_scico(palette = "oleron",
                              limits = c(-1, 1) * max(abs(sdf$sdf)),
                              direction = -1) +
      scico::scale_colour_scico(palette = "oleron",
                              limits = c(-1, 1) * max(abs(sdf$sdf)),
                              direction = -1) +
      ggplot2::coord_equal() +
      ggplot2::theme_void()
  }

  if(type == "isolines") {

    bands <- -min(sdf$sdf) / inner_lines

    levs <- seq(min(sdf$sdf), max(sdf$sdf), by = bands)
    levs_high <- seq(min(sdf$sdf) + bands, max(sdf$sdf) + bands, by = bands)

    iso <- df_to_iso(sdf, levs = levs, levs_high = levs_high, z = "sdf")

    pred_plot <- ggplot2::ggplot(iso) +
      ggplot2::geom_sf(aes(fill = val)) +
      scico::scale_fill_scico(palette = "oleron",
                              limits = c(-1, 1) * max(abs(iso$val)),
                              direction = -1) +
      ggplot2::theme_void()

  }
  pred_plot
}
