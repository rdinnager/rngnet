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

    if(bands < 0) {
      bands <- (max(sdf$sdf) - min(sdf$sdf)) / inner_lines
    }

    levs <- seq(min(sdf$sdf), max(sdf$sdf), by = bands)
    levs_high <- seq(min(sdf$sdf) + bands, max(sdf$sdf) + bands, by = bands)

    iso <- df_to_iso(sdf, levs = levs, levs_high = levs_high, z = "sdf")

    iso <- iso %>%
      smoothr::smooth("ksmooth", smoothness = 3)

    pred_plot <- ggplot2::ggplot(iso) +
      ggplot2::geom_sf(ggplot2::aes(fill = val)) +
      scico::scale_fill_scico(palette = "oleron",
                              limits = c(-1, 1) * max(abs(iso$val)),
                              direction = -1) +
      ggplot2::theme_void()

  }
  pred_plot
}


#' Title
#'
#' @param model_fit
#' @param use_future
#' @param freeze_frames
#'
#' @return
#' @export
#'
#' @examples
make_training_animation <- function(model_fit, use_future = FALSE,
                                    freeze_frames = 0.05, duration = 10) {
  message("Making training animation...")

  extra_frames <- ceiling(length(model_fit$training_progress) * freeze_frames)

  frame_nums <- names(model_fit$training_progress) %>%
    strsplit("_") %>%
    lapply(as.numeric) %>%
    lapply(function(x) x[1] * model_fit$model_info$batches_per_epoch + x[2]) %>%
    unlist()
  png_files <- tempfile(paste0("png_", seq_along(model_fit$training_progress)),
                        fileext = ".png")
  message("Plotting frames...")
  if(use_future) {
    png_files <- furrr::future_map(seq_along(png_files),
                                   ~ plot_preds(model_fit$training_progress[[.x]],
                                                model_fit$test_data,
                                                model_fit$true_range_polygons,
                                                model_fit$bg_polygons,
                                                frame_nums[.x],
                                                file_names = png_files[.x]),
                                   .progress = TRUE)
  } else {
    png_files <- pbapply::pblapply(seq_along(png_files), function(x) plot_preds(fit$training_progress[[x]],
                                                                       model_fit$test_data,
                                                                       model_fit$true_range_polygons,
                                                                       model_fit$bg_polygons,
                                                                       frame_nums,
                                                                       file_names = png_files[x]))
  }
  gif_file <- tempfile()
  message("Generating gif...")
  png_files <- unlist(png_files)
  png_files <- c(png_files, rep(png_files[length(png_files)], extra_frames))
  anim <- gifski::gifski(png_files, gif_file = gif_file, height = 480, width = 960,
                         delay = duration / length(png_files))

  rstudioapi::viewer(anim)

}
