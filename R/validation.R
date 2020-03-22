#' Function to generate validation data
#'
#' @param sdf_samples
#' @param validation_type
#' @param validation_folds
#' @param validation_split
#'
#' @return
#' @export
#'
#' @examples
make_cross_validations <- function(sdf_samples, validation_type, validation_folds, validation_split, validation_prop, use_coords) {

  if(!use_coords) {
    sdf_sample_mat <- sdf_samples %>%
      dplyr::select(-X, -Y) %>%
      as.matrix()
  } else {
    sdf_sample_mat <- as.matrix(sdf_samples)
  }

  sdf_col <- which(colnames(sdf_sample_mat) == "sdf")

  no_val <- list(list(train = list(x = sdf_sample_mat[ , -sdf_col],
                    y = sdf_sample_mat[ , sdf_col])))

  if(validation_type == "none") {
    return(no_val)
  }

  if(validation_type == "random") {
    #validation_prop <- 1 / validation_folds

    intervals <- replicate(validation_folds, {
      samp_ang <- (2 * pi * validation_prop) / validation_split
      rand_start <- runif(validation_split, -pi, pi - samp_ang)
      end <- rand_start + samp_ang
      list(starts = rand_start, ends = end)
      },
      simplify = FALSE)

    cvs <- lapply(intervals, function(x) make_one_cross_validation(sdf_samples,
                                                                   x$starts,
                                                                   x$ends,
                                                                   use_coords))

    return(c(cvs, no_val))

  }

  if(validation_type == "folds") {

    slices <- validation_folds * validation_split
    slice_angle <- 2 * pi / slices
    all_starts <- seq(-pi, pi - slice_angle, by = slice_angle)
    all_ends <- seq(-pi + slice_angle, pi, by = slice_angle)

    intervals <- sample.int(slices) %>%
      split(rep(1:validation_folds, each = validation_split)) %>%
      lapply(function(x) list(starts = all_starts[x],
                              ends = all_ends[x]))

    cvs <- lapply(intervals, function(x) make_one_cross_validation(sdf_samples,
                                                                   x$starts,
                                                                   x$ends,
                                                                   use_coords))

    return(c(cvs, no_val))

  }
}

#' Make one cross validation from a set of intervals
#'
#' @param sdf_samples
#' @param starts
#' @param ends
#' @param use_coords
#'
#' @return
#' @export
#'
#' @examples
make_one_cross_validation <- function(sdf_samples, starts, ends, use_coords) {
  range_centre <- sdf_samples %>%
    dplyr::filter(sdf < 0) %>%
    dplyr::summarise(mean_X = mean(X, na.rm = TRUE),
                     mean_Y = mean(Y, na.rm = TRUE)) %>%
    unlist()

  new_samples <- sdf_samples %>%
    dplyr::mutate(new_x = (X - range_centre[1]) / max(abs(X)),
                  new_y = (Y - range_centre[2]) / max(abs(Y))) %>%
    dplyr::mutate(angle = atan2(new_y, new_x))


  new_samples <- new_samples %>%
    dplyr::mutate(validation = as.numeric(apply(sapply(angle, function(x) x > starts) &
                                                  sapply(angle, function(x) x < ends),
                                                2,
                                                any)))
  if(!use_coords) {
    new_samples <- new_samples %>%
      dplyr::select(-X, -Y)
  }

  train_samples <- new_samples %>%
    dplyr::filter(validation == 0) %>%
    dplyr::select(-angle, -validation, -new_x, -new_y)

  val_samples <- new_samples %>%
    dplyr::filter(validation == 1) %>%
    dplyr::select(-angle, -validation, -new_x, -new_y)

  train_sample_mat <- as.matrix(train_samples)
  train_col <- which(colnames(train_sample_mat) == "sdf")

  val_sample_mat <- as.matrix(val_samples)
  val_col <- which(colnames(val_sample_mat) == "sdf")

  list(train = list(x = train_sample_mat[ , -train_col],
                    y = train_sample_mat[ , train_col]),
       validation = list(x = val_sample_mat[ , -val_col],
                         y = val_sample_mat[ , val_col]),
       intervals = list(starts = starts, ends = ends))
}
