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
make_cross_validations <- function(sdf_samples, validation_type, validation_folds, validation_split, use_coords) {


  if(validation_type == "none") {
    if(!use_coords) {
      sdf_samples <- sdf_samples %>%
        dplyr::select(-X, -Y)
    }

    sdf_sample_mat <- as.matrix(sdf_samples)
    sdf_col <- which(colnames(sdf_sample_mat) == "sdf")
    return(list(list(train = list(x = sdf_sample_mat[ , -sdf_col],
                             y = sdf_sample_mat[ , sdf_col]))))
  }
}
