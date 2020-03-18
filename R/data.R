#' Species ranges for 31 Australian Goannas from Shai et al. (2017).
#'
#' A dataset containing 31 Australian Goanna species' ranges in the form of
#' spatial polygons (in \code{sf} format)
#'
#' @format An sf object with 31 rows and 7 variables:
#' \describe{
#'   \item{Binomial}{Latin binomial name of Goanna species}
#'   \item{Area}{Area of species' range, in meters^2}
#'   \item{FID_2}{??}
#'   \item{TID}{??}
#'   \item{Group}{Taxonomic group, always lizard in this case}
#'   \item{Value}{??}
#'   \item{geometry}{sfc_GEOMETRY list of type MULTIPOLYGON containing the range polygons}
#' }
#' @source \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.83s7k}
"Oz_goannas"
