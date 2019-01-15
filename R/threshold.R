#' Cmmpute mean of 0-dimensional persistence
#'
#' The 0-dimensional persistence is one of the indicators
#' for the connectedness of data.
#' We can use the value for thresholding non 0-dimensional persistence
#' to judge whether the cycle is exist or not on each dimension.
#'
#' @param x `pd` or `pl` object.
#' @return mean of 0-dimensional persistence.
#' @export
zero_threshold <- function(x) {
  UseMethod("zero_threshold")
}

#' @export
#' @rdname zero_threshold
zero_threshold.pd <- function(x) {
  x %>%
    dplyr::filter(dim == 0) %>%
    dplyr::mutate(persistence = death - birth) %>%
    dplyr::filter(persistence != max(attr(x, "scale"))) %>%
    magrittr::use_series(persistence) %>%
    mean
}

#' @export
#' @rdname zero_threshold
zero_threshold.pl <- function(x) {
  attr(x, "pd") %>% zero_threshold
}

#' Cmmpute mean of non 0-dimensional persistence
#'
#' The 0-dimensional persistence is one of the indicators
#' for the connectedness of data.
#' We can use double of the value for thresholding non 0-dimensional persistence
#' to judge whether the cycle is exist or not on each dimension.
#'
#' @param x `pd` or `pl` object.
#' @return mean of 0-dimensional persistence.
#' @export
zero_hat_double_threshold <- function(x) {
  UseMethod("zero_hat_double_threshold")
}

#' @export
#' @rdname zero_hat_double_threshold
zero_hat_double_threshold.pd <- function(x) {
  x %>%
    dplyr::filter(dim != 0) %>%
    dplyr::mutate(persistence = death - birth) %>%
    magrittr::use_series(persistence) %>%
    magrittr::multiply_by(2) %>%
    mean
}

#' @export
#' @rdname zero_hat_double_threshold
zero_hat_double_threshold.pl <- function(x) {
  attr(x, "pd") %>% zero_hat_double_threshold
}

#' Calculate area of the n-dimensional hypersphere
#'
#' Threshold for n-dimensional persistence is must be magnified
#' by considering difficulty of forming n-dimensional hypersphere.
#' In paticular, use threshold multipuled by `nd_surface(1) / nd_surface(n)`.
#'
#' The unit circle is a 1-dimensional hole and its surface area is `2 * pi`.
#' Then, the unit sphere is a 2-dimensional hole and its surface area is `4 * pi`.
#' To form both the unit circle and the unit sphere with data points
#' having the same surface variance,
#' the unit sphere must have twice as many data points as the unit circle.
#'
#' @param n dimension.
#' @return area of n-dimensional superplane.
#' @export
nd_surface <- function(n) {
  2 * pi ** ((n + 1) / 2) / gamma((n + 1) / 2)
}
