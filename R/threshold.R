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
    filter(dim == 0) %>%
    mutate(persistence = death - birth) %>%
    use_series(persistence) %>%
    mean
}

#' @export
#' @rdname zero_threshold
zero_threshold.pl <- function(x) {
  attr(x, "pd") %>% zero_threshold
}
