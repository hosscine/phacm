#' Count local maximal of a vector.
#'
#' `count_local_maximal` supposes to take a persistent landscape.
#' For parsistent landcape, the number of local maximal indicates
#' the number of cycles.
#'
#' @param x a vector or a persistent landscape object inherits `pl`.
#' @param thresh threshold to ignore local maximals under the value.
#' @return the number of local maximal.
#' @seealso [compute_pd()]
#' @export
count_local_maximal <- function(x, thresh = 0) {
  UseMethod("count_local_maximal")
}

#' @export
#' @rdname count_local_maximal
count_local_maximal.default <- function(x, thresh) {
  assert_that(is.numeric(x))
  assert_that(assertthat::is.scalar(thresh))

  x[x < thresh] <- 0
  x %>% diff %>% sign %>% diff %>% magrittr::equals(-2) %>% sum
}

#' @export
#' @rdname count_local_maximal
count_local_maximal.pl <- function(x, thresh) {
  purrr::map_int(x$dimnames, ~ count_local_maximal(x[[.]], thresh)) %>%
    setter::set_names(x$dimnames)
}

#' @export
#' @rdname count_local_maximal
count_local_maximal.pd <- function(x, thresh) {
  count_local_maximal.pl(compute_pl(x), thresh)
}

#' @export
#' @rdname count_local_maximal
count_local_maximal.smooth.spline <- function(x, thresh) {
  count_local_maximal(x$y, thresh)
}
