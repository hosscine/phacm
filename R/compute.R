#' Compute persistent diagram from point cloud.
#'
#' Uses [TDA::ripsDiag()] to compute persistent diagram.
#' Then keeps diagram to avoid to recomputing.
#' The keeped diagram can be took out by [last_pd()].
#'
#' @param X target data.
#' @param maxdimension max dimension to compute.
#'   Lower value makes computatin time to short.
#'   In case of computing on the general computer,
#'   `maxdimension = 1`` is recomended firstly to avoid freeze your computer.
#' @param maxscale max scale to compute.
#' @param plot if `TRUE`, plot diagram.
#'
#' @return persistent diagram.
#' @family persistent homology computation
#' @seealso [TDA::ripsDiag()], [last_pd()], [plot.pd()], [autoplot.pd()]
#' @export
#' @examples
#' library(TDA)
#'
#' circle <- circleUnif(100)
#' circle.diag <- compute_pd(circle, maxdimension = 1, maxscale = 1)
#' # you can see plotted persistent diagram.
compute_pd <- function(X, maxdimension, maxscale, plot = TRUE) {
  pd <- TDA::ripsDiag(X, maxdimension = maxdimension, maxscale = maxscale) %>% as_pd
  set_last_pd(pd)
  return(pd)
}

#' Compute persistent landscape from the persistent diagram
#'
#' Uses [TDA::landscape()] to compute persistent landscape.
#' The main difference between the backend function is that
#' `compute_pl` computes for all dimensions of the persistent diagram at once.
#'
#' The persistent landscape is efficient way to count the betti number of
#' persistent diagram.
#' Because it can reduce useless holes that has scaller persistence and
#' convert the diagram to a vector.
#' We can apply analzing methods to persistent landscape easily than
#' the persistent diagram.
#'
#' @param pd persistent diagram object inherits `pd`.
#' @return persistent landscape object inherits `pl`.
#' @export
#' @seealso [compute_pd], [TDA::landscape()], [autoplot.pl()], [count_local_maximal()]
#' @examples
#' anulus <- anulusUnif(100)
#' anulus.pd <- compute_pd(anulus, 1, 1)
#'
#' anulus.pl <- compute_pl(anulus.pd)
#' plot(anulus.pl)
compute_pl <- function(pd) {
  diagram <- pd %>% extract_diagram %>% as_diagram
  dimension <- 1:attr(diagram, "maxdimension")
  scale <- attr(diagram, "scale")
  tseq <- seq(min(scale), max(scale), length.out = 500)

  purrr::map(dimension, ~ TDA::landscape(diagram, dimension = ., tseq = tseq)) %>%
    dplyr::bind_cols() %>%
    setter::set_names(dimension %>% as.character) %>%
    tibble::rowid_to_column("tseq") %>%
    tidyr::gather(dim, value, -tseq) %>%
    dplyr::mutate(dim = as.integer(dim)) %>%
    setter::set_class(c("pl", "tbl_df", "tbl", "data.frame")) %>%
    setter::set_attributes(pd = pd %>% as_pd)
}

#' Smooth persistent landscape using `stats::smooth.spline()`
#'
#' @param pl `pl` object.
#' @param spar smoothing parameters to be passed [stats::smooth.spline()].
#' @return smoothed persistent landscape as numeric vector.
#' @seealso [count_smooth_maximal()], [stats::smooth.spline()]
#' @export
compute_smooth_pl <- function(pl, spar = seq(0, 1, 0.1)) {
  assert_that(inherits(pl, "pl"))

  pl %>%
    tidyr::crossing(spar = spar) %>%
    tidyr::nest(tseq, value, .key = lands) %>%
    dplyr::mutate(smooth = purrr::pmap(., function(dim, spar, lands)
      stats::smooth.spline(lands$tseq, lands$value, spar = spar))) %>%
    dplyr::select(-lands) %>%
    setter::set_class(c("smooth_pl", "tbl_df", "tbl", "data.frame"))
}

#' Title
#'
#' @param X
#' @param maxdimension
#' @param maxscale
#' @param rate
#' @param plot
#' @param ret
#'
#' @return
#' @export
#'
#' @examples
calcSubsamplePhom <- function(X, maxdimension, maxscale, rate = 0.5, plot = T, ret = F) {
  assertthat::assert_that(assertthat::is.number(rate) && rate <= 1 && rate >= 0)
  subX <- X[sample(nrow(X), nrow(X) * rate), ]
  calcPhom(subX, maxdimension, maxscale, plot, ret)
}
