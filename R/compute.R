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
#' @seealso [TDA::ripsDiag()], [last_pd()]
#' @export
#' @examples
#' library(TDA)
#'
#' circle <- circleUnif(100)
#' circle.diag <- computePD(circle, maxdimension = 1, maxscale = 1)
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
#' @seealso [compute_pd], [TDA::landscape()]
#' @examples
#' anulus <- anulusUnif(100)
#' anulus.pd <- computePD(anulus, 1, 1)
#'
#' anulus.pl <- computePL(anulus.pd)
#' plot(anulus.pl)
compute_pl <- function(pd) {
  pd %<>% extract_diagram %>% as_diagram
  dimension <- 1:attr(pd, "maxdimension")
  dimnames <- paste0("d", dimension)
  scale <- attr(pd, "scale")
  tseq <- seq(min(scale), max(scale), length.out = 500)
  purrr::map(dimension, ~ TDA::landscape(pd, dimension = ., tseq = tseq)) %>%
    setter::set_names(dimnames) %>%
    setter::set_class(c("pl")) %>%
    magrittr::inset2("pd", pd) %>%
    magrittr::inset2("tseq", tseq) %>%
    magrittr::inset2("dimnames", dimnames)
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
