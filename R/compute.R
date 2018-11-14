#' Compute persistent diagram from point cloud.
#'
#' Uses [TDA::ripsDiag()] to compute persistent diagram.
#' Then keeps diagram to avoid to recomputing.
#' The keeped diagram can be took out by [lastPD()].
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
#' @seealso [TDA::ripsDiag()], [lastPD()]
#' @export
#' @examples
#' library(TDA)
#'
#' circle <- circleUnif(100)
#' circle.diag <- computePD(circle, maxdimension = 1, maxscale = 1)
#' # you can see plotted persistent diagram.
computePD <- function(X, maxdimension, maxscale, plot = TRUE) {
  pd <- TDA::ripsDiag(X, maxdimension = maxdimension, maxscale = maxscale) %>% as_pd
  setLastPD(pd)
  return(pd)
}

#' Title
#'
#' @param PD
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
computePL <- function(pd, dimension) {
  pd <- rawPD(pd)
  scale <- attr(pd, "scale")
  tseq <- seq(min(scale), max(scale), length.out = 500)
  pl <- TDA::landscape(pd, dimension = dimension, tseq = tseq)
  class(pl) <- "landscape"
  return(pl)
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
