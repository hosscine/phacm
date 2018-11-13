#' Title
#'
#' @param X
#' @param maxdimension
#' @param maxscale
#' @param plot
#' @param ret
#'
#' @return
#' @export
#'
#' @examples
computePD <- function(X, maxdimension, maxscale) {
  pd <- TDA::ripsDiag(X, maxdimension = maxdimension, maxscale = maxscale)$diagram
  class(pd) <- "diagram"
  diagram <<- pd
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
