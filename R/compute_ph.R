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
calcPhom <- function(X, maxdimension, maxscale, plot = T, ret = F) {
  diagram <<- TDA::ripsDiag(X, maxdimension = maxdimension, maxscale = maxscale, printProgress = T)
  if (plot)
    showPersistentDiagram(diagram)
  if (ret)
    return(diagram)
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

#' Title
#'
#' @param PD
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
computePL <- function(PD, dimension) {
  PD <- rawPD(PD)
  scale <- attr(PD, "scale")
  tseq <- seq(min(scale), max(scale), length.out = 500)
  PL <- TDA::landscape(PD, dimension = dimension, tseq = tseq)
  return(PL)
}
