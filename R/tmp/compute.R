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
