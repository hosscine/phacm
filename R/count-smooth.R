# 平滑化して平??<U+383C><U+3E37>??<U+393C><U+3E34>?<U+383C><U+3E33>?ク数を数える。ダイアグラ??<U+393C><U+3E63>?<U+3E30>直入力可??<U+383C><U+3E32>
#' Title
#'
#' @param PD
#' @param dimension
#' @param thresh
#' @param spar
#' @param plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
countSmoothLocalMaximalPD <- function(pd, dimension, thresh = NULL, spar = seq(0, 1, 0.1), plot = F,
                                      ...) {
  PL <- computePL(pd, dimension = dimension)
  if (is.null(thresh))
    thresh <- max(PL)/4
  estimate <- countSmoothLocalMaximalPL(PL, thresh, spar, plot, ...)
  return(estimate)
}

#' Title
#'
#' @param PL
#' @param dimension
#' @param thresh
#' @param spar
#' @param plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
countSmoothLocalMaximalPL <- function(pl, thresh = max(pl)/4, spar = seq(0, 1, 0.1), plot = F, ...) {
  smPL.list <- lapply(spar, function(sp) stats::smooth.spline(x = 1:length(pl), y = pl, spar = sp))

  estimate <- smPL.list %>% sapply(countLocalMaximalPL, thresh) %>% mean
  if (!plot)
    return(estimate)

  # if plot == true then
  elp <- myfs::overwriteEllipsis(..., x = 0, type = "n", xlim = c(0, pl %>% length), ylim = c(0,
                                                                                              pl %>% max))
  elp <- myfs::softwriteEllipsis(..., append = elp, xlab = "(Birth + Death) / 2", ylab = "(Death - Birth) / 2")
  do.call(graphics::plot, elp)
  graphics::abline(thresh, 0, col = 2)
  col <- smPL.list %>% length %>% grDevices::rainbow()
  for (i in 1:length(smPL.list)) graphics::lines(smPL.list[[i]]$y, col = col[i])

  return(estimate)
}
