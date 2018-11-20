#' Count local maximal of persistent landscape with smoothing
#'
#' @param x `pd` object or `pl` object.
#' @param ... other arguments passed to specific methods.
#' @return counting result.
#' @export
count_smooth_maximal <- function(x, ...) {
  UseMethod("count_smooth_maximal")
}


#' Title
#'
#' @param spar
#' @param plot
#' @param ...
#' @param x
#' @param exist.method
#' @param cutoff.method
#'
#' @return
#' @export
#'
#' @examples
count_smooth_maximal.pd <- function(x,
                                    exist.method = zero_threshold,
                                    cutoff.method,
                                    spar = seq(0, 1, 0.1),
                                    plot = TRUE, ...) {
  pl <- compute_pl(pd)
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
  assert_that()
  assert_that()

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
