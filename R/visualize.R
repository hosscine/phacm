#' Plot persistent diagram.
#'
#' Plot diagram like `phom` package.
#'
#' @param x persistent diagram.
#' @param scale limitation of birth and death.
#' @param ... additional graphical arguments, see [plot], [plot.default] and [par].
#' @export
plot.pd <- function(x, scale = attr(rawPD(pd), "scale"), ...) {
  pd <- finitePD(x)
  maxdimention <- attr(pd, "maxdimension") + 1
  assert_that(myfs::is.range(scale))

  elp <- myfs::overwriteEllipsis(..., x = pd$Birth, y = pd$Death)
  elp <- myfs::softwriteEllipsis(..., append = elp,
                                 xlab = "Birth", ylab = "Death",
                                 xlim = scale, ylim = scale,
                                 col = pd$dimension + 1,
                                 pch = pd$dimension + 1)
  do.call(graphics::plot, elp)
  graphics::abline(0, 1)

  # graphics::plot(pd[, 2], pd[, 3], xlim = scale, ylim = scale, cex = point, cex.axis = point,
                 # col = pd[, 1] + 1, pch = pd[, 1] + 1, xlab = "Birth", ylab = "Death", cex.lab = 1.5, cex.main = 2)

  legends <- 0
  for (i in (0:(maxdimention - 1))) {
    # legends <- c(legends,bquote(H[.(i)]))
    legends <- c(legends, paste("dim", i))
  }
  legends <- legends[-1]

  graphics::legend(scale[2]/2 * 1.2, scale[2]/2, legend = sapply(legends, as.expression), col = 1:maxdimention,
                   pch = 1:maxdimention, cex = point, pt.cex = point)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.pd <- function(x, ...) {
  info <- paste0("Persistent Diagram [", nrow(x), "]")
  x %<>%
    setter::set_class(c("tbl_df", "tbl", "data.frame")) %>%
    tibble::trunc_mat()
  x$summary <- info
  print(x)
}

#' Title
#'
#' @param diag
#' @param band
#'
#' @return
#' @export
#'
#' @examples
showPhomBand <- function(diag = diagram, band) {
  diag <- rawPD(diag)
  max.scale <- attr(diag, "scale")[2]

  graphics::plot(1, type = "n", ylim = c(0, max.scale), xlim = c(0, max.scale), ann = F, axes = F)
  graphics::polygon(c(max.scale + 1, max.scale - band + 1, -1, -1), c(max.scale + 1, max.scale +
                                                                        1, band - 1, 0 - 1), col = "pink", border = 0)
  graphics::par(new = T)
  showPersistentDiagram(diag)
}
