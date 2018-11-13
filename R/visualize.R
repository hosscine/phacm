#' Title
#'
#' @param diag
#' @param maxdimention
#' @param scale
#' @param point
#'
#' @return
#' @export
#'
#' @examples
plot.diagram <- function(x, ...) {
  pd <- finitePD(x)
  maxdimention <- attr(pd, "maxdimension") + 1
  scale <- attr(pd, "scale")

  graphics::plot(pd[, 2], pd[, 3], xlim = scale, ylim = scale, cex = point, cex.axis = point,
                 col = pd[, 1] + 1, pch = pd[, 1] + 1, xlab = "Birth", ylab = "Death", cex.lab = 1.5, cex.main = 2)
  graphics::abline(0, 1)

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
#' @param PD
#'
#' @return
#' @export
#'
#' @examples
print.diagram <- function(x, ...) {
  update_summary <- function(x) {
    x$summary <- paste0("Persistent Diagram [", x$summary %>% stringr::str_sub(6, -6), "]")
    return(x)
  }
  x %>% rawPD %>% setter::set_class("matrix") %>% tibble::trunc_mat() %>% update_summary %>% print
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
