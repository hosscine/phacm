#' Plot persistent diagram
#'
#' Plot diagram like `phom` package.
#'
#' @param x persistent diagram.
#' @param scale limitation of birth and death.
#' @param size size of plot text.
#' @param legend.size size of legend text.
#' @param ... additional graphical arguments,
#'   see [plot()], [plot.default()] and [par()].
#' @seealso [plot()]
#' @export
plot.pd <- function(x, scale = attr(x, "scale"), size = 1.5, legend.size = 1.5, ...) {
  pd <- finite_pd(x)
  maxdimention <- attr(pd, "maxdimension") + 1
  assert_that(myfs::is.range(scale))

  elp <- myfs::overwriteEllipsis(...,
                                 x = pd$birth, y = pd$death,
                                 col = pd$dim + 1,
                                 pch = pd$dim + 1)
  elp <- myfs::softwriteEllipsis(..., append = elp,
                                 xlab = "Birth", ylab = "Death",
                                 xlim = scale, ylim = scale,
                                 cex = size, cex.axis = size,
                                 cex.lab = size, cex.main = size)
  do.call(graphics::plot, elp)
  graphics::abline(0, 1)

  legends <- paste("dim", 1:maxdimention - 1)
  graphics::legend(x = scale[2]/2 * 1.2, y = scale[2]/2,
                   legend = legends,
                   col = 1:maxdimention, pch = 1:maxdimention,
                   cex = legend.size, pt.cex = legend.size)
}


#' Print persistent diagram
#'
#' Prints `pd` object like `tibble`.
#'
#' @param x `pd` object.
#' @param ... other arguments passed on to individual methods.
#' @export
#' @seealso [compute_pd()], [plot.pd()], [as_pd()]
#' @examples
print.pd <- function(x, ...) {
  info <- paste0("Persistent Diagram [", nrow(x), "]")
  x %<>%
    setter::set_class(c("tbl_df", "tbl", "data.frame")) %>%
    tibble::trunc_mat()
  x$summary <- info
  print(x)
}

#' Print persistent landscape
#'
#' Prints `pl` object to understand it quickly.
#'
#' @param x `pl` object.
#' @param ... other arguments passed on to individual methods.
#' @param digit integer to be passed [round()]
#' @seealso [compute_pl()]
#' @export
print.pl <- function(x, ..., digit = 3) {
  if (!assertthat::has_name(x, "tseq")){
    print.data.frame(x)
    return()
  }

  cat("# Persistent Landscape\n")
  thresh <- x %>% dplyr::select(-tseq) %>% unlist %>% max %>% magrittr::divide_by(4)
  for (d in colnames(x)[-1]) {
    val <- x[[d]]
    cat("\n- Dimension", stringr::str_sub(d, 4), "\n")
    cat("  - cycle:", val %>% count_local_maximal(thresh), "or less\n")
    cat("  - max  :", max(val) %>% round(digit), "\n")
    cat("  - mean :", mean(val) %>% round(digit), "\n")
    cat("  - var  :", var(val) %>% round(digit), "\n")
  }
}

#' Autoplot persistent landscape
#'
#' @param x `pl` object.
#' @param ... ignored.
#' @return `ggplot` object.
#' @export
autoplot.pl <- function(x, ...) {
  x %>% tidyr::gather(key, value, -tseq) %>%
    ggplot(aes(x = tseq, y = value, group = key, fill = key)) +
    geom_area(alpha = 0.5) +
    theme_bw() +
    labs(x = "(Birth + Death) / 2",
         y = "(Birth - Death) / 2",
         fill = "Dimension")
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
