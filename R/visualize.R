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
  assert_that(length(scale) == 2)

  elp <- elp::overwrite_elp(...,
                            x = pd$birth, y = pd$death,
                            col = pd$dim + 1,
                            pch = pd$dim + 1)
  elp <- elp::softwrite_elp(..., append = elp,
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
  if (!assertthat::has_name(x, "value") || !assertthat::has_name(x, "dim")){
    print(tibble::trunc_mat(x))
    return()
  }

  thresh <- x %>% zero_hat_double_threshold %>% magrittr::divide_by(8)
  betti <- x %>% count_local_maximal(thresh)

  cat("# Persistent Landscape\n")
  for (d in x$dim %>% unique) {
    val <- x %>% dplyr::filter(dim == d) %>% magrittr::use_series(value)
    cat("\n- Dimension", d, "\n")
    cat("  - cycle:", betti[d], "or less\n")
    cat("  - max  :", max(val) %>% round(digit), "\n")
    cat("  - mean :", mean(val) %>% round(digit), "\n")
    cat("  - var  :", stats::var(val) %>% round(digit), "\n")
  }
}

#' Autoplot persistent diagram
#'
#' @param object `pd` object.
#' @param ... ignored.
#' @return `ggplot` object.
#' @seealso [compute_pd()], [plot.pd()]
#' @export
autoplot.pd <- function(object, ...) {
  scale <- attr(object, "scale")
  object$dim %<>% as.factor
  diagonal <- list(scale, -scale) %>%
    purrr::map(~ scales::cbreaks(.)$breaks) %>%
    unlist %>% unique

  object %>%
    ggplot2::ggplot(aes(x = birth, y = death, group = dim)) +
    ggplot2::geom_abline(intercept = diagonal, slope = 1, colour = "white") +
    ggplot2::geom_abline(intercept = 0, slope = 1, size = 1) +
    ggplot2::geom_point(aes(colour = dim, shape = dim), size = 2) +
    ggplot2::xlim(scale) +
    ggplot2::ylim(scale) +
    ggplot2::labs(x = "Birth", y = "Death",
                  colour = "Dimension", shape = "Dimension")
}

#' Autoplot persistent landscape
#'
#' @param object `pl` object.
#' @param ... ignored.
#' @return `ggplot` object.
#' @seealso [compute_pl()]
#' @export
autoplot.pl <- function(object, size = 1, ...) {
  object %<>% dplyr::mutate(dim = as.factor(dim))

  maximal <- object %>%
    dplyr::mutate(maximal = value %>% diff %>% sign %>%
                    diff %>% magrittr::equals(-2) %>% c(0, ., 0)) %>%
    dplyr::filter(maximal == TRUE)

  object %>%
    ggplot2::ggplot(aes(x = tseq, y = value, group = dim, colour = dim)) +
    ggplot2::geom_line(size = size) +
    ggplot2::labs(x = "(Birth + Death) / 2",
                  y = "(Birth - Death) / 2",
                  colour = "Dimension") +
    ggplot2::geom_segment(aes(x = tseq, y = 0, xend = tseq, yend = value),
                          data = maximal)
}

#' Autoplot smoothed persistent landscape
#'
#' @param object `smooth_pl` object.
#' @param dimension specific dimension. If `NULL`, plot for all dimension.
#' @param ... ignored.
#' @return `ggplot` object.
#' @seealso [compute_smooth_pl()]
#' @export
autoplot.smooth_pl <- function(object, dimension = NULL, ...) {
  if (is.numeric(dimension)) {
    p <- object %>%
      dplyr::filter(dim %in% dimension) %>%
      dplyr::mutate(smooth = purrr::map(smooth, ~ tibble::tibble(x = .$x, y = .$y))) %>%
      dplyr::mutate(spar = as.factor(spar)) %>%
      tidyr::unnest() %>%
      ggplot2::ggplot(aes(x, y, group = spar, colour = spar)) +
      ggplot2::geom_point(size = 0.5)
    return(p)
  }

  messy <- object %>%
    dplyr::mutate(smooth = purrr::map(smooth, ~ tibble::tibble(x = .$x, y = .$y))) %>%
    dplyr::mutate(dim = as.factor(dim)) %>%
    tidyr::unnest() %>%
    tidyr::spread(spar, y)

  ggplot2::ggplot(messy, aes(x, group = dim, colour = dim)) +
    purrr::map(messy %>% dplyr::select(-dim, -x), ~ ggplot2::geom_line(aes(y = .))) +
    ggplot2::geom_abline(intercept = 0, slope = 0)
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

#' Plot persistence histgram
#'
#' The histgram of persistence is usefull for checking effect of threshold.
#'
#' @param pd persistent diagram.
#'
#' @return `ggplot` object.
#' @export
#'
persistence_hist <- function(pd) {
  pd %<>% persistence()
  w <- mean(pd$persistence) / 4
  pd %>%
    ggplot2::ggplot(aes(persistence)) +
    ggplot2::geom_histogram(binwidth = w, alpha = 0.5) +
    ggplot2::geom_density(aes(y = w * ..count..))
}
