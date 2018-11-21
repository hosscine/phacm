#' Count local maximal of persistent landscape with smoothing
#'
#' Calculate fuzy betti number of the persistent diagram
#' like human expert.
#'
#' The number of local maximal of persistent landscape is corresponding to
#' the betti number of the data.
#' However, the counting result is frequently larger than the true betti number
#' because of some jaggies on the persistent landscape.
#' Human experts who analyze persistent landscape calculate the betti number
#' fuzzly to interpret the jaggies.
#' `count_smooth_maximal` imitates the method of human expert's analysis
#' by counting local maximal of multi resolutional smoothed persistent landscape
#' and meaning it.
#'
#' @param x `pd` or `pl` object.
#' @param ... other arguments passed to specific methods.
#' @return counting result.
#' @seealso [count_local_maximal()], [stats::smooth.spline()]
#' @references R.Futagami, N. Yamada, T. Shibuya.
#' "Infering Underlying Manifold of Data by the Use of Persistent Homology Analysis."
#' Proc. of 7th Workshop on Computational Topology in Image Context, The Spain, Jan. 2018.
#' @export
count_smooth_maximal <- function(x, ...) {
  UseMethod("count_smooth_maximal")
}

#' Count local maximal of persistent landscape with smoothing
#'
#' @param x `pd` object.
#' @param exist.method the function to compute threshold
#' for judging whther the cycle exists or not on each dimension.
#' @param cutoff.method the function to compute threshold
#' for taking out small persistence holes.
#' @param spar smoothing parameters to be passed [stats::smooth.spline()].
#' @param plot if `TRUE`, plot smoothing result.
#' @param ... ignored.
#' @export
#' @rdname count_smooth_maximal
count_smooth_maximal.pd <- function(x,
                                    exist.method = zero_threshold,
                                    cutoff.method = zero_hat_threshold,
                                    spar = seq(0, 1, 0.1),
                                    plot = TRUE, ...) {
  assert_that(is.function(exist.method))
  assert_that(is.function(cutoff.method))

  pl <- compute_pl(pd)
  count_smooth_maximal(pl,
                       exist.method = exist.method, cutoff.method = cutoff.method,
                       spar = spar, plot = plot, ...)
}

#' Count local maximal of persistent landscape with smoothing
#'
#' @param x `pl` object.
#' @inheritParams count_smooth_maximal.pd
#' @export
#' @rdname count_smooth_maximal
count_smooth_maximal.pl <- function(x,
                                    exist.method = zero_threshold,
                                    cutoff.method = zero_hat_threshold,
                                    spar = seq(0, 1, 0.1),
                                    plot = TRUE, ...) {
  assert_that(is.function(exist.method))
  assert_that(is.function(cutoff.method))

  sms.pl <- compute_smooth_pl(x) %>%
    mutate(d = str_sub(dim, 4) %>% as.integer) %>%
    nest(-spar, -dim)
  exist.thresh <- exist.method(x)
  cutoff <- . %>% {2 * cutoff.method(x) * nd_area(1) / nd_area(.)}

  exist <- x %>%
    tidyr::gather(dim, value, -tseq) %>%
    tidyr::nest(-dim, .key = lands) %>%
    dplyr::mutate(exist = purrr::map_lgl(lands, ~ .$value %>% max > exist.thresh)) %>%
    magrittr::use_series(exist)

  result <- sms.pl %>%
    dplyr::mutate(count = purrr::map_int(
      data, ~ count_local_maximal(x = .$smooth[[1]], thresh = cutoff(.$d)))) %>%
    dplyr::select(-data) %>%
    tidyr::nest(-dim, .key = detail) %>%
    dplyr::mutate(betti = map_dbl(detail, ~ mean(.$count)) * exist) %>%
    dplyr::bind_cols(exist = exist)

  if (!plot) return(result)

  # # if plot == true then
  # elp <- myfs::overwriteEllipsis(..., x = 0, type = "n", xlim = c(0, pl %>% length), ylim = c(0,
  #                                                                                             pl %>% max))
  # elp <- myfs::softwriteEllipsis(..., append = elp, xlab = "(Birth + Death) / 2", ylab = "(Death - Birth) / 2")
  # do.call(graphics::plot, elp)
  # graphics::abline(thresh, 0, col = 2)
  # col <- sms.list %>% length %>% grDevices::rainbow()
  # for (i in 1:length(sms.list)) graphics::lines(sms.list[[i]]$y, col = col[i])

  return(result)
}
