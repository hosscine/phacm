#' Title
#'
#' @param X
#' @param maxdim
#' @param maxscale
#' @param const.band
#' @param maximum.thresh
#'
#' @return
#' @export
#'
#' @examples
bootstrap.homology <- function(X, maxdim, maxscale, const.band = 0, maximum.thresh = F) {
  # require(pracma)
  if (!("bootsSamples" %in% class(X)))
    stop("input must be bootsSamples")
  peak <- matrix(0, maxdim, length(X))
  # band <- ifelse(const.band > 0,const.band,hausdInterval(X, m=sample.size, B=times, alpha =
  # (1-confidence)))
  tseq <- seq(0, maxscale, length.out = 1000)
  diags <- lapply(X, function(x) calcPhom(x, maxdim, maxscale, ret = T, plot = F))
  print(sapply(diags, function(diag) calcDiagCentroid(diag)[3]))
  band <- ifelse(const.band == 0, max(sapply(diags, function(diag) calcDiagCentroid(diag)[3])),
                 const.band)
  print(band)

  for (t in 1:length(X)) {
    land <- lapply(1:maxdim, function(d) TDA::landscape(diags[[t]][[1]], dimension = d, KK = 1,
                                                        tseq = tseq))
    if (maximum.thresh)
      band <- max(sapply(land, max))/4
    for (d in 1:maxdim) {
      peak[d, t] <- countPeaksPL(X = land[[d]], thresh = (band/(2 * d)), tseq = tseq)
    }
  }

  dimnames(peak) <- list(paste0("dim", 1:maxdim), paste0("sample", 1:length(X)))
  bootstrap.summary <- list(peak = peak)
  bootstrap.summary <- append(bootstrap.summary, c(band = band, show.hole.density(peak)))
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}

#' Title
#'
#' @param diag
#'
#' @return
#' @export
#'
#' @examples
calcDiagCentroid <- function(diag = diagram) {
  if (class(diag) == "list")
    diag <- diag[[1]]
  diag <- diag[-which(diag[, 1] == 0), ]
  centroid <- apply(diag[, 2:3], 2, mean)
  cpersistence <- (centroid[2] - centroid[1])
  ret <- c(centroid, cpersistence, cpersistence * 2)
  names(ret) <- c("birth", "death", "persistence", "noizes.thresh")
  return(ret)
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
plot.smoothPhom <- function(x, ...) x <- show.hole.density(x$peak)

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
countSmoothLocalMaximalPD <- function(PD, dimension, thresh = 0, spar = seq(0, 1, 0.1), plot = F, ...) {
  PL <- computePL(PD, dimension = dimension)
  estimate <- countPeakPL(PL, spar, thresh, plot, ...)
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
countSmoothLocalMaximalPL <- function(PL, thresh = 0, spar = seq(0, 1, 0.1), plot = F, ...) {
  smPL.list <- lapply(spar, function(sp)
    stats::smooth.spline(x = 1:length(PL), y = PL, spar = sp))

  estimate <- smPL.list %>% sapply(countLocalMaximal, thresh) %>% mean
  if (!plot) return(estimate)

  # if plot == ture then
  elp <- myfs::overwriteEllipsis(..., x = 0, type = "n", ylim = c(0, max(PD)))
  elp <- myfs::softwriteEllipsis(..., append = elp,
                                 xlab = "(Birth + Death) / 2",
                                 ylab = "(Death - Birth) / 2")
  do.call(graphics::plot, elp)
  graphics::abline(thresh, 0, col = 2)
  col <- smPL.list %>% length %>% grDevices::rainbow
  for (i in 1:length(smPL.list))
    graphics::lines(smPL.list[[i]]$y, col = col[i])

  return(estimate)
}


# 1次関数の??<U+393C><U+3E34>?<U+383C><U+3E33>?ク数を数える Internal Function
#' Title
#'
#' @param x
#' @param weakcut
#' @param thresh
#' @param show.thresh
#'
#' @return
#' @export
#'
#' @examples
countLocalMaximalPL <- function(PL, thresh = 0) {
  if (class(PL) == "smooth.spline") PL <- PL$y
  PL[PL < thresh] <- 0
  lmax <- PL %>%  diff %>% sign %>% diff %>% magrittr::equals(-2) %>% sum
  return(lmax)
}

# ??<U+383C><U+3E34>次??<U+383C><U+3E33>??<U+383C><U+3E65>?<U+383C><U+3E31>?peak数??<U+383C><U+3E63>?<U+383C><U+3E38>???<U+393C><U+3E32>??<U+383C><U+3E36>度推定し<U+653C><U+3E66>??表示する??<U+383C><U+3E32>
# Internal FUnction
#' Title
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples
show.hole.density <- function(X) {
  dens <- apply(X, 1, stats::density)
  maxdim <- nrow(X)
  bootstrap.summary <- list()
  xlim = c(min(sapply(dens, function(den) min(den$x))), max(sapply(dens, function(den) max(den$x))))
  ylim = c(0, max(sapply(dens, function(den) max(den$y))))
  plot(1, type = "n", xlim = xlim, ylim = ylim, xlab = "betti number", ylab = "probability")
  for (d in 1:maxdim) {
    mhole <- mean(X[d, ])
    dhole <- dens[[d]][["x"]][which.max(dens[[d]][["y"]])]
    par(new = T)
    plot(dens[[d]], xlim = xlim, ylim = ylim, col = d + 1, ann = F, axes = F)
    print(paste0("dimension ", d, ", ", round(mhole, digits = 2), " mean hole, ", round(dhole),
                 " density hole"))
    bootstrap.summary[[paste0("dim", d, "mhole")]] <- mhole
    bootstrap.summary[[paste0("dim", d, "dhole")]] <- dhole
  }
  legend("topright", legend = paste("dim", 1:maxdim), pch = 0, col = 2:(maxdim + 1))
  return(bootstrap.summary)
}

#' Title
#'
#' @param confidence
#' @param maxscale
#' @param haus.dist
#' @param digit
#'
#' @return
#' @export
#'
#' @examples
calc.bootstrap.confidence <- function(confidence, maxscale, haus.dist, digit = 3) {
  Lb <- function(t, haus.dist) sum(length(which(haus.dist > t)))/length(haus.dist)
  max.dist <- max(haus.dist)
  from <- 0
  to <- max.dist
  for (i in 1:digit) {
    tseq <- seq(from, to = to, length.out = 10)
    # ErrorText(from,to,haus.dist,seq(from,to = to,length.out = 10))
    prob <- sapply(tseq, Lb, haus.dist)
    # print(prob)
    from <- tseq[which(prob < confidence)[1] - 1]
    # print(from)
    to <- tseq[which(prob < confidence)[1]]
  }
  c <- 2 * from
  return(c)
}
