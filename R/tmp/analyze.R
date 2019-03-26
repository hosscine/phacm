#' Title
#'
#' @param band
#' @param diag
#' @param ignore.0
#'
#' @return
#' @export
#'
#' @examples
upperCycles <- function(band, diag = diagram, ignore.0 = F) {
  diag <- rawPD(diag)
  pdiag <- calcPersistence(diag)
  if (ignore.0)
    pdiag <- pdiag[pdiag[, "dimension"] != 0, ]
  upperdiag <- pdiag[pdiag[, "Persistence"] >= band, ]
  uppercycle <- upperdiag[order(upperdiag[, "Persistence"], decreasing = T), ]
  if (nrow(uppercycle) == 0)
    warning("there is no upper cycle") else return(uppercycle)
}

#' Compute persistence of pd.
#'
#' @param pd persistent diagram.
#'
#' @return `pd` object with persistence.
#' @export
#'
persistence <- function(pd) {
  pd %>%
    dplyr::mutate(persistence = death - birth) %>%
    inherit_pd(pd)
}
