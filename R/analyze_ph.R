# パ�<U+383C><U+3E33>�シス�<U+383C><U+3E36>ンスが閾値以上�<U+383C><U+3E31>�サイクルを表示
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
  diag <- rawdiag(diag)
  pdiag <- calcPersistence(diag)
  if (ignore.0) 
    pdiag <- pdiag[pdiag[, "dimension"] != 0, ]
  upperdiag <- pdiag[pdiag[, "Persistence"] >= band, ]
  uppercycle <- upperdiag[order(upperdiag[, "Persistence"], decreasing = T), ]
  if (nrow(uppercycle) == 0) 
    warning("there is no upper cycle") else return(uppercycle)
}

#' Title
#'
#' @param diag
#'
#' @return
#' @export
#'
#' @examples
calcPersistence <- function(diag = diagram) {
  diag <- rawdiag(diag)
  diag. <- cbind(diag, diag[, "Death"] - diag[, "Birth"])
  colnames(diag.) <- c(colnames(diag), "Persistence")
  return(diag.)
}
