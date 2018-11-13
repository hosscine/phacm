#' Title
#'
#' @param PD
#'
#' @return
#' @export
#'
#' @examples
rawPD <- function(pd) {
  if (class(pd) == "list")
    pd <- pd$diagram
  class(pd) <- "diagram"
  return(pd)
}


#' Title
#'
#' @param PD
#' @param replace
#'
#' @return
#' @export
#'
#' @examples
finitePD <- function(pd, replace = attr(pd, "scale")[2]) {
  pd <- rawPD(pd)
  pd[pd == Inf] <- replace
  return(pd)
}

#' Title
#'
#' @param PD
#'
#' @return
#' @export
#'
#' @examples
tidyPD <- function(pd) {
  pd %>% rawPD %>% setter::set_class("matrix") %>% tibble::as.tibble() %>% tibble::rowid_to_column("id") %>%
    tidyr::gather(time, value, -id, -dimension)
}
