#' Title
#'
#' @param PD
#'
#' @return
#' @export
#'
#' @examples
rawPD <- function(PD) {
  if (class(PD) == "list") return(PD$diagram)
  return(PD)
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
finitePD <- function(PD, replace = 10 ** 10) {
  PD <- rawPD(PD)
  PD[PD == Inf] <- replace
  return(PD)
}

#' Title
#'
#' @param PD
#'
#' @return
#' @export
#'
#' @examples
tidyPD <- function(PD) {
  PD %>% rawPD %>% setter::set_class("matrix") %>% tibble::as.tibble() %>%
    tibble::rowid_to_column("id") %>%
    tidyr::gather(time, value, -id, -dimension)
}
