#' Format persistent diagram
#'
#' Extracts persistent diagram from bad behavier object computed by `TDA`` packagee.
#' The object is frequently a list contains diagram.
#' So you need to extract diagram by this function to handle diagram.
#'
#' @param pd persistent diagram.
#'
#' @return extracted persistent diagram.
#'
rawPD <- function(pd) {
  if (class(pd) == "list")
    pd <- pd$diagram
  else if (class(pd) != "diagram")
    stop("cannot extract persistent diagram from input")
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
