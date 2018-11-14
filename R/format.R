#' Extract persistent diagram
#'
#' Extracts persistent diagram from bad behavier object computed by `TDA`` packagee.
#' The object is frequently a list contains diagram.
#' So you need to extract diagram by this function to handle diagram.
#'
#' @param x `list` object that contains `diagram` computed by such as [TDA::ripsDiag()]
#' @return extracted `diagram` object.
#' @export
extract_diagram <- function(x) {
  if ("list" %in% class(x))
    return(x$diagram)
  else if (c("pd", "diagram") %in% class(x) %>% any %>% magrittr::not())
    stop("the object dons not contain diagram")
  else
    return(x)
}

#' Convert `diagram` to `pd`
#'
#' @param x `diagram` object computed by `TDA` package.
#' @return `pd` object.
#' @export
#' @seealso [is_pd()], [tidy_pd()]
as_pd <- function(x) {
  if (is_pd(x)) return(x)
  x %<>% extract_diagram %>% tidy_pd
  return(x)
}

#' Test if the object is a pd
#'
#' @param x object.
#' @return TRUE if the object inherits from the pd class.
#' @export
is_pd <- function(x) inherits(x, "pd") & is.recursive(x)

#' Replace `Inf` value to finite value for `pd`
#'
#' @param pd `pd` object.
#' @param replace the value to replace `Inf`
#' @return `pd` object not including `Inf`.
#' @export
finite_pd <- function(pd, replace = attr(pd, "scale")[2]) {
  assert_that(is_pd(pd))
  pd[pd == Inf] <- replace
  return(pd)
}

#' Convert the `diagram` object to `pd` object that inherits `tibble`
#'
#' @param x `diagram` object.
#' @return `pd` object.
#' @export
#' @seealso [tibble::tibble]
tidy_pd <- function(x) {
  x %<>% extract_diagram
  tibble::tibble(dim = x[, "dimension"] %>% as.integer,
                      birth = x[, "Birth"],
                      death = x[, "Death"]) %>%
  setter::copy_attributes(x, c("maxdimension", "scale")) %>%
  setter::set_class(c("pd", "data.frame"))
}
