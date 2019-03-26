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
  else if (c("pd", "diagram", "tbl_df") %in% class(x) %>%
           any %>%
           magrittr::not())
    stop("this object x does not contain a diagram")
  else
    return(x)
}

#' Convert `TDA::diagram` to `pd`
#'
#' @param x `diagram` object computed by `TDA` package.
#' @return `pd` object.
#' @export
#' @seealso [is_pd()]
as_pd <- function(x) {
  if (phacm::is_pd(x)) return(x)

  ext_x <- x %>% phacm::extract_diagram()
  if ("diagram" %in% class(ext_x))
    new_x <- tibble::tibble(dim = ext_x[, "dimension"] %>% as.integer,
                            birth = ext_x[, "Birth"],
                            death = ext_x[, "Death"])

  new_x %<>% setter::copy_attributes(ext_x, c("maxdimension", "scale")) %>%
    setter::set_class(c("pd", "tbl_df", "tbl", "data.frame"))
  return(new_x)
}

#' Test if the object is a `pd`
#'
#' @param x object.
#' @return `TRUE` if the object inherits the `pd` class.
#' @seealso [as_pd()]
#' @export
is_pd <- function(x) inherits(x, "pd") & is.recursive(x)

#' Inherit attributes from `pd` object
#'
#' @param pd inherits target `pd`.
#' @param from inherits from.
#'
#' @return `pd` object with attributes of `from`.
#' @export
#'
inherit_pd <- function(pd, from) {
  pd %>% setter::copy_attributes(from, c("maxdimension", "scale")) %>%
    setter::set_class(c("pd", "tbl_df", "tbl", "data.frame"))
}

#' Convert `pd` to `TDA::diagram`
#'
#' @param pd `pd` object.
#' @return converted `TDA::diagram`
#' @export
as_diagram <- function(pd) {
  if (inherits(pd, "diagram")) return(pd)
  else if (!is_pd(pd))
    stop("can not convert to the diagram from ", class(pd)[1], " object")
  pd %>%
    as.matrix %>%
    setter::copy_attributes(pd, c("maxdimension", "scale")) %>%
    setter::set_class("diagram") %>%
    setter::set_colnames(c("dimension", "Birth", "Death"))
}

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
