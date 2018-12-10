.diagram_store <- function() {
  .last_diagram <- NULL

  list(
    get = function() .last_diagram,
    set = function(value) .last_diagram <<- value
  )
}
.store <- .diagram_store()

#' Set the last persistent diagram to be fetched by `last_pd()`
#'
#' @seealso [last_pd()]
#' @export
#' @keywords internal
set_last_pd <- function(value) .store$set(value)


#' Retrieve the last persistent diagram to be computed.
#'
#' @seealso [compute_pd()], [compute_dist_pd()]
#' @export
#' @keywords internal
last_pd <- function() .store$get()
