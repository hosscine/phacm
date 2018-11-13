.diagram_store <- function() {
  .last_diagram <- NULL

  list(
    get = function() .last_diagram,
    set = function(value) .last_diagram <<- value
  )
}
.store <- .diagram_store()

#' Set the last persistent diagram to be fetched by lastPD()
#'
#' @seealso [lastPD()]
#' @export
#' @keywords internal
setLastPD <- function(value) .store$set(value)


#' Retrieve the last persistent diagram to be computed.
#'
#' @seealso [computePD()]
#' @export
#' @keywords internal
lastPD <- function() .store$get()
