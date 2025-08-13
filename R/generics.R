#' Citation information for OpenESM datasets
#' @param x An object to get citation information for
#' @param ... Additional arguments passed to methods
#' @export
cite <- function(x, ...) {
  UseMethod("cite")
}

#' Additional notes for OpenESM datasets
#' @param x An object to get notes for
#' @param ... Additional arguments passed to methods
#' @export
notes <- function(x, ...) {
  UseMethod("notes")
}

