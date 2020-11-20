#' Check for actions in data
#' 
#' A set of functions to check for appropriate Dividends and Splits column names within a data object, as well as the availability and position of those columns.
#'
#' @param x data object
#' @param which numeric, display position of match
#'
#' @export
#'
#' @rdname check_actions
has.Sp <- function (x, which = FALSE) {
  colAttr <- attr(x, "Sp")
  if (!is.null(colAttr)) 
    return(if (which) colAttr else TRUE)
  loc <- grep("Split", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @export
#'
#' @rdname check_actions
has.Di <- function (x, which = FALSE) {
  colAttr <- attr(x, "Di")
  if (!is.null(colAttr)) 
    return(if (which) colAttr else TRUE)
  loc <- grep("Dividend", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @export
#'
#' @rdname check_actions
Sp <- function (x) {
  if (has.Sp(x)) 
    return(x[, grep("Split", colnames(x), ignore.case = TRUE)])
  stop("subscript out of bounds: no column name containing \"Split\"")
}

#' @export
#'
#' @rdname check_actions
Di <- function (x) {
  if (has.Di(x)) 
    return(x[, grep("dividend", colnames(x), ignore.case = TRUE)])
  stop("subscript out of bounds: no column name containing \"Dividend\"")
}