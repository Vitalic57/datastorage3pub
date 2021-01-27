PARAMS <- settings::options_manager(src = 'yahoo', currency = 'USD', getSymbols_version = 'new')

#' Set or get option for datastorage package
#'
#' @param ... option names to retrieve option values
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'   \item{\code{src}}
#'   \item{\code{currency}}
#' }
#'
#' @return
#' @export
datastorage_options <- function(...){
  settings::stop_if_reserved(...)
  PARAMS(...)
}
