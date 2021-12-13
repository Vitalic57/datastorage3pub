

#' currency
#'
#' currency from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname currency
#' @method currency Data
#' @export
currency.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::currency(...)
  },
  finally = {
    assignInNamespace('.instrument', tmp, 'FinancialInstrument')
  }
    )
  if(length(ls_currencies(this)) > 1){
    this[['multiple_currencies']] <- TRUE
  }
  return(invisible(this))
}

#' rm_instruments
#'
#' rm_instruments from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname rm_instruments
#' @method rm_instruments Data
#' @export
rm_instruments.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::rm_instruments(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' instrument_attr
#'
#' instrument_attr from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname instrument_attr
#' @method instrument_attr Data
#' @export
instrument_attr.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::instrument_attr(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' add.identifier
#'
#' add.identifier from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname add.identifier
#' @method add.identifier Data
#' @export
add.identifier.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::add.identifier(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' ls_stocks
#'
#' ls_stocks from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname ls_stocks
#' @method ls_stocks Data
#' @export
ls_stocks.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::ls_stocks(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' ls_instruments
#'
#' ls_instruments from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname ls_instruments
#' @method ls_instruments Data
#' @export
ls_instruments.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::ls_instruments(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' ls_currencies
#'
#' ls_currencies from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname ls_currencies
#' @method ls_currencies Data
#' @export
ls_currencies.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::ls_currencies(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' ls_futures
#'
#' ls_futures from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname ls_futures
#' @method ls_futures Data
#' @export
ls_futures.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::ls_futures(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' ls_exchange_rates
#'
#' ls_exchange_rates from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname ls_exchange_rates
#' @method ls_exchange_rates Data
#' @export
ls_exchange_rates.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::ls_exchange_rates(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' getInstrument
#'
#' getInstrument from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname getInstrument
#' @method getInstrument Data
#' @export
getInstrument.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::getInstrument(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' buildHierarchy
#'
#' buildHierarchy from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname buildHierarchy
#' @method buildHierarchy Data
#' @export
buildHierarchy.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::buildHierarchy(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}

#' instrument_attr
#'
#' instrument_attr from FinancialInstrument library
#'
#' @param this Data
#'
#' @rdname instrument_attr
#' @method instrument_attr Data
#' @export
instrument_attr.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    FinancialInstrument::instrument_attr(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}
