index_ <- function (primary_id, currency = NULL, multiplier = 1, tick_size = 0.01,
                        identifiers = NULL, assign_i = TRUE, overwrite = TRUE, ...)
{
  if (is.null(currency)){
    currencies <- ls_currencies(this)
    if(length(currencies) == 0){
      stop("Please, define currency of this first")
    }else if(length(currencies) > 1){
      stop("Please, choose currency of instrument")
    }else{
      currency <- currencies
    }
  }
  if (!isTRUE(overwrite) && isTRUE(assign_i) && any(in.use <- primary_id %in%
                                                    (li <- FinancialInstrument::ls_instruments()))) {
    stop(paste(paste("In index(...) : ", "overwrite is FALSE and primary_id",
                     if (sum(in.use) > 1)
                       "s are"
                     else " is", " already in use:\n", sep = ""), paste(intersect(primary_id,
                                                                                  li), collapse = ", ")), call. = FALSE)
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, function(prime_id){
      instrument(primary_id = prime_id, currency = currency,
                 multiplier = multiplier, tick_size = tick_size, identifiers = identifiers,
                 ..., type = "index", assign_i = assign_i, trade = FALSE)
    })
    return(if (assign_i) unname(out) else out)
  }
  instrument(primary_id = primary_id, currency = currency,
             multiplier = multiplier, tick_size = tick_size, identifiers = identifiers,
             ..., type = "index", assign_i = assign_i, trade = FALSE)
}


ls_indexes_ <- function (pattern = NULL, match = TRUE)
{
  symbols <- FinancialInstrument::ls_instruments(pattern, match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = FinancialInstrument:::.instrument), silent = TRUE)
    if (inherits(tmp_instr, "index") && inherits(tmp_instr,
                                                 "instrument")) {
      tmp_symbols <- c(tmp_symbols, instr)
    }
  }
  tmp_symbols
}


#' ls_indexes
#'
#' @param this Data
#'
#' @rdname ls_indexes
#' @method ls_indexes Data
#' @export
ls_indexes.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace('.instrument', this$envir, 'FinancialInstrument')
  tryCatch({
    ls_indexes_(...)
  },
  finally =
    assignInNamespace('.instrument', tmp, 'FinancialInstrument'))
}
