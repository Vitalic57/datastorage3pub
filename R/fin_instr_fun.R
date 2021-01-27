
instrument <- function (primary_id, ..., currency, multiplier=1, tick_size = NULL,
                        trade = TRUE,
          identifiers = NULL, type = NULL, assign_i = FALSE, overwrite = TRUE)
{
  if (is.null(primary_id)) {
    stop("you must specify a primary_id for the instrument")
  }
  raw_id <- primary_id
  primary_id <- make.names(primary_id)
  if (missing(currency) || is.null(currency) || (!missing(currency) &&
                                                 !FinancialInstrument:::is.currency.name(currency))) {
    stop("currency ", currency, " must be defined first")
  }
  if (!hasArg(identifiers) || is.null(identifiers))
    identifiers = list()
  if (!is.list(identifiers)) {
    warning("identifiers", identifiers, "do not appear to be a named list")
  }
  if (raw_id != primary_id) {
    identifiers <- c(identifiers, raw_id = raw_id)
  }
  arg <- list(...)
  if (is.list(arg[["..."]])) {
    if (length(arg) == 1)
      arg <- arg[["..."]]
    else {
      targ <- arg[["..."]]
      arg[["..."]] <- NULL
      arg <- c(arg, targ)
    }
  }
  if (!is.null(arg$src)) {
    sarg <- list()
    sarg[[primary_id]] <- arg$src
    quantmod:::setSymbolLookup(sarg)
  }
  ident_str <- tolower(c("X.RIC", "RIC", "CUSIP", "SEDOL",
                         "OSI", "Bloomberg", "Reuters", "ISIN", "CQG", "TT", "Yahoo",
                         "Google"))
  lnarg <- tolower(names(arg))
  pos_arg <- which(lnarg %in% ident_str)
  identifiers <- c(identifiers, arg[pos_arg])
  arg[pos_arg] <- NULL
  if (!is.numeric(multiplier) || length(multiplier) > 1) {
    stop("multiplier must be a single number")
  }
  if (!is.null(tick_size) && (!is.numeric(tick_size) | length(tick_size) >
                              1)) {
    stop("tick_size must be NULL or a single number")
  }
  if (is.null(type)) {
    tclass = "instrument"
  }
  else tclass = unique(c(type, "instrument"))
  if (FinancialInstrument:::is.currency.name(primary_id) && !inherits(FinancialInstrument:::getInstrument(primary_id,
                                                              type = "currency"), "exchange_rate")) {
    oid <- primary_id
    primary_id <- tail(make.names(c(FinancialInstrument:::ls_instruments(), oid),
                                  unique = TRUE), 1)
    warning(paste(oid, "is the name of a currency. Using",
                  primary_id, "for the primary_id of this", type))
    identifiers <- c(identifiers, ticker = oid)
  }
  else if ((primary_id %in% FinancialInstrument:::ls_instruments()) && !overwrite &&
           isTRUE(assign_i)) {
    stop(paste("an instrument with primary_id", primary_id,
               "already exists in the .instrument environment.",
               "Set overwrite=TRUE to overwrite."))
  }
  tmpinstr <- list(primary_id = primary_id, currency = currency, trade = trade,
                   multiplier = multiplier, tick_size = tick_size, identifiers = identifiers,
                   type = type)
  if (length(arg) >= 1) {
    tmpinstr <- c(tmpinstr, arg)
  }
  class(tmpinstr) <- tclass
  if (assign_i) {
    assign(primary_id, tmpinstr, envir = as.environment(FinancialInstrument:::.instrument))
    return(primary_id)
  }
  else return(tmpinstr)
}


add_currency_to_dots <- function(this, dots){
  currencies <- ls_currencies(this)
  if(is.null(currencies)){
    if("currency" %in% names(dots)){
      currency(this, dots[['currency']])
    }else{
      currency(this, PARAMS('currency'))
    }
  }
  currencies <- ls_currencies(this)
  if(!"currency" %in% names(dots)){
    if(length(currencies) == 0){
      stop("Please, define currency of this first")
    }else if(length(currencies) > 1){
      stop("Please, choose currency of instrument")
    }else{
      dots[['currency']] <- currencies
    }
  }
  return(dots)
}


#' Add stock
#'
#' @param this Data
#' @param ... args to FinancialInstrument::stock
#'
#' @return Data object
#' @rdname stock
#' @method stock Data
#' @export
stock.Data <- function(this, ...){
  f <- 'stock'
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace("instrument", instrument, "FinancialInstrument")
  assignInNamespace(".instrument", this$envir, "FinancialInstrument")
  tryCatch({
    dots <- list(...)
    if(!'src' %in% names(dots)){
      dots[['src']] <- PARAMS('src')
    }
    dots <- add_currency_to_dots(this, dots)
    do.call(eval(parse(text = paste0('FinancialInstrument::',f))), args = dots)
    if('primary_id' %in% names(dots)){
      setOrder(this, c(getOrder(this), dots[['primary_id']]))
    }else{
      setOrder(this, c(getOrder(this), dots[[1]]))
    }

  },
  finally =
    assignInNamespace(".instrument", tmp, "FinancialInstrument"))
  return(invisible(this))
}


#' Add stock
#'
#' @param this Data
#' @param ... args to FinancialInstrument::stock
#'
#' @return Data object
#' @rdname index
#' @method index Data
#' @export
index.Data <- function(this, ...){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace("instrument", instrument, "FinancialInstrument")
  assignInNamespace(".instrument", this$envir, "FinancialInstrument")
  tryCatch({
    dots <- list(...)
    if(!'src' %in% names(dots)){
      dots[['src']] <- PARAMS('src')
    }
    dots <- add_currency_to_dots(this, dots)
    do.call('index_', args = dots)
    if('primary_id' %in% names(dots)){
      setOrder(this, c(getOrder(this), dots[['primary_id']]))
    }else{
      setOrder(this, c(getOrder(this), dots[[1]]))
    }

  },
  finally =
    assignInNamespace(".instrument", tmp, "FinancialInstrument"))
  return(invisible(this))
}



#' @return
#' @export
#' @rdname tbl
#' @method tbl Data
#'
#' @examples
#' \dontrun{
#' data <- Data() %>%
#' modify("from", '2015-01-01') %>%
#'   stock("AAPL",
#'         src ='yahoo',
#'         currency = 'USD') %>%
#'   tbl("ETFS",
#'       download = c("XLB", 'ACWI'), # multiple instruments
#'       currency = c("USD", "RUB"), # multiple currencies
#'       ex_rate =  list(c(NA, "USDRUB"), # exchange rate for each instrument for the first call
#'                       "USDJPY"), # exchange rate for the second call
#'       fun_table = list(c(function(x) x * 10,  function(x) x / 2), # function to apply for each instrument for the first call
#'                        function(x) x * 10), # function for the second call
#'       price_align =TRUE, # align with closee
#'       fun_component = 'Ad', # apply this function to each instrument before additional actions
#'       action_seq = c("fun", 'ex_rate', 'cbind', "fun", "price", "ex_rate", "na.locfl")) %>% # actions to apply
#'   tbl("Derived",
#'       download = "XLB",
#'       price_align=TRUE,
#'       fun_table = function(x){
#'         x * ETFS[,1] # we can use defined above names
#'       },
#'       fun_component ='Ad'
#'   ) %>%
#'   getSymbols
#'}
tbl.Data <- function(this,
                            name,
                            download=NULL,
                            one_by_one=TRUE,
                            price_align=FALSE,
                            fun_table=NULL,
                            fun_component=NULL,
                            fun_download="getSymbols",
                            currency=NULL,
                            ex_rate=NULL,
                            action_seq=c("cbind", "fun", "price", "ex_rate"),
                            ...){
  if(!is.null(currency)){
    for(x in currency){
      if(!x %in% ls_currencies(this)){
        currency(this, x)
      }
    }
  }
  if(!is.null(ex_rate)){
    for(ex in unlist(ex_rate)){
      if(is.na(ex)){
        next
      }
      if(!ex %in% ls_exchange_rates(this)){
        counter_cur <- substr(gsub("[\\./0-9]", "", ex), 4, 6)
        base_cur <- substr(gsub("[\\./0-9]", "", ex), 1, 3)
        if(PARAMS('src') == 'bloomberg'){
          exchange_rate(this, ex, src = 'bloomberg', download = paste0(base_cur, counter_cur, " Curncy"), trade = FALSE)
        }else if(PARAMS('src' == 'yahoo')){
          if(base_cur == 'USD'){
            exchange_rate(this, ex, src = 'yahoo', download = paste0(counter_cur, "=X"), trade = FALSE)
          }else{
            exchange_rate(this, ex, src = 'yahoo', download = paste0(base_cur, counter_cur, "=X"), trade = FALSE)
          }
        }
      }
    }
  }

  # set environment to functions
  if(!is.null(fun_table)){
    if(is.function(fun_table)){
      fun_table <- list(fun_table)
    }else if(is.character(fun_table)){
      fun_table <- list(fun_table)
    }

    for(i in seq_along(fun_table)){
      if(is.function(fun_table[[i]])){
        environment(fun_table[[i]]) <- this
      }else if(is.list(fun_table[[i]])){
        for(j in seq_along(fun_table[[i]])){
          if(is.function(fun_table[[i]][[j]])){
            environment(fun_table[[i]][[j]]) <- this
          }
        }
      }
    }
  }

  if(!is.null(ex_rate)){
    if(!is.list(ex_rate)){
      ex_rate <- list(ex_rate)
    }
  }
  this$tableord <- c(this$tableord, name)
  this$tablesenv[[name]] <- list(name=name,
                                  download=download,
                                  fun_table=fun_table,
                                  fun_component=fun_component,
                                  fun_download=fun_download,
                                  price_align=price_align,
                                  one_by_one=one_by_one,
                                  currency=currency,
                                  ex_rate=ex_rate,
                                  action_seq=action_seq,
                                  args_download = list(...))
  return(invisible(this))
}


#' Add exchange rate
#'
#' For detailed description see FinancialInstrument::exchange_rate
#'
#' @param this Data
#' @param primary_id character
#' @param currency character
#' @param counter_currency character
#' @param tick_size numeric
#' @param identifiers list
#' @param assign_i logical
#' @param overwrite logical
#' @param multiplier numeric
#' @param ... params
#'
#' @return Data object
#' @rdname exchange_rate
#' @method exchange_rate Data
#' @export
exchange_rate.Data <- function(this,
                                    primary_id = NULL,
                                    currency = NULL,
                                    counter_currency = NULL,
                                    tick_size = 0.01,
                                    identifiers = NULL,
                                    assign_i = TRUE,
                                    overwrite = TRUE,
                                    multiplier =1,
                                    ... ){
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace(".instrument", this$envir, "FinancialInstrument")
  tryCatch({
    if (is.null(primary_id) && !is.null(currency) && !is.null(counter_currency)) {
      primary_id <- c(outer(counter_currency, currency, paste,
                            sep = ""))
      same.same <- function(x) substr(x, 1, 3) == substr(x,
                                                         4, 6)
      primary_id <- primary_id[!same.same(primary_id)]
    }
    else if (is.null(primary_id) && (is.null(currency) || is.null(counter_currency))) {
      stop(paste("Must provide either 'primary_id' or both",
                 "'currency' and 'counter_currency'"))
    }
    if (!isTRUE(overwrite) && isTRUE(assign_i) && any(in.use <- primary_id %in%
                                                      (li <- ls_instruments()))) {
      stop(paste(paste("In exchange_rate(...) : ", "overwrite is FALSE and primary_id",
                       if (sum(in.use) > 1)
                         "s are"
                       else " is", " already in use:\n", sep = ""), paste(intersect(primary_id,
                                                                                    li), collapse = ", ")), call. = FALSE)
    }
    if (length(primary_id) > 1) {
      for(pr in primary_id){
        exchange_rate(this, pr, identifiers = identifiers, multiplier = multiplier,
                      assign_i = assign_i, ... = ...)
      }
    }
    if (is.null(currency))
      currency <- substr(gsub("[\\./0-9]", "", primary_id), 4, 6)
    if (is.null(counter_currency))
      counter_currency <- substr(gsub("[\\./0-9]", "", primary_id), 1, 3)
    if (!exists(currency, where = this$envir, inherits = TRUE)) {
      currency(this,currency)
    }
    if (!exists(counter_currency, where = this$envir, inherits = TRUE)) {
      currency(this, counter_currency)
    }
    FinancialInstrument::instrument(primary_id = primary_id, currency = currency,
               multiplier = multiplier, tick_size = tick_size, identifiers = identifiers,
               ..., counter_currency = counter_currency, type = c("exchange_rate",
                                                                  "currency"), assign_i = assign_i)

  },
  finally =
    assignInNamespace(".instrument", tmp, "FinancialInstrument"))
  return(invisible(this))
}






