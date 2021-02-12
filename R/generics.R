f_tryCatch_FI <- function(f,print = FALSE){
  #force(f)
  function(this,...){
    tmp <- FinancialInstrument:::.instrument
    assignInNamespace(".instrument", this$envir, "FinancialInstrument")
    tryCatch({
      #x <- get(f, "package:FinancialInstrument")(...)
      eval(parse(text = paste0('FinancialInstrument::',f,"(...)")))
      if(print){
        print(x)
        cat("\n")
      }
      return(this)
    },
    finally =
      assignInNamespace(".instrument", tmp, "FinancialInstrument"))
  }
}

f_tryCatch_FI_nm <- function(f){ #returns not this
  #force(f)
  function(this,...){
    tmp <- FinancialInstrument:::.instrument
    assignInNamespace(".instrument", this$envir, "FinancialInstrument")
    tryCatch({
      #get(f, "package:FinancialInstrument")(...)
      eval(parse(text = paste0('FinancialInstrument::',f,"(...)")))
    },
    finally =
      assignInNamespace(".instrument", tmp, "FinancialInstrument"))
  }
}

f_makeGen <- function(f){
  #force(f)
  function(this,...){
    UseMethod(f, this)
  }
}


#create generic functions
funs_all <- c("instrument_attr","add.identifier",
              "getInfo")
funs <- lapply(funs_all, f_makeGen)
names(funs) <- funs_all
list2env(funs,envir = environment())


#' @export
toPeriod <- function(this, ...){
  UseMethod('toPeriod', this)
}

#' @export
setTables <- function(this, ...){
  UseMethod('setTables', this)
}

#' @export
getTables <- function(this){
  UseMethod('getTables', this)
}

#' @export
getDataColumns <- function(this){
  UseMethod('getDataColumns', this)
}

#' @export
setDataColumns <- function(this,...){
  UseMethod('setDataColumns', this)
}

#' @export
getBuildPeriod <- function(this, ...){
  UseMethod('getBuildPeriod', this)
}

#' @export
setBuildPeriod <- function(this, ...){
  UseMethod('setBuildPeriod', this)
}

#' Adds new currency to this, it can add several currencies
#'
#' @param this Data
#' @param ... character vector
#'
#' @return Data object
#' @export
#'
currency <- function(this,...){
  UseMethod('currency', this)
}

#' Returns current currencies that this uses
#'
#' @param this Data
#'
#' @return Data object
#' @export
#'
ls_currencies <- function(this){
  UseMethod('ls_currencies', this)
}


#' Returns current currencies that this uses
#'
#' @param this Data
#' @param ... params
#'
#' @return Data object
#' @export
ls_indexes <- function(this, ...){
  UseMethod('ls_indexes', this)
}

#' Adds exchange currency rate
#'
#' @param this Data
#' @param ... character vector
#'
#' @return Data object
#' @export
#'
exchange_rate <- function(this,...){
  UseMethod('exchange_rate', this)
}

#' Adds stock to this
#'
#' @param this Data
#' @param ... character vector of stocks
#'
#' @return Data object
#' @export
#'
stock <- function(this, ...){
  UseMethod('stock', this)
}

#' Returns stocks in this
#'
#' @param this Data
#'
#' @return character vector of stocks
#' @export
#'
ls_stocks <- function(this){
  UseMethod('ls_stocks', this)
}

#' Returns instruments in this
#'
#' @param this Data
#'
#' @return character vector of instruments
#' @export
#'
ls_instruments <- function(this){
  UseMethod('ls_instruments', this)
}



#' Add table
#'
#' @param this Data
#' @param name character, name of table
#' @param download character,  names to download,
#' @param fun_component function / character name of function, function that will be applied to each component  in table after download
#' @param fun_table function / character name of function, function will be applied on your table after cbind components and before alignment
#' @param fun_download function/ list of functions/  character name of function or it can be a table or list of xts tables, this function will be used to download data. By default it is getSymbols
#' @param one_by_one logical, if true then full vector of download argument will be passed to getSymbols
#' @param currency character/NULL/list, name of currency. See example below
#' @param ex_rate character / NULL, identifier for defined exchange rate in the this
#' @param price_align logical, if true then table will be aligned with data_raw
#' @param action_seq character, this sequence describes how table should be created. Available values are:
#'
#' 1. cbind - Reduce('cbind') tables
#'
#' 2. ex_rate - apply currency exchange
#'
#' 3. na.locf - do na.locf
#'
#' 4. na.locfl - do  na.locf(fromLast=TRUE)
#'
#' 5. fun - apply function from fun_table
#'
#' 6. price - do aligning to price
#'
#' @param ... params for fun_download
#'
#' @return
#' @export
tbl <- function(this,
                name,
                download,
                one_by_one,
                price_align,
                fun_table,
                fun_component,
                fun_download,
                action_seq,
                ...){
    UseMethod('tbl', this)
}

#' Title
#'
#' @param this Data
#' @param ... character, instrument from this by id
#'
#' @return instrument
#' @export
#'
getInstrument <- function(this,...){
  UseMethod('getInstrument', this)
}


#' Returns character vector of futures' names in this
#'
#' @param this Data
#'
#' @return character vector, futures' names in this
#' @export
#'
ls_futures <- function(this, ...){
  UseMethod('ls_futures', this)
}


#' Returns names of exchange rates in this
#'
#' @param this
#'
#' @return character vector, names of exchange rates in this
#' @export
#'
ls_exchange_rates <- function(this){
  UseMethod('ls_exchange_rates', this)
}

#' Lets change arguments of this
#'
#' @param this Data object
#' @param ... which element, which value
#'
#' @return Data object
#' @export
#'
modify <- function(this, ...){
  UseMethod('modify', this)
}

#' Delete specific instruments from this
#'
#' @param this Data object
#' @param ... character vector, instruments to delete
#'
#' @return nothing
#' @export
rm_instruments <- function(this, ...){
  UseMethod('rm_instruments', this)
}

#' Builds information data frame
#'
#' @param this Data object
#' @param ... character, names of instruments and
#' character vector of fields from instruments
#'
#' @return data frame
#' @export
#'
buildHierarchy <- function(this, ...){
  UseMethod('buildHierarchy', this)
}


#' Deletes specific currencies from this
#'
#' @param this Data
#' @param ... character vector of currencies
#'
#' @return nothing
#' @export
#'
rm_currencies <- function(this, ...){
  UseMethod('rm_currencies', this)
}

#' Updates metadata of instruments from this
#'
#' @param this Data
#' @param ... character, method to update
#'
#' @return Data
#' @export
#'
update_instruments <- function(this, ...){
  UseMethod('update_instruments', this)
}

#' Get data from soures
#'
#' @param Symbols Data / character
#' @param ... params
#'
#' @return xts / Data
#' @rdname getSymbols
#' @export
getSymbols <- function(Symbols, ...){
  UseMethod('getSymbols', Symbols)
}


#' @export
getSymbols.default <- function( ..., src = 'yahoo'){
  if(src == 'yahoo'){
    src <- 'yahoo2'
  }else if(src == 'yahoo_'){
    src <- 'yahoo'
  }
  quantmod:::getSymbols(..., src = src)
}


#' Get dividends data from soures
#'
#' @param Symbols character
#' @param split.adjust logical, default TRUE
#' @param ... params
#'
#' @return xts / data.frame
#' @rdname getDividends
#' @export
getDividends <- function(...){
  dots <- list(...)
  if('src' %in% names(dots)){
    src <- dots[['src']]
    dots[['src']] <- NULL
  }else{
    src <- 'yahoo'
  }
  # if (src == 'yahoo') {
  #   return(do.call(getDividends.yahoo, dots))
  # }
  do.call(paste("getDividends.", src, sep = ""), args = dots)
}


#' Get splits data from soures
#'
#' @param Symbols character
#' @param ... params
#'
#' @return xts / data.frame
#' @rdname getSplits
#' @export
getSplits <- function(...){
  dots <- list(...)
  if('src' %in% names(dots)){
    src <- dots[['src']]
    dots[['src']] <- NULL
  }else{
    src <- 'yahoo'
  }
  if (src == 'yahoo') {
    x <- do.call(quantmod::getSplits, dots) %>% na.omit
    tryCatch({
      return(1 / x)
    }, error = function(e){})
    return(numeric(0))
  }
  do.call(paste("getSplits.", src, sep = ""), args = dots)
}


#' Add instruments to this by name, if it exists in Data.xlsx file
#'
#' @param this Data object
#' @param ... character vector
#'
#' @return Data object
#' @export
#'
addInstruments <- function(this, ...){
  UseMethod("addInstruments", this)
}

#' Delete all data from Data object
#'
#' @param this Data object
#'
#' @return nothing
#' @export
#'
rm_data <- function(this){
  UseMethod('rm_data', this)
}


#' Make copy of object
#'
#' @param this Data
#'
#' @return Data object
#' @export cloneData
#' @rdname cloneData
cloneData <- function(this){
  UseMethod('cloneData', this)
}

#' Update Data object
#'
#' @param this Data
#'
#' @return Data object
#' @export cloneData
#' @rdname cloneData
updateSymbols <- function(this){
  UseMethod('updateSymbols', this)
}

#' Delay currency
#'
#' @param this Data
#' @param ... ndays = numeric
#'
#' @return Data object
#' @export
#'
C2LFX <- function(this, ...){
  UseMethod('C2LFX', this)
}


#' Get last currency from finam and updates currency table
#'
#' @param this Data
#'
#' @return Data
#' @export
#'
getLastCurrency <- function(this){
  UseMethod('getLastCurrency', this)
}


#' @export
createSpread <- function(this, ...){
  UseMethod('createSpread', this)
}

#' @export
getDateByIndex <- function(this, ...){
  UseMethod('getDateByIndex', this)
}

#' @export
getShift <- function(this){
  UseMethod('getShift', this)
}

#' @export
setShift <- function(this, ...){
  UseMethod('setShift', this)
}

#' @export
getOrder <- function(this){
  UseMethod('getOrder', this)
}

#' @export
setOrder <- function(this, ...){
  UseMethod('setOrder', this)
}

#' @export
adjust <- function(this, ...){
  UseMethod('adjust', this)
}

# #' @export
# loadAdditional <- function(this, ...){
#   UseMethod('loadAdditional', this)
# }

#create s3 methods with try catch
#these methods do not return Data
# funs_nm <- c("ls_currencies","ls_stocks","ls_instruments", "getInstrument",
#              "ls_futures","ls_exchange_rates","buildHierarchy")
# funs <- lapply(funs_nm, f_tryCatch_FI_nm)
# names(funs) <- paste0(funs_nm,".Data")
# list2env(funs,envir = environment())




#create s3 methods with try catch from FinancialInstrument
#these methods return Data
funs_part <- setdiff(funs_all,c("update_instruments", "getSymbols",
                                "modify","addInstruments","getInfo",'rm_data','cloneData','C2LFX',
                                'getLastCurrency'))
funs <- lapply(funs_part, f_tryCatch_FI)
names(funs) <- paste0(funs_part,".Data")
list2env(funs,envir = environment())

#create defaults methods for functions from FinancialInstrument
# funs_part <- setdiff(funs_all,c("update_instruments","getSymbols","modify",
#                                 "addInstruments","getInfo",'rm_data','cloneData','C2LFX',
#                                 'getLastCurrency'))
# funs <- lapply(funs_part,function(x) get(x, "package:FinancialInstrument"))
#
# names(funs) <- paste0(funs_part,".default")
# list2env(funs,envir = environment())


