#' Creates Data object
#'
#' @return Data object
#' @export
Data <- function(){
  thisEnv <- new.env()
  me <- list(
    ._period  = "day", # one of min,hour,day,week,month
    ._freq = 1,
    from = quote('2000-01-01'),
    to = quote(Sys.Date()),
    columns = 'Ad',
    ._shift =0,
    ._candles = FALSE,
    na_locf = TRUE,
    na_omit = TRUE,
    ncol = 0,
    nrow = 0,
    multiple_currencies = FALSE,
    currency = 'USD',
    price_columns = c('open', 'high', 'low', 'close', 'adjusted', 'dividends', 'splits'),
    'open' = NULL,
    'high' = NULL,
    'low' = NULL,
    'close' = NULL,
    'adjusted' = NULL,
    'dividends' = NULL,
    'splits' = NULL,
    'ex_rates' = NULL,
    nontraded = list(),
    tz = Sys.timezone(),
    envir = new.env(), #Financial instrument environment
    tablesenv = new.env() # info for additional tables
  )
  me <- list2env(me, thisEnv)
  parent.env(thisEnv[['envir']]) <- emptyenv()
  ## Set the name for the class
  #class(me) <- "Datastorage"
  class(me) <- append("Data", class(me))
  return(me)
}



#' Returns period for building spread from storage
#'
#' @param this Data
#'
#' @return character, period
#' @export
#' @rdname getBuildPeriod
#' @method getBuildPeriod Data
getBuildPeriod.Data <- function(this, recalc = FALSE){
  # if(is.null(this$._period_build) || recalc){
  #   this$._period_build <- calcActualPeriod(this$._period, this$._freq)
  # }
  # return(this$._period_build)
  return(this$._period)
}

#' Sets period for building spread from storage
#'
#' @param this Data
#'
#' @return Data
#' @rdname setBuildPeriod
#' @method setBuildPeriod Data
#' @export
setBuildPeriod.Data <- function(this, period){
  this[['._period_build']] <- period
  return(this)
}

calcActualPeriod <- function(period, freq){
  if(period == "min"){
    if(freq %% 30 == 0){
      period <- "30min"
    }else if(freq %% 15 == 0){
      period <- "15min"
    }else if(freq %% 10 == 0){
      period <- "10min"
    }else if(freq %% 5 == 0){
      period <- "5min"
    }else{
      period <- "1min"
    }
  }
  return(period)
}

#' is.Date
#'
#' @param x Data
#' @return bool, if object is Data or not
#' @export
#'
is.Data <- function(x){
  inherits(x, "Data")
}

#' print.Data
#'
# dots <- list(...)
#' @param x Data
#' @param ... here can be additional arguments
#'
#' @return some text in console
#' @export
#'
print.Data <- function(x, ...){
  print.default(x)
}



#' Change settings in Data
#'
#' @param this Data
#' @param ... one of c("period", "currency", "freq", "from", "to", 'columns', 'candles', "na_omit", "na_locf")
#'
#' * period - input for period argument of xts::to.period function
#'
#' * currency - currency of the model, all traded instruments will be converted to that currency
#'
#' * freq - input for k argument of xts::to.period function
#'
#' * from - start date of downloads
#'
#' * to - end date of downloads
#' 
#' * tz - timezone
#'
#' * columns - character vector with elements from c('cl', 'ad', 'op', 'hi', 'lo', 'vo', 'di', 'sp')
#'
#' * candles - logical, whether save candles of instruments or not in candles slot
#'
#' * na_locf - logical, whether apply na.locf to cbinded data or not
#'
#' * na_omit - logical, whether apply na.omit to cbinded traded instruments and then create tables or not.
#' If na_locf argument is TRUE and na_omit is FALSE, then despite of na_omit will be na.locf(fromLast = TRUE)
#' for not to cut initial points where instruments were not existed.
#'
#' @rdname modify
#' @method modify Data
#' @return Data object
#' @export
modify.Data <- function(this, ...){
  dots <- rlang::enexprs(...)
  for(objective in names(dots)){
    data <- dots[[objective]]
    switch(objective,
           col=,
           column=,
           columns=,
           col_funs={
             this$columns <- sapply(eval(data, envir = parent.frame()), function(name){
               switch(tolower(name),
                      ad=,
                      adj=,
                      adjusted= 'Ad',
                      sp=,
                      split=,
                      splits = 'Sp',
                      di=,
                      div=,
                      divs=,
                      dividend=,
                      dividends = 'Di',
                      cl = ,
                      close = {
                        'Cl'
                      },
                      hi = ,
                      high ={
                        'Hi'
                      },
                      lo = ,
                      low = {
                        'Lo'
                      },
                      op = ,
                      open = {
                        'Op'
                      },
                      vo = ,
                      vol = ,
                      volume = {
                        'Vo'
                      },
                      NULL)

             })
           },
           period = {
             if(is.numeric(data)){
               period <- abs(trunc(data))
               if(period == 0){
                 stop('Period should be more than 0')
               }
             }
             this$._period <- eval(data, envir = parent.frame())
             # data <- period2xts(eval(data, envir = parent.frame()))
             # if(is.null(data)){
             #   stop('no such period')
             # }
             # this$._period <- data
             # this$._period_build <- getBuildPeriod(this, recalc = TRUE)

           },
           # build_period = ,
           # period_build = {
           #   stopifnot(data %in% periods)
           #   this$._period_build <- data
           # },
           currency = {
             this$currency <- eval(data, envir = parent.frame()) %>% toupper
           },
           # freq = {
           #   data <- eval(data, envir = parent.frame())
           #   if(!is.numeric(data)){
           #     stop('freq must be numeric')
           #   }
           #   this$._freq <- data
           #   this$._period_build <- getBuildPeriod(this, recalc = TRUE)
           # },
           from = {
             this$from <- call('as.Date', data)
           },
           to = {
             this$to <- call('as.Date', data)
           },
           candle= ,
           candles=,
           bars= {
             this$._candles <- eval(data, envir=parent.frame())
           },
           timezone=,
           tz={
             this[['tz']] <- eval(data, envir = parent.frame())
           },
           na.omit=,
           na.rm=,
           na_omit=,
           na_rm={
             this$na_omit <- eval(data, envir=parent.frame())
           },
           na.locf=,
           na_locf = {
             this$na_locf <- eval(data, envir=parent.frame())
           }
           )
  }
  return(invisible(this))
}


#' Updates instruments metadata
#'
#' @param this Data object
#' @param data character vector of instruments' names
#' @param src character one of c("yahoo","iShares","masterDATA","md","morningstar","SPDR","TTR")
#' @param ... additional parameters, can be viewed in FinancialInstrument's methods
#' @rdname update_instruments
#' @method update_instruments Data
#' @export
update_instruments.Data <- function(this, data,
                                         src = c("yahoo","iShares","masterDATA","md","morningstar","SPDR","TTR"), ...
){
  src <- src[1]
  tmp <- FinancialInstrument:::.instrument
  assignInNamespace(".instrument", this$envir, "FinancialInstrument")
  tryCatch({
    stopifnot(src %in% c("yahoo","iShares","masterDATA","md","morningstar","SPDR","TTR"))
    switch(src ,
           yahoo = {
             FinancialInstrument::update_instruments.yahoo(data,...)
           },
           iShares = {
             FinancialInstrument::update_instruments.iShares(data,...)
           },
           masterDATA = {
             FinancialInstrument::update_instruments.masterDATA(data,...)
           },
           md = {
             FinancialInstrument::update_instruments.md(data,...)
           },
           morningstar = {
             FinancialInstrument::update_instruments.morningstar(data,...)
           },
           SPDR = {
             FinancialInstrument::update_instruments.SPDR(data,...)
           },
           TTR = {
             FinancialInstrument::update_instruments.TTR(data,...)
           })},
    finally =
      assignInNamespace(".instrument", tmp, "FinancialInstrument"))
}


#' Removes data from Data object
#'
#' @param this Data object
#' @rdname rm_data
#' @method rm_data Data
#' @return nothing
#' @export
#'
rm_data.Data <- function(this){
  suppressWarnings({
    x <- sapply(names(this), function(x){
      if(!is.null(nrow(this[[x]]))){
        rm(list = x, envir = this)
      }
    })
  })
}

#' Copy Data object
#'
#' @param this Data object
#'
#' @rdname cloneData
#' @method cloneData Data
#' @export
cloneData.Data <- function(this){
  # e <-  this %>%
  #   ls %>%
  #   mget(.,envir = this) %>%
  #   as.environment
  # class(e) <- append("Data",class(e))
  # return(e)
  temp <- tempfile()
  saveRDS(this, temp)
  return(readRDS(temp))
}


#' Gets dates by indexes
#'
#' @param this Data
#' @param indexes numeric vector
#'
#' @return vector of dates
#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex Data
getDateByIndex.Data <- function(this, indexes = NULL){
  if(!is.null(indexes)){
    indexes[indexes < 0] <- this[['nrow']] + 1 + indexes[indexes < 0]
    return(this$dates[indexes])
  }else{
    return(this$dates)
  }
}


nrow.Data <- function(this){
  nrow(this[[this$price_table]])
}


ncol.Data <- function(this){
  ncol(this[[this$price_table]])
}


colnames.Data <- function(this){
  colnames(this[[this$price_table]])
}
