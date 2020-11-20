#poloniex
#' @export getSymbols.Poloniex
getSymbols.Poloniex <- function(Symbols, env, return.class = "xts",
                                from = "2007-01-01", to = Sys.Date(),  period = "day",
                                col_funs = NULL, timecuts = list(), date_format = NULL,
                                auto.assign = FALSE,
                                ...){
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if(is.null(date_format)){
    date_format <- '%Y-%m-%d %H:%M:%S'
  }
  if (missing(verbose))
    verbose <- FALSE
  if (missing(auto.assign))
    auto.assign <- FALSE
  if(is.numeric(p)){
    p <- p * 60
  }else{
    p <- 0
    if ("5min" == period)
      p <- 300
    if ("15min" == period)
      p <- 900
    if ("30min" == period)
      p <- 1800
    if ("2hour" == period)
      p <- 7200
    if ("4hour" == period)
      p <- 14400
    if ("day" == period)
      p <- 86400
    if (p == 0) {
      stop(paste("Unknown period ", period))
    }
  }

  fr <- NULL
  for (i in 1:length(Symbols)) {
    if(class(from)[1] == 'character'){
      if(grepl(':', from) || nchar(from) > 10){
        from <- as.numeric(strptime(from, format = date_format))
      }else{
        from <- as.numeric(strptime(paste0(from, ' 00:00:00'), format = '%Y-%m-%d %H:%M:%S'))
      }
    }else if(class(from)[1] == 'Date'){
      from <- as.numeric(strptime(paste0(from, ' 00:00:00'), format = '%Y-%m-%d %H:%M:%S'))
    }else if(class(from)[1] == 'POSIXct'){
      from <- as.numeric(from)
    }else{
      stop('wrong date format')
    }

    if(class(to)[1] == 'character'){
      if(grepl(':', to) || nchar(to) > 10){
        to <- as.numeric(strptime(to, format = date_format))
      }else{
        to <- as.numeric(strptime(paste0(to, ' 00:00:00'), format = '%Y-%m-%d %H:%M:%S'))
      }
    }else if(class(to)[1] == 'Date'){
      to <- as.numeric(strptime(paste0(to, ' 00:00:00'), format = '%Y-%m-%d %H:%M:%S'))
    }else if(class(to)[1] == 'POSIXct'){
      to <- as.numeric(to)
    }else{
      stop('wrong date format')
    }

    poloniex.public <- PoloniexR::PoloniexPublicAPI()
    fr <- PoloniexR::ReturnChartData(theObject = poloniex.public,
                                  pair      = Symbols[i],
                                  from      = as.POSIXct(from, origin="1970-01-01"),
                                  to        = as.POSIXct(to, origin="1970-01-01"),
                                  period    = p)
    indexTZ(fr) <- ''
    #fr <- .ohlc[[paste0(symbol1,symbol2)]]
    if (verbose)
      cat("done.\n")
    colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols[i])),
                          c("High", "Low", "Open", "Close", "Volume", 'QuoteVolume', 'WeightedAverage'),
                          sep = ".")
    fr <- convert.time.series(fr = fr, return.class = return.class)
    if(!is.null(col_funs)){
      fr <- lapply(col_funs, function(fun){
        do.call(fun, args = list(fr))
      }) %>%
        Reduce('cbind', .)
    }
    if(!period %in% c('day') && !is.null(timecuts) && length(timecuts) > 0){
      fr <- cut_time_of_day(fr, timecuts)
    }
    if (auto.assign){
      assign(Symbols[i], fr, env)
    }
    if (i >= 5 && length(Symbols) > 5) {
      message("pausing 1 second between requests for more than 5 symbols")
      Sys.sleep(1)
    }
  }
  if (auto.assign)
    return(Symbols)
  return(fr)
}




