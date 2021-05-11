
#' @export
cut_time_of_day <- compiler::cmpfun(function(x, l) {
  if(length(l) > 0){
    tstr_to_sec <- function(t_str) {
      #"09:00:00" to sec of day
      as.numeric(as.POSIXct(paste("1970-01-01", t_str), "UTC")) %% 86400L
    }

    #handle tzone
    tz <- indexTZ(x)
    sec_of_day = {
      lt = as.POSIXlt(index(x), tz = tz)
      lt$hour *60*60 + lt$min*60 + lt$sec
    }
    sec_begin  = lapply(l,function(x) tstr_to_sec(x[1]))
    sec_end    = lapply(l,function(x) tstr_to_sec(x[2]))

    rule <- sapply(seq_along(sec_begin), function(x){
      paste('(sec_of_day >= sec_begin[[', x,']]', ' & ', 'sec_of_day <= sec_end[[', x,']])',sep ='')
    }) %>%
      paste(collapse = ' | ')

    return(x[eval(parse(text = rule)),])
  }else{
    return(x)
  }

})


convert.time.series <- function (fr, return.class)
{
  if ("quantmod.OHLC" %in% return.class) {
    class(fr) <- c("quantmod.OHLC", "zoo")
    return(fr)
  }
  else if ("xts" %in% return.class) {
    return(fr)
  }
  if ("zoo" %in% return.class) {
    return(as.zoo(fr))
  }
  else if ("ts" %in% return.class) {
    fr <- as.ts(fr)
    return(fr)
  }
  else if ("data.frame" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("matrix" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
}


#' Title
#'
#' @param Symbols character vector
#' @param env environment, where assign value
#' @param return.class character, which class to return
#' @param index.class character, which index class to return
#' @param from character, start date
#' @param to character, end date
#' @param adjust bool, if TRUE then 00:00:00 - > 23:59:59
#' @param period character, one of 'day', '1min', '10min', 'hour'
#' @param ... additional args
#'
#' @return nothing or table
#' @export getSymbols.Finam_
#'
getSymbols.Finam_ <- function (Symbols, env, return.class = "xts", index.class = "Date",
                              from = "2007-01-01", to = Sys.Date(), adjust = FALSE, period = "day",
                              col_funs = NULL, timecuts = list(),
                              ...)
{
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  default.return.class <- return.class
  default.from <- from
  default.to <- to
  if (missing(verbose))
    verbose <- FALSE
  if (missing(auto.assign))
    auto.assign <- FALSE
  if(verbose){
    cat(paste0('Downloading: ', Symbols, ', from: ', from, ', to: ', to, '\n'))
  }
  if(is.numeric(period)){
    ind_to_period <- TRUE
    if(period %% 60 == 0){
      p <- 7
      ind_to_period <- period != 60
    }else if(period %% 30 == 0){
      p <- 6
      ind_to_period <- period != 30
    }else if(period %% 15 == 0){
      p <- 5
      ind_to_period <- period != 15
    }else if(period %% 10 == 0){
      p <- 4
      ind_to_period <- period != 10
    }else if(period %% 5 == 0){
      p <- 3
      ind_to_period <- period != 5
    }else{
      p <- 2
      ind_to_period <- period != 1
    }
  }else{
    p <- 0
    if ("tick" == period)
      p <- 1
    if ("1min" == period)
      p <- 2
    if ("5min" == period)
      p <- 3
    if ("10min" == period)
      p <- 4
    if ("15min" == period)
      p <- 5
    if ("30min" == period)
      p <- 6
    if ("hour" == period)
      p <- 7
    if ("day" == period)
      p <- 8
    if ("week" == period)
      p <- 9
    if ("month" == period)
      p <- 10
    if (p == 0) {
      stop(paste("Unknown period ", period))
    }
  }

  finam.HOST <- "export.finam.ru"
  finam.URL <- "/table.csv?d=d&market=1&f=table&e=.csv&dtf=1&tmf=1&MSOR=1&sep=1&sep2=1&at=1&"
  fr <- NULL
  for (i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class), default.return.class,
                           return.class)
    from <- getSymbolLookup()[[Symbols[[i]]]]$from
    from <- if (is.null(from)) {
      default.from
    }else{
      from
    }
    to <- getSymbolLookup()[[Symbols[[i]]]]$to
    to <- if (is.null(to)) {
      default.to
    }else{
      to
    }
    from.y <- as.numeric(strsplit(as.character(as.Date(from,
                                                       origin = "1970-01-01")), "-", )[[1]][1])
    from.m <- as.numeric(strsplit(as.character(as.Date(from,
                                                       origin = "1970-01-01")), "-", )[[1]][2]) - 1
    from.d <- as.numeric(strsplit(as.character(as.Date(from,
                                                       origin = "1970-01-01")), "-", )[[1]][3])
    to.y <- as.numeric(strsplit(as.character(as.Date(to,
                                                     origin = "1970-01-01")), "-", )[[1]][1])
    to.m <- as.numeric(strsplit(as.character(as.Date(to,
                                                     origin = "1970-01-01")), "-", )[[1]][2]) - 1
    to.d <- as.numeric(strsplit(as.character(as.Date(to,
                                                     origin = "1970-01-01")), "-", )[[1]][3])
    Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
    Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]],
                           Symbols.name)
    # if (verbose)
    #   cat("downloading ", Symbols.name, ".....\n\n")
    Symbols.ids <- get_ids(Symbols.name) #finam.stock.list[Symbols.name]
    for(Symbols.id in Symbols.ids){
      tryCatch({
        if (is.na(Symbols.id)) {
          if (verbose)
            cat("Don't know about", Symbols[[i]], "\n\n")
          next
        }
        stock.URL <- paste(finam.URL, "p=", p, "&em=", Symbols.id,
                           "&df=", from.d, "&mf=", from.m, "&yf=", from.y, "&dt=",
                           to.d, "&mt=", to.m, "&yt=", to.y, "&cn=", Symbols.name,
                           sep = "")
        # if (verbose)
        #   cat(stock.URL)
        tmp <- tempfile()
        if (p == 1) {
          lts <- rusquant:::http.get(finam.HOST, paste(stock.URL, "&datf=6",
                                                       sep = ""), referer = "http://www.finam.ru/analysis/export/default.asp",
                                     verbose = FALSE)
          write(lts, file = tmp)
        }else {
          stock.URL <- paste("http://", finam.HOST, stock.URL,
                             "&datf=1", sep = "")
          #print(stock.URL)
          Sys.sleep(1)
          download.file(stock.URL, destfile = tmp, quiet = TRUE)
          #print('here')
        }
        #print('__________________________________________________________')
        fr <- read.csv(tmp, as.is = TRUE, colClasses = "character")
        #print(tail(fr))
        #print('__________________________________________________________')
        unlink(tmp)
        # if (verbose)
        #   cat("done.\n")
        if (p == 1) {
          fr <- xts(apply(as.matrix(fr[, (5:6)]), 2, as.numeric),
                    as.POSIXct(strptime(paste(fr[, 3], fr[, 4]),
                                        "%Y%m%d %H%M%S")), src = "finam", updated = Sys.time())
          colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)),
                                c("Close", "Volume"), sep = ".")
        }
        else if (p > 7) {
          x <- apply(as.matrix(fr[, (5:9)]), 2, as.numeric)
          if(class(x) == 'numeric'){
            x <- rbind(x)
          }
          fr <- xts(x,
                    as.Date(strptime(fr[, 3], "%Y%m%d")), src = "finam",
                    updated = Sys.time())
          colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)),
                                c("Open", "High", "Low", "Close", "Volume"),
                                sep = ".")
        }
        else {
          x <- apply(as.matrix(fr[, (5:9)]), 2, as.numeric)
          if(class(x) == 'numeric'){
            x <- rbind(x)
          }
          fr <- xts(x,
                    as.POSIXct(strptime(paste(fr[, 3], fr[, 4]),
                                        "%Y%m%d %H%M%S")), src = "finam", updated = Sys.time())
          colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)),
                                c("Open", "High", "Low", "Close", "Volume"),
                                sep = ".")
        }
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if(!is.null(col_funs)){
          fr <- lapply(col_funs, function(fun){
            do.call(fun, args = list(fr))
          }) %>%
            Reduce('cbind', .)
        }
        if (is.xts(fr) && p > 7)
          indexClass(fr) <- index.class

        if(is.xts(fr) && is.numeric(period) && ind_to_period){
          nms <- colnames(fr)
          index(fr) <- index(fr) - 60
          fr <- to.period(fr, period='minutes', k = period)
          index(fr) <- index(fr) + 60
          colnames(fr) <- nms
        }
        if(!period %in% c('day','week','month') && !is.null(timecuts)){
          fr <- cut_time_of_day(fr, timecuts)
        }else{
          if(is.xts(fr) && period %in% c('day','week','month')){
            dates <- as.POSIXct(paste0(index(fr), " ","19:00"),
                                tz = "", format = "%Y-%m-%d %H:%M")
            index(fr) <- as.Date(dates)
          }
        }
        if (auto.assign){
          assign(Symbols[[i]], fr, env)
        }
        if (i >= 5 && length(Symbols) > 5) {
          message("pausing 1 second between requests for more than 5 symbols")
          Sys.sleep(1)
        }

        if(length(fr) > 0) break
      }, error = function(e) {})
    }
    if(length(fr) == 0){
      stop('Problems with downloading')
    }

  }
  if (auto.assign)
    return(Symbols)
  #print(tail(fr))
  return(fr)
}

#' @export getSymbols.Finam
getSymbols.Finam <- function(Symbols, from = Sys.Date() - 1000, to = Sys.Date(), auto.assign = FALSE, env, ...){
  to <- min(as.Date(to), Sys.Date())
  from <- as.Date(from)
  this.env <- environment()
  suppressWarnings({
    for(Symbol in Symbols){
      period <- to - from
      nparts <- 1
      parts <- list()
      while(TRUE){
        tryCatch({
          parts[[1]] <- getSymbols.Finam_(Symbols = Symbol, from = to - period,
                                          to = to, auto.assign = FALSE, env = this.env, ...)
          break
        },
        error = function(e) {
          period <<- floor(period / 2)
          nparts <<- nparts * 2
          if(period < 1){
            stop('period less then 1')
          }
        })
      }
      if(nparts > 1){
        part <- 2
        while(part <= nparts){
          tryCatch({
            parts[[length(parts) + 1]] <- getSymbols.Finam_(Symbols = Symbol, from = to - period * part,
                                                            to = to - period * (part - 1) - 1, auto.assign = FALSE,
                                                            env = this.env, ...)
          },
          error = function(e){
            part <<- nparts
          })
          part <- part + 1
        }
      }
      # browser()
      x <- parts %>% Reduce(function(x, y){
        rbind(y[paste0('/', index(x)[1] - 1)], x)
      }, .)
      if(length(Symbols) > 1 || auto.assign){
        assign(Symbol, x, env)
      }else{
        return(x)
      }
    }
  })
  if(length(Symbols) > 1 || auto.assign){
    return(Symbols)
  }

}





getUSDFIX <- function(from,to = Sys.Date(),env = globalenv(),auto.assign = TRUE,index_ft = c("Date","POSIXct")){
  index_ft <- index_ft[1]
  stopifnot(index_ft %in% c("Date","POSIXct"))
  usdrub_path <- paste0("http://moex.com/export/derivatives/currency-rate.aspx?language=en&currency=USD_RUB&moment_start=",
                        from,"&moment_end=",to)
  x <- httr::GET(usdrub_path)
  tmp <- httr::content(x,"text",encoding = "UTF-8")
  tmp1 <- strsplit(tmp,"<rate moment=")
  tmp2 <- lapply(tmp1[[1]][-1],function(x){
    out.date <- strsplit(x,"\\\"")[[1]][2]
    out.price <- strsplit(x,"\\\"")[[1]][4]
    c(out.date, out.price)
  })
  tmp.dates <- sapply(tmp2,"[[",1)
  tmp.values <- as.numeric(sapply(tmp2,"[[",2))
  tmp.evening_index <- grep("18:",tmp.dates)
  if(index_ft == "Date"){
    dates <- as.Date(tmp.dates[tmp.evening_index])
  }else if(index_ft == "POSIXct"){
    dates <- as.POSIXct(paste0(as.Date(tmp.dates[tmp.evening_index]), " ","19:00"),
                               tz = "", format = "%Y-%m-%d %H:%M")
  }
  usdrub <- xts(tmp.values[tmp.evening_index],dates )
  colnames(usdrub)<-"Close"
  if(auto.assign){
    assign("USDFIX",usdrub,env)
  }else{
    return(usdrub)
  }

}









