#' @export getSymbols.Binance
getSymbols.Binance <- function(Symbols, period='day', from=Sys.Date() - 30, to=Sys.time(), env, auto.assign=TRUE, tz=Sys.timezone(), ...){
  results <- list()
  startTime <- format(as.numeric(as.POSIXct(from)) * 1000, scientific = FALSE)
  endTime <- format(as.numeric(as.POSIXct(to)) * 1000, scientific = FALSE)
  period_text <- c("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
  period_num <- c(1, 3, 5, 15, 30, 60, 120, 240, 360, 480, 720, 1440, 4320, 10080, 21600)
  if(is.numeric(period)){
    periods <- rev(period_num)
    for(p in periods){
      if(period >= p){
        interval_num <- p
        break
      }
    }
  }else{
    interval_num <- switch(tolower(period),
                      'day'=,
                      "daily"=,
                      "d"=,
                      "1d"=1440,
                      "3d"=4320,
                      "week"=,
                      "weekly"=,
                      "1w"=10080,
                      '12h'=720,
                      '8h'=480,
                      "6h"=360,
                      "4h"=240,
                      "2h"=120,
                      "h"=,
                      "hour"=60,
                      "30m"=,
                      "30min"=30,
                      "15m"=,
                      "15min"=15,
                      "5m"=,
                      "5min"=5,
                      "3min"=3,
                      "1m"=,
                      "1min"=1,
                      stop('no such period')
    )
  }
  interval <- period_text[which(period_num==interval_num)]
  for(pair in Symbols){
    res <- list()
    while(TRUE){
      base_url <- "https://api.binance.com/api/v3/klines"
      url <- paste0(base_url, "?", "symbol=", pair, "&limit=1000", "&startTime=", startTime, 
                    "&endTime=", endTime, "&interval=", interval)
      ohlc_out <- jsonlite::fromJSON(url)
      class(ohlc_out) <- 'numeric'
      dates <- lubridate::with_tz(as.POSIXct(ohlc_out[,1] / 1000, origin='1970-01-01 00:00:00', tz='UTC'), tz)
      res[[length(res) + 1]] <- xts(ohlc_out[,c(2:6)], dates) %>%
        set_colnames(paste(pair, c("Open", "High", "Low", "Close", "Volume"), sep='.')) 
      if(tail(dates, 1) +  interval_num * 60 <  lubridate::with_tz(as.POSIXct(to), tz) && 
         tail(dates, 1) + interval_num * 60 < lubridate::with_tz(as.POSIXct(Sys.time()), tz)){
        startTime <- format(as.numeric(as.POSIXct(tail(dates, 1) +  interval_num * 60)) * 1000, scientific = FALSE)
      }else{
        break
      }
    }
    results[[pair]] <- do.call(rbind, res)
  }
  if(auto.assign){
    for(pair in Symbols){
      assign(pair, results[[pair]], envir=env)
    }
  }else{
    if(length(Symbols) == 1){
      return(results[[1]])
    }else{
      return(results)
    }
  }
}

