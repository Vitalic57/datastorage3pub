#' @export getSymbols.Kraken
getSymbols.Kraken <- function(Symbols, period='day', from=Sys.Date() - 30, to=Sys.Date(), env, auto.assign=TRUE, tz=Sys.timezone(), ...){
  results <- list()
  since <- (as.Date(from) - as.Date("1970-01-01")) * 60 * 60 * 24
  
  interval <- NULL
  if(is.numeric(period)){
    periods <- rev(c(1, 5, 15, 30, 60, 240, 1440, 10080, 21600))
    for(p in periods){
      if(period >= p){
        interval <- p
        break
      }
    }
  }else{
    interval<- switch(tolower(period),
                   'day'=,
                   "daily"=,
                   "d"=,
                   "1d"=1440,
                   "week"=,
                   "weekly"=,
                   "1w"=10080,
                   "4h"=240,
                   "h"=,
                   "hour"=60,
                   "30m"=,
                   "30min"=30,
                   "15m"=,
                   "15min"=15,
                   "5m"=,
                   "5min"=5,
                   "1m"=,
                   "1min"=1,
                   stop('no such period')
                   )
  }
  for(pair in Symbols){
    base_url <- "https://api.kraken.com/0/public/OHLC"
    url <- paste0(base_url, "?", "pair=", pair, "&since=", since, 
                  "&interval=", interval)
    res <- jsonlite::fromJSON(url)$result
    ohlc_out <- res[[1]]
    class(ohlc_out) <- 'numeric'
    dates <- lubridate::with_tz(as.POSIXct(ohlc_out[,1], origin='1970-01-01 00:00:00', tz='UTC'), tz)
    results[[pair]] <- xts(ohlc_out[,c(2:5, 7)], dates) %>%
      set_colnames(paste(pair, c("Open", "High", "Low", "Close", "Volume"), sep='.')) %>%
      .[paste0("/", to)]
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

