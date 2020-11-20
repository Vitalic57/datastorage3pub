#' @export
date_to_seconds <- function(date, start = FALSE){
  date <- as.character(date)
  if(nchar(date) < 11){
    if(start){
      date <- strptime(paste0(date,' 00:00:01'), '%Y-%m-%d %H:%M:%S')
    }else{
      date <- min(strptime(paste0(date,' 23:59:59'), '%Y-%m-%d %H:%M:%S'), Sys.time())
    }

  }
  as.integer(lubridate::time_length(lubridate::interval(lubridate::ymd_hms("1970-01-01 00:00:00", tz = Sys.timezone()), date), "second"))
}

#' @export
seconds_to_date <- function(sec){
  lubridate::ymd_hms("1970-01-01 00:00:00", tz = Sys.timezone()) + sec
}


#' Download RUBUSD from moex
#'
#' @param Symbols here can be any character
#' @param from start date
#' @param to end date
#' @param auto.assign the same as in quantmod package
#' @param ... additional params
#'
#' @return nothing or xts in according to auto.assign
#' @export getSymbols.USDFIX
getSymbols.USDFIX <- function(Symbols = "usdfix",
                              from = Sys.Date() - 100,
                              to = Sys.Date(),
                              auto.assign = FALSE,
                              ...){
  getUSDFIX(from = from, to = to, auto.assign = auto.assign, index_ft = "POSIXct")
}









