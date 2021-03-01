period2xts <- function(period){
  period <- switch(period,
                   min =,
                   minute = ,
                   minutes = ,
                   mins = {
                     'minutes'
                   },
                   hour =,
                   hours ={
                     'hours'
                   },
                   days = ,
                   day ={
                     'days'
                   },
                   week =,
                   weeks = {
                     'weeks'
                   },
                   month =,
                   months = {
                     'months'
                   },
                   default = {
                     NULL
                   }
  )
  return(period)
}

#' @export
to.period2 <- function(x, period, k = 1){
  # browser()
  name <- NULL
  tryCatch({
    name <- colnames(x)[1] %>% strsplit('\\.') %>% .[[1]] %>% head(-1) %>% paste(collapse = '.')
  }, error = function(e){
  })

  period <- period2xts(period)
  res <- to.period(x, period = period, k = k, name = name)
  res <- res[, intersect(colnames(x), colnames(res))]
  if(has.Sp(x) || has.Di(x)){
    ep <- endpoints(x, on = period, k = k)
  }
  if(has.Di(x)){
    res <- cbind(res, period.sum(Di(x), INDEX = ep))
  }
  if(has.Sp(x)){
    res <- cbind(res, period.prod(Sp(x), INDEX = ep))
  }
  return(res %>% set_colnames(colnames(x)))
}

#' @export
to.period3 <- function(x, period){
  if(is.numeric(period)){
    return(to.period2(x, 'minutes', period))
  }
  new_period <- period2xts(period)
  if(!is.null(new_period)){
    return(to.period2(x, new_period, 1))
  }
  if(period == '1min'){
    return(to.period2(x, 'minutes', 1))
  }else if(period == '5min'){
    return(to.period2(x, 'minutes', 5))
  }else if(period == '10min'){
    return(to.period2(x, 'minutes', 10))
  }else if(period == '15min'){
    return(to.period2(x, 'minutes', 15))
  }else if(period == '15min'){
    return(to.period2(x, 'minutes', 15))
  }else if(period == '30min'){
    return(to.period2(x, 'minutes', 30))
  }
  stop(paste0('No such period ', period))
  # if(period)
}












