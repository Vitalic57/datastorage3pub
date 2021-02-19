#' Update all tables to Sys.time()
#'
#' @param this Data
#'
#' @return
#' @export
#' @rdname updateSymbols
#' @method updateSymbols Data
updateSymbols.Data <- function(this){
  cl <- cloneData(this)
  cl$from <- this$dates %>% tail(1) + 1
  # if()
  cl$to <- Sys.time()
  for(name in ls_instruments(this)){
    inst <- getInstrument(this, name)
    instrument_attr(this, name, "src", inst$update_src)
    instrument_attr(this, name, "download", inst$update_name)
  }
  getSymbols(cl)
  for(group in c('mat', 'nontraded', 'exchange_rates')){
    for(name in names(this[[group]])){
      this[[group]][[name]] <- rbind(this[[group]][[name]], cl[[group]][[name]])
    }
  }
  this$dates <- c(this$dates, cl$dates)
  this$nrow <- this$nrow + cl$nrow
  return(invisible(this))
}


#' Update the last point
#'
#' @param this Data
#'
#' @return
#' @export
#' @rdname updateLast
#' @method updateLast Data
updateLast.Data <- function(this){
  cl <- cloneData(this)
  for(name in ls_instruments(this)){
    inst <- getInstrument(this, name)
    instrument_attr(this, name, "src", inst$online_src)
    instrument_attr(this, name, "download", inst$online_name)
  }
  getLast(cl)
  update_last <- FALSE
  if(this$._period == 'day' && as.Date(tail(this$dates, 1)) == Sys.Date()){
    update_last <- TRUE
  }
  # browser()
  for(group in c('mat', 'nontraded', 'exchange_rates')){
    for(name in names(this[[group]])){
      if(update_last){
        this[[group]][[name]][this$nrow, ] <- cl[[group]][[name]]
      }else{
        this[[group]][[name]] <- rbind(this[[group]][[name]], cl[[group]][[name]])
      }
    }
  }
  if(update_last){

  }else{
    this$dates <- c(this$dates, cl$dates)
    this$nrow <- this$nrow + cl$nrow
  }
  return(invisible(this))
}
