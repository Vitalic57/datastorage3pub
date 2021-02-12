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
  cl$to <- Sys.time()
  for(name in ls_instruments(this)){
    inst <- getInstrument(this, name)
    instrument_attr(this, name, "src", inst$update_src)
  }
  getSymbols(cl)
  for(group in c('mat', 'nontraded', 'exchange_rates')){
    for(name in names(this[[group]])){
      this[[group]][[name]] <- rbind(this[[group]][[name]], cl[[group]][[name]])
    }
  }
  this$dates <- c(this$dates, cl$dates)
  return(invisible(this))
}
