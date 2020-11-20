#' Get data for Data object
#'
#' @param this Data
#' @param version character, heavy or light
#' @param ... arguments for functions getSymbols_heavy and getSymbols_light
#'
#' @return Data
#' @export
getSymbols.Data <- function(this, version = 'light',...){
  do.call(paste0('getSymbols_', version), args=c(list(this), list(...)))
}


download_instrument <- function(this, name, from, to, ...){
  info <- getInstrument(this, name)
  ids <- c(info$download, info$identifiers %>% unlist, info$primary_id)
  if('period' %in% names(info)){
    period <- info$period
  }else{
    period <- getBuildPeriod(this)
  }
  if('time' %in% names(info)){
    time <- info$time
  }else{
    time <- NULL
  }
  dots <- list(...)
  # download symbols
  for(id in ids){
    tryCatch({
      args <- list(Symbols = id,
                   from = from,
                   to = to,
                   period = period,
                   time = time,
                   src = info$src,
                   auto.assign = FALSE
      )
      args <- c(args, dots)
      if('args_getSymbols' %in% names(info)){
        args <- c(args, info$args_getSymbols)
        args[duplicated(names(args), fromLast = TRUE)] <- NULL
      }
      x <- do.call('getSymbols', args = args)
      break
    }, error = function(e){
      print(e)
      })
  }
  return(x)
}

is_without_args <- function(f){
  if(is.function(f)){
    return(is.null(formals(f)))
  }else if(is.character(f)){
    return(is.null(formals(get(f))))
  }else{
    stop("f is not a character or a function")
  }
}

#' Get data for Data object
#'
#' @param this Data
#' @param version character, heavy or light
#' @param ... arguments for functions getSymbols_heavy and getSymbols_light
#'
#' @return Data
#' @export
getSymbols.Data <- function(this, version = 'new',...){
  dots <- lapply(names(this), function(name){
    if(substr(name, 1, 2) == '._'){
      NULL
    }else if(!class(this[[name]])[1] %in% c('list', 'xts', 'environment')){
      this[[name]]
    }else{
      NULL
    }
  }) %>% set_names(names(this)) %>% {.[sapply(., is.null)] <- NULL; .}
  for(name in c('to', 'from')){
    dots[[name]] <- eval(dots[[name]])
  }
  do.call(paste0('getSymbols_', version),
          args=c(list(this), list(...), dots))
}


#' Set order of instruments
#'
#' @param this Data
#' @param x character, vector of primary ids
#'
#' @export
setOrder.Data <- function(this, x){
  this$ord <- x
  return(this)
}


#' Get order of instruments
#'
#' @param this Data
#'
#' @export
getOrder.Data <- function(this, x){
  return(this$ord)
}

