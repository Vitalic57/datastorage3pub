#' Shrink Data
#'
#' @param data Data
#' @param start_ind numeric
#' @param end_ind numeric
#' @param copy logical
#'
#' @return Data
#' @export
shrink_by_ind <- function(data, start_ind, end_ind, copy=TRUE){
  if(copy){
    res <- new.env()
    class(res) <- c("Data", "environment")
  }else{
    res <- data
  }
  for(block in c('indexes')){
    res[[block]] <- list()
    for(name in names(data[[block]])){
      res[[block]][[name]] <- data[[block]][[name]][start_ind:end_ind,,drop=FALSE]
    }
  }
  for(name in data$price_columns){
    res[[name]] <- data[[name]][start_ind:end_ind,,drop=FALSE]
  }
  for(name in names(data)){
    if(name %in% c( 'dates', 'indexes', data$price_columns)){
      next
    }
    res[[name]] <- data[[name]]
  }
  res[['dates']] <- data[['dates']][start_ind:end_ind]
  res[['nrow']] <- end_ind - start_ind + 1

  return(invisible(res))
}



#' Get subset of Data object
#' 
#' All tables will be subset
#'
#' @param data Data
#' @param rows numeric vector
#' @param columns character vector
#'
#' @return Data
#' @export
get_subset <- function(data, rows, columns){
  if(missing(rows)){
    rows <- 1:data[['nrow']]
  }
  if(missing(columns)){
    columns <- data[['colnames']]
  }
  if(is.numeric(columns) || is.logical(columns)){
    columns <- data$colnames[columns]
  }
  res <- new.env()
  class(res) <- c("Data", "environment")
  res[['envir']] <- data[['envir']]
  res[['multiple_currencies']] <- data[['multiple_currencies']]
  for(name in data[['price_columns']]){
    res[[name]] <- data[[name]][rows, columns, drop=FALSE]
  }
  res[['ex_rates']] <- data[['ex_rates']][rows, , drop=FALSE]
  
  for(block in c('indexes')){
    res[[block]] <- list()
    for(name in names(data[[block]])){
      res[[block]][[name]] <- data[[block]][[name]][rows, columns, drop=FALSE]
    }
  }
  # res[['exchange_rates']] <- list()
  # for(name in names(data[['exchange_rates']])){
  #   res[['exchange_rates']][[name]] <- data[['exchange_rates']][[name]][rows, , drop=FALSE]
  # }
  
  
  for(name in names(data)){
    if(name %in% c( 'dates', 'indexes', 'colnames', data[['price_columns']], 'ex_rates')){
      next
    }
    res[[name]] <- data[[name]]
  }
  res[['dates']] <- data[['dates']][rows]
  res[['colnames']] <- columns
  res[['ncol']] <- length(columns)
  if(is.logical(rows)){
    res[['nrow']] <- sum(rows)
  }else{
    res[['nrow']] <- length(rows)
  }
  
  return(invisible(res))
}
