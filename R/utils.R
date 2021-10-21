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
