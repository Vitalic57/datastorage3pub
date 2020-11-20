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
  for(block in c('series', 'mat')){
    for(name in names(data[[block]])){
      res[[name]] <- data[[block]][[name]][start_ind:end_ind,]
    }
  }
  for(name in names(data)){
    if(name %in% c('series', 'mat')){
      next
    }
    res[[name]] <- data[[name]]
  }

  return(invisible(res))
}
