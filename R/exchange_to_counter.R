#' Exchange table in different currencies
#'
#' @param this Data
#' @param table matrix with names from this object
#' @param inds numeric indexes
#'
#' @return matrix
#' @export
exchange_to_counter <- function(this, table, inds){
  if(is.vector(table)){
    table <- rbind(table)
  }
  if(length(inds) != nrow(table)){
    stop('Length of inds and nrow of table do not coinside')
  }
  if(is.null(colnames(table))){
    if(this[['ncol']] == ncol(table)){
      colnames(table) <- this[['colnames']]
    }else{
      stop('Table should have names')
    }
  }
  nms <- make.names(colnames(table))
  for(i in seq_len(ncol(table))){
    tryCatch({
      inst <- this[['envir']][[nms[i]]]
      if(inst[['currency']] == this[['currency']]){
        next
      }
      if(paste0(inst[['currency']], this[['currency']]) %in% names(this[['ex_rates']])){
        table[,i] <- table[,i] * this[['ex_rates']][inds,paste0(inst[['currency']], this[['currency']]),drop=FALSE]
      }else{
        table[,i] <- table[,i] / this[['ex_rates']][inds, paste0(this[['currency']], inst[['currency']]), drop=FALSE]
      }
    },
    warning = function(w){
    })
  }
  return(table)
}

a <- 0
tryCatch({
  a <- -1
  warning('asd')
  a <- 1
},
warning = function(w){

})
