#' Exchange table in different currencies
#'
#' @param this Data
#' @param table matrix with names from this object
#' @param inds numeric indexes
#'
#' @return matrix
#' @export
exchange_to_counter <- function(this, table, inds){
  if(length(inds) != nrow(table)){
    stop('Length of inds and nrow of table do not coinside')
  }
  nms <- colnames(table)
  for(i in seq_len(ncol(table))){
    inst <- getInstrument(this, nms[i])
    if(inst$currency == this$currency){
      next
    }
    if(paste0(inst$currency, this$currency) %in% names(this$exchange_rates)){
      table[,i] <- table[,i] * this$exchange_rates[[paste0(inst$currency, this$currency)]][inds,,drop=FALSE]
    }else{
      table[,i] <- table[,i] / this$exchange_rates[[paste0(this$currency, inst$currency)]][inds,,drop=FALSE]
    }
  }
  return(table)
}
