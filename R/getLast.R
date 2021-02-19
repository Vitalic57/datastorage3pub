getLast.default <- function (Symbols, src = "yahoo", what, ...) {
  args <- list(Symbols = Symbols, ...)
  if (!missing(what))
    args$what <- what
  df <- do.call(paste("getLast", src, sep = "."), args)
  return(df)
  # if (nrow(df) != length(Symbols)) {
  #   allSymbols <- data.frame(Symbol = Symbols, stringsAsFactors = FALSE)
  #   df <- merge(allSymbols, df, by = "Symbol", all.x = TRUE)
  # }
  # rownames(df) <- df$Symbol
  # df$Symbol <- NULL
  # df[Symbols, ]
}


getLast.yahoo <- function(Symbols, add.actions=FALSE, ...){
  df <- getQuote(Symbols, what = yahooQF('Last Trade (Price Only)'))
  res <- list()
  for(i in seq_len(nrow(df))){
    res[[rownames(df)[i]]] <- xts(df[i, 'Last'], Sys.time()) %>% set_colnames(paste(rownames(df)[i], 'Close', sep = '.'))
  }
  if(add.actions){
    for(Symbol in rownames(df)){
      div <- DEFAULT_DIV
      tryCatch({
        suppressWarnings({
          divs <- getDividends(Symbol, src = 'yahoo')
          if(tail(index(divs), 1) == Sys.Date()){
            div <- tail(divs, 1)[[1]]
          }
        })
      }, error = function(e){})
      spl <- DEFAULT_SPLIT
      tryCatch({
        suppressWarnings({
          splits <- getSplits(Symbol, src = 'yahoo')
          if(tail(index(splits), 1) == Sys.Date()){
            spl <- tail(splits, 1)[[1]]
          }
        })
      }, error = function(e){})

      res[[Symbol]] <- coredata(res[[Symbol]]) %>%
        cbind(div, spl) %>%
        set_colnames(paste(make.names(rownames(df)[i]), c('Close', 'Dividend', 'Split'), sep = '.')) %>%
        xts(Sys.time())
    }
  }
  if(length(res) == 1){
    return(res[[1]])
  }
  return(res)
}


getLast.Data <- function(this, ...){
  getSymbols(this, download_fun=download_instrument_last, ...)
}
