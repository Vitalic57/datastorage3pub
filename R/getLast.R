getLast <- function (Symbols, src = "yahoo", what, ...) {
  Symbols <- unique(unlist(strsplit(Symbols, ";")))
  args <- list(Symbols = Symbols, ...)
  if (!missing(what))
    args$what <- what
  df <- do.call(paste("getLast", src, sep = "."), args)
  # if (nrow(df) != length(Symbols)) {
  #   allSymbols <- data.frame(Symbol = Symbols, stringsAsFactors = FALSE)
  #   df <- merge(allSymbols, df, by = "Symbol", all.x = TRUE)
  # }
  # rownames(df) <- df$Symbol
  # df$Symbol <- NULL
  # df[Symbols, ]
}
