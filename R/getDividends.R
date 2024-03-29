#' @export getDividends.yahoo
getDividends.yahoo <- function (Symbol, from = "1970-01-01", to = Sys.Date(), env = parent.frame(),
          src = "yahoo", auto.assign = FALSE, auto.update = FALSE,
          verbose = FALSE, split.adjust = TRUE, ...) {
  if (missing(env))
    env <- parent.frame(1)
  if (is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol), deparse(substitute(Symbol)),
                        as.character(Symbol))
  from.posix <- quantmod:::.dateToUNIX(from)
  to.posix <- quantmod:::.dateToUNIX(to)
  handle <- quantmod:::.getHandle()
  yahoo.URL <- quantmod:::.yahooURL(Symbol.name, from.posix, to.posix,
                         "1d", "div", handle)
  con <- curl::curl(yahoo.URL, handle = handle$ch)
  fr <- read.csv(con)
  try(close(con), silent = TRUE)
  fr <- xts(fr[, 2], as.Date(fr[, 1]))
  colnames(fr) <- paste(Symbol.name, "div", sep = ".")
  if (src[1] == "yahoo" && !split.adjust) {
    splits <- quantmod:::getSplits(Symbol.name, from = "1900-01-01")
    if (is.xts(splits) && is.xts(fr) && nrow(splits) > 0 &&
        nrow(fr) > 0) {
      fr <- fr * adjRatios(splits = merge(1 / splits, index(fr)))[, 1]
    }
  }
  if (is.xts(Symbol)) {
    if (auto.update) {
      xtsAttributes(Symbol) <- list(dividends = fr)
      assign(Symbol.name, Symbol, envir = env)
    }
  }
  else if (auto.assign) {
    assign(paste(Symbol.name, "Dividend", sep = "."), fr, envir = env)
  }
  else fr
}





