#' @export getSymbols.yahoo2
getSymbols.yahoo2 <- function (Symbols, env, return.class = "xts", index.class = "Date", 
                              add.actions = FALSE,
                              from = "2007-01-01", to = Sys.Date(), ..., periodicity = "daily", 
                              curl.options = list()) 
{
  importDefaults("getSymbols.yahoo2")
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!hasArg("adjust")) 
    adjust <- FALSE
  default.return.class <- return.class
  default.from <- from
  default.to <- to
  intervals <- c(daily = "1d", weekly = "1wk", monthly = "1mo")
  default.periodicity <- match.arg(periodicity, names(intervals))
  if (!hasArg("verbose")) 
    verbose <- FALSE
  if (!hasArg("auto.assign")) 
    auto.assign <- TRUE
  handle <- quantmod:::.getHandle(curl.options)
  returnSym <- Symbols
  noDataSym <- NULL
  for (i in seq_along(Symbols)) {
    test <- try({
      return.class <- quantmod:::getSymbolLookup()[[Symbols[[i]]]]$return.class
      return.class <- ifelse(is.null(return.class), default.return.class, 
                             return.class)
      periodicity <- quantmod:::getSymbolLookup()[[Symbols[[i]]]]$periodicity
      periodicity <- if (is.null(periodicity)) 
        default.periodicity
      else periodicity
      p <- pmatch(periodicity, names(intervals))
      if (is.na(p)) 
        stop("periodicity must be one of: ", paste(intervals, 
                                                   collapse = ", "))
      interval <- intervals[p]
      from <- quantmod:::getSymbolLookup()[[Symbols[[i]]]]$from
      from <- if (is.null(from)) 
        default.from
      else from
      to <- quantmod:::getSymbolLookup()[[Symbols[[i]]]]$to
      to <- if (is.null(to)) 
        default.to
      else to
      from.posix <- quantmod:::.dateToUNIX(from)
      to.posix <- quantmod:::.dateToUNIX(to)
      Symbols.name <- quantmod:::getSymbolLookup()[[Symbols[[i]]]]$name
      Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
                             Symbols.name)
      if (verbose) 
        cat("downloading ", Symbols.name, ".....\n\n")
      yahoo.URL <- quantmod:::.yahooURL(Symbols.name, from.posix, 
                             to.posix, interval, "history", handle)
      fr <- try(read.csv(curl::curl(yahoo.URL, handle = handle$ch), 
                         na.strings = "null"), silent = TRUE)
      if (inherits(fr, "try-error")) {
        warning(Symbols.name, " download failed; trying again.", 
                call. = FALSE, immediate. = TRUE)
        handle <- quantmod:::.getHandle(curl.options, force.new = TRUE)
        yahoo.URL <- quantmod:::.yahooURL(Symbols.name, from.posix, 
                               to.posix, interval, "history", handle)
        fr <- try(read.csv(curl::curl(yahoo.URL, handle = handle$ch), 
                           na.strings = "null"), silent = TRUE)
        if (inherits(fr, "try-error")) {
          stop(Symbols.name, " download failed after two attempts. Error", 
               " message:\n", attr(fr, "condition")$message, 
               call. = FALSE)
        }
      }
      if (verbose) 
        cat("done.\n")
      fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1]), 
                src = "yahoo", updated = Sys.time())
      if (any(is.na(fr))) {
        warning(Symbols.name, " contains missing values. Some functions will", 
                " not work if objects contain missing values in the middle", 
                " of the series. Consider using na.omit(), na.approx(),", 
                " na.fill(), etc to remove or replace them.", 
                call. = FALSE)
      }
      cnames <- c("Open", "High", "Low", "Close", "Volume", 
                  "Adjusted")
      corder <- pmatch(substr(cnames, 1, 3), colnames(fr))
      fr <- fr[, corder]
      colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)), 
                            cnames, sep = ".")
      if (adjust) {
        fr <- quantmod:::adjustOHLC(fr, symbol.name = Symbols.name)
      }
      fr <- quantmod:::convert.time.series(fr = fr, return.class = return.class)
      if (is.xts(fr)){
        tclass(fr) <- index.class
        splits <- suppressWarnings( getSplits(Symbols.name, src = 'yahoo'))
        if(length(splits) > 0){
          ratio <- adjRatios(splits = merge(splits, index(fr)))[, 1]
          if(has.Vo(fr)){
            ind <- has.Vo(fr, which=TRUE)
            fr[,ind] <- fr[,ind] / ratio
          }
          if(has.Op(fr)){
            ind <- has.Op(fr, which=TRUE)
            fr[,ind] <- fr[,ind] * ratio
          }
          if(has.Cl(fr)){
            ind <- has.Cl(fr, which=TRUE)
            fr[,ind] <- fr[,ind] * ratio
          }
          if(has.Hi(fr)){
            ind <- has.Hi(fr, which=TRUE)
            fr[,ind] <- fr[,ind] * ratio
          }
          if(has.Lo(fr)){
            ind <- has.Lo(fr, which=TRUE)
            fr[,ind] <- fr[,ind] * ratio
          }
        }
        
        
        if(add.actions){
          divs <- suppressWarnings( getDividends(Symbols.name, src = 'yahoo', split.adjust=FALSE))
          
          if(length(divs) == 0){
            divs <- DEFAULT_DIV
          }else{
            divs <- divs[paste0(index(fr)[1], '/', tail(index(fr), 1))]
            if(length(divs) == 0){
              divs <- DEFAULT_DIV
            }
          }
          fr <- cbind(fr, divs)
          fr[,ncol(fr)] <- na.fill(fr[,ncol(fr)], DEFAULT_DIV)
          colnames(fr)[ncol(fr)] <- paste(make.names(Symbols.name), "Dividend", sep = '.')
          if(length(splits) == 0){
            splits <- DEFAULT_SPLIT
          }else{
            splits <- splits[paste0(index(fr)[1], '/', tail(index(fr), 1))]
            if(length(splits) == 0){
              splits <- DEFAULT_SPLIT
            }
          }
          fr <- cbind(fr, splits)
          fr[,ncol(fr)] <- na.fill(fr[,ncol(fr)], DEFAULT_SPLIT)
          colnames(fr)[ncol(fr)] <- paste(make.names(Symbols.name), "Split", sep = '.')
        }
      } 
        
      Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
      if (auto.assign) 
        assign(Symbols[[i]], fr, env)
      if (i >= 5 && length(Symbols) > 5) {
        message("pausing 1 second between requests for more than 5 symbols")
        Sys.sleep(1)
      }
    }, silent = TRUE)
    if (inherits(test, "try-error")) {
      msg <- paste0("Unable to import ", dQuote(returnSym[[i]]), 
                    ".\n", attr(test, "condition")$message)
      if (hasArg(".has1sym.") && match.call(expand.dots = TRUE)$.has1sym.) {
        stop(msg)
      }
      warning(msg, call. = FALSE, immediate. = TRUE)
      noDataSym <- c(noDataSym, returnSym[[i]])
    }
  }
  if (auto.assign) 
    return(setdiff(returnSym, noDataSym))
  return(fr)
}