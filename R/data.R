#' Create Data from list of xts
#'
#' @param x xts
#'
#' @return Data
#' @export
data_from_list_xts <- function(l,
                               names_from_list = TRUE,
                               candles = FALSE,
                               data = NULL,
                               na_omit = TRUE,
                               na_locf = TRUE,
                               columns = c('Cl', 'Ad', 'Vo', 'Di', 'Sp')){
  l <- lapply(l, na.omit)
  f <- . %>% Reduce('cbind', .) %>% set_rownames(NULL)
  if(names_from_list){
    f <- . %>% Reduce('cbind', .) %>% set_colnames(names(l)) %>% set_rownames(NULL)
  }

  g <- . %>%
    {
      if(na_locf){
        na.locf(.)
      }else{
        .
      }
    } %>%
    {
      if(na_omit){
        na.omit(.)
      }else if(na_locf){
        na.locf(., fromLast = TRUE)
      }else{
        .
      }
    }

  dates_class <- l[[1]] %>% index %>% class
  tz <- tzone(l[[1]])
  if(dates_class[1] %in% c('POSIXct', 'POSIXlt')){
    if(length(l) > 1){
      for(i in 2:length(l)){
        if(tzone(l[[i]]) == tz){
          next
        }
        tzone(l[[i]]) <- tz
      }
    }
  }

  if(na_omit){
    dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(intersect, .)
  }else{
    dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(union, .)
  }
  if(dates_class[1] == 'Date'){
    dates <- as.Date(dates)
  }
  if(dates_class[1] == 'POSIXct'){
    dates <- as.POSIXct(dates, tz = tz)
  }
  if(dates_class[1] == 'POSIXlt'){
    dates <- as.POSIXlt(dates, tz = tz)
  }

  l <- lapply(l, function(x){
    merge(x, dates) %>% g %>% .[dates]
  })
  if(!is.null(data)){
    suppressWarnings({
      l <- lapply(l, function(x){
        to.period3(x[dates], period = data$._period)
      })
    })

    if(na_omit){
      dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(intersect, .)
    }else{
      dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(union, .)
    }
    if(dates_class[1] == 'Date'){
      dates <- as.Date(dates)
    }
    if(dates_class[1] == 'POSIXct'){
      dates <- as.POSIXct(dates, tz = tz)
    }
    if(dates_class[1] == 'POSIXlt'){
      dates <- as.POSIXlt(dates, tz = tz)
    }
    if(!all(sapply(l, nrow) == nrow(l[[1]]))){
      l <- lapply(l, function(x) merge(x, dates))
    }
  }
  data_was_null <- FALSE
  if(is.null(data)){
    data <- Data()
    data$columns <- columns
    data_was_null <- TRUE
  }
  columns <- data$columns
  if(candles){
    data$candles <- l
  }
  n <- length(dates)
  l <- lapply(l, coredata)
  if('Ad' %in% columns){
    adjusted <- lapply(l, function(x){
      tryCatch({
        Ad(x) %>% cbind
      }, error = function(e){
        tryCatch({
          Cl(x) %>% cbind
        }, error = function(e){
          rep(NA, n) %>% cbind
        })
      })
    }) %>% f
  }else{
    adjusted <- NULL
  }

  if('Cl' %in% columns){
    close <- lapply(l, function(x){
      tryCatch({
        Cl(x) %>% cbind
      }, error = function(e){
        rep(NA, n) %>% cbind
      })
    }) %>% f
  }else{
    close <- NULL
  }

  if('Op' %in% columns){
    open <- lapply(l, function(x){
      tryCatch({
        Op(x) %>% cbind
      }, error = function(e){
        rep(NA, n) %>% cbind
      })
    }) %>% f
  }else{
    open <- NULL
  }

  if('Hi' %in% columns){
    high <- lapply(l, function(x){
      tryCatch({
        Hi(x) %>% cbind
      }, error = function(e){
        rep(NA, n) %>% cbind
      })
    }) %>% f
  }else{
    high <- NULL
  }

  if('Lo' %in% columns){
    low <- lapply(l, function(x){
      tryCatch({
        Lo(x) %>% cbind
      }, error = function(e){
        rep(NA, n) %>% cbind
      })
    }) %>% f
  }else{
    low <- NULL
  }
  # browser()
  if('Di' %in% columns){
    in_div <- FALSE
    dividends <- lapply(l, function(x){
      tryCatch({
        x <- Di(x) %>% cbind
        in_div <<- TRUE
        x
      }, error = function(e){
        rep(DEFAULT_DIV, n) %>% cbind

      })
    })
    if(in_div){
      dividends <- dividends %>% f %>% na.fill(DEFAULT_DIV)
    }else{
      dividends <- NULL
    }

  }else{
    dividends <- NULL
  }

  if('Sp' %in% columns){
    in_split <- FALSE
    splits <- lapply(l, function(x){
      tryCatch({
        x <- Sp(x) %>% cbind
        in_split <<- TRUE
        x
      }, error = function(e){
        rep(DEFAULT_SPLIT, n) %>% cbind

      })
    })
    if(in_split){
      splits <- splits %>% f %>% na.fill(DEFAULT_SPLIT)
    }else{
      splits <- NULL
    }
  }else{
    splits <- NULL
  }

  if('Vo' %in% columns){
    volume <- lapply(l, function(x){
      tryCatch({
        Vo(x) %>% cbind
      }, error = function(e){
        rep(DEFAULT_VOLUME, n) %>% cbind
      })
    }) %>% f
  }else{
    volume <- NULL
  }


  data$adjusted <- adjusted
  data$close <- close
  data$dividends <- dividends
  data$splits <- splits
  data$open <- open
  data$high <- high
  data$low <- low
  data$volume <- volume

  data$dates <- dates

  if(!is.null(data$close)){
    data$price_table <- 'close'
  }else if(!is.null(data$adjusted)){
    data$price_table <- 'adjusted'
  }else{
    warning('Price table is not specified')
  }
  data$nrow <- nrow.Data(data)
  data$ncol <- ncol.Data(data)
  data$colnames <- colnames.Data(data)
  if(data_was_null){
    data %>% stock(data$colnames)
  }
  return(data)
}

#' Create Data from xts
#'
#' @param x xts
#'
#' @return Data
#' @export
data_from_xts <- function(x, ...){
  nms <- colnames(x)
  if(is.null(nms)){
    nms <- paste0('x', 1:ncol(x))
  }
  l <- lapply(seq_len(ncol(x)), function(i){
    y <- x[,i]
    if(!has.Ad(y)){
      colnames(y) <- paste0(nms[i], '.Adjusted')
    }
    y
  })  %>% set_names(nms)
  data_from_list_xts(l, columns = 'Ad', candles = FALSE, names_from_list = TRUE)
}
