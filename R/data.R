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
  # browser()
  f <- . %>% Reduce('cbind', .)
  if(names_from_list){
    f <- . %>% Reduce('cbind', .) %>% set_colnames(names(l))
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

  # dates <- l %>% lapply(function(x) x[,1]) %>% Reduce('cbind', .)  %>%
  #   g %>%
  #   index
  dates_class <- l[[1]] %>% index %>% class
  if(na_omit){
    dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(intersect, .) # %>% as.Date
  }else{
    dates <- lapply(l, . %>% index %>% as.character) %>% Reduce(union, .)
  }
  if(dates_class[1] == 'Date'){
    dates <- as.Date(dates)
  }
  if(dates_class[1] == 'POSIXct'){
    dates <- as.POSIXct(dates)
  }
  if(dates_class[1] == 'POSIXlt'){
    dates <- as.POSIXlt(dates)
  }

  l <- lapply(l, function(x){
    merge(x, dates) %>% g %>% .[dates]
  })
  if(!is.null(data)){
    l <- lapply(l, function(x){
      to.period3(x[dates], period = data$._period)
    })
  }

  if(is.null(data)){
    data <- Data()
    data$columns <- columns
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

  data$mat <- list()

  data$mat$adjusted <- adjusted
  data$mat$close <- close
  data$mat$dividends <- dividends
  data$mat$splits <- splits
  data$mat$open <- open
  data$mat$high <- high
  data$mat$low <- low

  data$dates <- dates
  data$nrow <- nrow.Data(data)
  data$ncol <- ncol.Data(data)
  data$colnames <- colnames.Data(data)
  if('close' %in% names(data$mat)){
    data$price_table <- which(names(data$mat) == 'close')
  }else if('adjusted' %in% names(data$mat)){
    data$price_table <- which(names(data$mat) == 'adjusted')
  }else{
    warning('Price table is not specified')
  }
  data %>% stock(data$colnames)
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
  data_from_list_xts(lapply(seq_len(ncol(x)), function(i){
    y <- x[,i]
    if(!has.Ad(y)){
      colnames(y) <- paste0(nms[i], '.Adjusted')
    }
    y
  }) %>% set_names(nms) , columns = 'Ad', candles = FALSE, names_from_list = TRUE)
}
