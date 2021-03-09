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

  l <- lapply(l, function(x){
    if(!is.null(data)){
      to.period3(x[dates], period = data$._period)
    }else{
      x[dates]
    }
  })
  if(is.null(data)){
    data <- Data()
    data$columns <- columns
  }
  columns <- data$columns
  if(candles){
    data$candles <- l
  }
  if('Ad' %in% columns){
    adjusted <- lapply(l, function(x){
      tryCatch({
        Ad(x)
      }, error = function(e){
        tryCatch({
          Cl(x)
        }, error = function(e){
          NA
        })
      })
    }) %>% f
  }else{
    adjusted <- NULL
  }

  if('Cl' %in% columns){
    close <- lapply(l, function(x){
      tryCatch({
        Cl(x)
      }, error = function(e){
        NA
      })
    }) %>% f
  }else{
    close <- NULL
  }

  if('Op' %in% columns){
    open <- lapply(l, function(x){
      tryCatch({
        Op(x)
      }, error = function(e){
        NA
      })
    }) %>% f
  }else{
    open <- NULL
  }

  if('Hi' %in% columns){
    high <- lapply(l, function(x){
      tryCatch({
        Hi(x)
      }, error = function(e){
        NA
      })
    }) %>% f
  }else{
    high <- NULL
  }

  if('Lo' %in% columns){
    low <- lapply(l, function(x){
      tryCatch({
        Lo(x)
      }, error = function(e){
        NA
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
        x <- Di(x)
        in_div <<- TRUE
        x
      }, error = function(e){
        DEFAULT_DIV
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
        x <- Sp(x)
        in_split <<- TRUE
        x
      }, error = function(e){
        DEFAULT_SPLIT
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
        Vo(x)
      }, error = function(e){
        DEFAULT_VOLUME
      })
    }) %>% f
  }else{
    volume <- NULL
  }


  data$series <- list()
  data$mat <- list()


  data$series$adjusted <- adjusted
  data$series$close <- close
  data$series$dividends <- dividends
  data$series$splits <- splits
  data$series$open <- open
  data$series$high <- high
  data$series$low <- low

  for(name in names(data$series)){
    data$mat[[name]] <- coredata(data$series[[name]])
  }
  data$series <- NULL
  data$dates <- index(l[[1]])
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
