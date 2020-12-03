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

  dates <- l %>% Reduce('cbind', .) %>%
    g %>%
    index

  l <- lapply(l, function(x){
    merge(x, dates) %>% g %>% .[dates]
  })

  l <- lapply(l, function(x){
    if(!is.null(data)){
      to.period2(x[dates], period = data$._period, k = data$._freq)
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

  if('Di' %in% columns){
    dividends <- lapply(l, function(x){
      tryCatch({
        Di(x)
      }, error = function(e){
        DEFAULT_DIV
      })
    }) %>% f %>% na.fill(DEFAULT_DIV)
  }else{
    dividends <- NULL
  }

  if('Sp' %in% columns){
    splits <- lapply(l, function(x){
      tryCatch({
        Sp(x)
      }, error = function(e){
        DEFAULT_SPLIT
      })
    }) %>% f %>% na.fill(DEFAULT_SPLIT)
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
  return(data)
}

#' Create Data from xts
#'
#' @param x xts
#'
#' @return Data
#' @export
data_from_xts <- function(x, ...){
  data_from_list_xts(lapply(seq_len(ncol(x)), function(i){
    y <- x[,i]
    if(!(has.Cl(y) || has.Ad(y))){
      colnames(y) <- paste0(colnames(y), '.Adjusted')
    }
  }) %>% set_names(colnames(x)) , columns = c('Ad', 'Cl'), candles = FALSE, names_from_list = TRUE)
}
