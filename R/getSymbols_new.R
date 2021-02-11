#' Get data
#'
#' This function returns lightweighted object.
#'
#' @param this Data
#'
#' @return
#' @export
#'
#' @examples
getSymbols_new <- function(this, prices=TRUE, tables=TRUE, ...){
  dots <- list(...)
  if(prices){
    instruments <- sapply(c(ls_stocks, ls_indexes), function(x) this %>% x) %>% unlist
    trade_inst <- list()
    nontraded <- list()

    # download and sort symbols ---------------------------------------------------------------------------------
    for(instr in instruments){
      info <- getInstrument(this, instr)

      if(!is.null(info[['ex_rate']])){
        if(!info[['ex_rate']] %in% names(this$exchange_rates)){
          this$exchange_rates[[info[['ex_rate']]]] <- download_instrument(this, info[['ex_rate']], ...) %>% Cl
        }
      }
      x <- download_instrument(this, instr, add.actions = TRUE, ...)
      if(!is.null(info[['ex_rate']])){
        y <- x
        if(has.Vo(x)){
          y <- y[,-has.Vo(x, which=TRUE)]
        }
        if(has.Sp(x)){
          y <- y[,-has.Sp(x, which=TRUE)]
        }
        rate <- this$exchange_rates[[info[['ex_rate']]]]
        rate <- cbind(rate, y) %>% .[,1] %>% na.locf  %>% {.[index(y)]}
        if(getInstrument(this, info[['ex_rate']])[['counter_currency']] == info[['currency']]){
          y <- y * drop(rate)
        }else{
          y <- y / drop(rate)
        }

        if(has.Vo(x)){
          y <- cbind(y, Vo(x))
        }
        if(has.Sp(x)){
          y <- cbind(y, Sp(x))
        }
        x <- y
      }

      inds <- c(has.Op(x, which = TRUE),
                has.Hi(x, which = TRUE),
                has.Lo(x, which = TRUE),
                has.Cl(x, which = TRUE),
                has.Ad(x, which = TRUE),
                has.Di(x, which = TRUE))
      inds <- setdiff(inds, 0)
      x[,inds] <- x[,inds] * info$multiplier
      if(info[['trade']]){
        trade_inst[[info$primary_id]] <- x
      }else{
        nontraded[[info$primary_id]] <- x
      }
    }

    instruments <- ls_exchange_rates(this)

    # download and sort symbols ---------------------------------------------------------------------------------
    for(instr in instruments){
      info <- getInstrument(this, instr)

      x <- download_instrument(this, instr, add.actions = FALSE, ...)

      inds <- c(has.Op(x, which = TRUE),
                has.Hi(x, which = TRUE),
                has.Lo(x, which = TRUE),
                has.Cl(x, which = TRUE))
      inds <- setdiff(inds, 0)
      x[,inds] <- x[,inds] * info$multiplier
      if(info[['trade']]){
        trade_inst[[info$primary_id]] <- x
      }else{
        nontraded[[info$primary_id]] <- x
      }
      this$exchange_rates[[paste0(info$counter_currency, info$currency)]] <- Cl(x)
    }
    ##############################################################################################################
    # creation of main tables
    if(length(trade_inst) > 0){
      if(length(getOrder(this)) == length(trade_inst)){
        trade_inst <- trade_inst[make.names(getOrder(this))]
      }
      if('candles' %in% names(dots)){
        candles <- dots[['candles']]
      }else{
        candles <- this$._candles
      }
      if('na_locf' %in% names(dots)){
        na_locf <- dots[['na_locf']]
      }else{
        na_locf <- this$na_locf
      }
      if('na_omit' %in% names(dots)){
        na_omit <- dots[['na_omit']]
      }else{
        na_omit <- this$na_omit
      }
      this <- data_from_list_xts(trade_inst,
                                 names_from_list = TRUE,
                                 candles = candles,
                                 na_omit = na_omit,
                                 na_locf = na_locf,
                                 data = this)
    }
    this$nontraded <- nontraded
    for(i in seq_along(this$exchange_rates)){
      this$exchange_rates[[i]] <- this$exchange_rates[[i]] %>%
        merge(this$dates) %>%
        na.locf %>%
        na.locf(fromLast=TRUE) %>%
        .[this$dates] %>%
        coredata
    }
  }


  if(tables){
    add_tables(this, ...)
  }
  return(this)
}




add_tables <- function(this, ...){
  for(tbl in this$tableord){
    # get info
    info <- this$tablesenv[[tbl]]

    # download exchange rates
    if(!is.null(info[['ex_rate']])){
      for(ex in unlist(info[['ex_rate']])){
        if(is.na(ex)){
          next
        }
        if(!ex %in% names(this$exchange_rates)){
          this$exchange_rates[[ex]] <- download_instrument(this, ex, ...) %>% Cl
        }
      }
    }

    # download table
    if(!is.null(info$download)){
      ids <- info$download
    }else{
      ids <- NULL
    }
    args <- info$args_download
    results <- list()
    if(info$one_by_one){
      if(is.character(info$fun_download) && info$fun_download == 'getSymbols'){
        args[['auto.assign']] <- FALSE
      }
      if(is.character(info$fun_download) || is.function(info$fun_download)){
        if(is_without_args(info$fun_download)){
          for(id in ids){
            results[[id]] <- do.call(info$fun_download)
          }
        }else{
          for(id in ids){
            results[[id]] <- do.call(info$fun_download, args=c(list(id), args))
          }
        }
      }else{
        results[[ids[1]]] <- info$fun_download
      }
    }else{
      if(is.character(info$fun_download) && info$fun_download == 'getSymbols'){
        args[['auto.assign']] <- NULL
        results <- do.call(info$fun_download, args=c(list(ids, auto.assign=TRUE, env=results), args))
      }else{
        if(is.character(info$fun_download) || is.function(info$fun_download)){
          if(is_without_args(info$fun_download)){
            results <- do.call(info$fun_download)
          }else{
            results <- do.call(info$fun_download, args=c(list(ids), args))
          }
        }else{
          results <- info$fun_download
        }
      }
    }

    # apply fun_component
    if(!is.null(info$fun_component)){
      results <- lapply(results, function(x) do.call(info$fun_component, args=list(x)))
    }

    fun_num <- 1
    current_cur <- info[['currency']]
    cur_num <- 1
    for(action in info[['action_seq']]){
      x <- switch(action,
                  "fun"={
                    if(!is.null(info$fun_table)){
                      if(class(results)[1] == 'list'){
                        funs <- info$fun_table[[fun_num]]
                        if(is.function(funs)){
                          funs <- list(funs)
                        }
                        results <- mapply(function(f, x) do.call(f, args=list(x)), funs, results)
                      }else{
                        results <- do.call(info$fun_table[[fun_num]], args=list(results))
                      }
                    }
                    fun_num <- fun_num + 1
                  },
                  "cbind"={
                    if(!is.null(info[['ex_rate']])){
                      if(length(unique(current_cur)) > 1){
                        stop('All components should be in one currency before cbind')
                      }else{
                        current_cur <- current_cur[1]
                      }
                    }
                    results <- results %>% Reduce('cbind', .)
                  },
                  "na.locf"={
                    if(class(results)[1] == 'list'){
                      results <- lapply(results, na.locf)
                    }else{
                      results <- na.locf(results)
                    }
                  },
                  "na.locfl"={
                    if(class(results)[1] == 'list'){
                      results <- lapply(results, na.locf, fromLast=TRUE)
                    }else{
                      results <- na.locf(results, fromLast=TRUE)
                    }
                  },
                  "price"={
                    if(class(results)[1] == 'list'){
                      if(class(results[[1]])[1] == 'xts' && info$price_align && 'close' %in% names(this$series)){
                        x <- this$series$close[,1]
                        results <- lapply(results, function(xx){
                          cbind(x, xx) %>% .[,-1] %>% na.locf %>% {.[index(x)]}
                        })
                      }
                    }else{
                      if(class(results)[1] == 'xts' && info$price_align && 'close' %in% names(this$series)){
                        x <- this$series$close[,1]
                        results <- cbind(x, results) %>% .[,-1] %>% na.locf %>% {.[index(x)]}
                      }
                    }
                  },
                  "ex_rate"={
                    if(class(results)[1] == 'list'){
                      if(!is.null(info[['ex_rate']]) && class(results[[1]])[1] == 'xts'){
                        if(length(current_cur) == 1){
                          current_cur <- rep(current_cur, length(results))
                        }
                        results <- lapply(seq_along(results), function(i){
                          y <- results[[i]]
                          if(has.Vo(results[[i]])){
                            y <- y[,-has.Vo(results[[i]], which=TRUE)]
                          }
                          if(length(info[['ex_rate']][[cur_num]]) == 1){
                            rate_name <- info[['ex_rate']][[cur_num]]
                          }else{
                            rate_name <- info[['ex_rate']][[cur_num]][i]
                          }
                          if(is.na(rate_name)){
                            return(results[[i]])
                          }
                          rate <- this$exchange_rates[[rate_name]]
                          rate <- cbind(rate, y) %>% .[,1] %>% na.locf  %>% {.[index(y)]}
                          if(getInstrument(this, rate_name)[['counter_currency']] == current_cur[i]){
                            y <- y * drop(rate)
                            current_cur[i] <<- getInstrument(this, rate_name)[['currency']]
                          }else{
                            y <- y / drop(rate)
                            current_cur[i] <<- getInstrument(this, rate_name)[['counter_currency']]
                          }
                          if(has.Vo(results[[i]])){
                            results[[i]] <<- cbind(y, Vo(results[[i]]))
                          }else{
                            results[[i]] <<- y
                          }
                        })
                      }
                    }else{
                      if(!is.null(info[['ex_rate']]) && class(results)[1] == 'xts'){
                        y <- results
                        if(has.Vo(results)){
                          y <- y[,-has.Vo(results, which=TRUE)]
                        }
                        if(length(info[['ex_rate']][[cur_num]]) > 1){
                          stop("Only one currency should be applied to cbinded table")
                        }
                        if(length(current_cur) > 1){
                          stop("Error with currencies length should be one")
                        }
                        rate <- this$exchange_rates[[info[['ex_rate']][[cur_num]]]]
                        rate <- cbind(rate, y) %>% .[,1] %>% na.locf  %>% {.[index(y)]}
                        if(getInstrument(this, info[['ex_rate']][[cur_num]])[['counter_currency']] == current_cur){
                          y <- y * drop(rate)
                          current_cur <- getInstrument(this, info[['ex_rate']][[cur_num]])[['currency']]
                        }else{
                          y <- y / drop(rate)
                          current_cur <- getInstrument(this, info[['ex_rate']][[cur_num]])[['counter_currency']]
                        }
                        if(has.Vo(results)){
                          results <- cbind(y, Vo(results))
                        }else{
                          results <- y
                        }
                      }
                    }
                    cur_num <- cur_num + 1
                  })
    }
    this[[tbl]] <- results
  }
}

