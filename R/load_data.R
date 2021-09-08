get_ids <- function(name){
  names(finam.stock.list) %>% {which(tolower(name) == tolower(.))} %>% {(finam.stock.list)[.]} %>% as.character %>% as.numeric %>% unique
}

loadStockList <- function (verbose = FALSE, mkts = c(1, 25, 520, 7, 14, 24, 6, 17, 91))
{
  stocklist.URL = "https://www.finam.ru/cache/N72Hgd54/icharts/icharts.js" #N72Hgd54
  tmp <- tempfile()
  download.file(stocklist.URL, destfile = tmp, quiet = !verbose)
  fr <- readLines(con = tmp, warn = FALSE)
  unlink(tmp)
  ids <- sub("var .*?= \\[", "", fr[1])
  ids <- sub("\\];", "", ids)
  ids <- strsplit(ids, ",")
  markets <- sub("var .*?= \\[", "", fr[4])
  markets <- sub("\\];", "", markets)
  markets <- strsplit(markets, ",")
  names <- sub("var .*?= \\[", "", fr[3])
  names <- sub("\\];", "", names)
  names <- gsub("'", "", names)
  names <- strsplit(names, ",")
  names <- names[[1]]
  #names <- names[-(11414)]
  res <- unlist(ids)
  markets <- unlist(markets)

  max_len <- min(length(res), length(names))
  data <- data.frame(name = names[1:max_len], em = res[1:max_len], market = markets[1:max_len])
  data_ <- data[data[, 3] %in% setdiff(mkts, c(1, 3)), ]
  data <- data[data[, 3] != 3, ]
  data <- rbind(data[(data[, 1] %in% data[data[, 3] == 1, 1]) &
                       data[, 3] == 1, ], data[!(data[, 1] %in% data[data[,
                                                                          3] == 1, 1]), ],
                data_)
  res <- data[, 2]
  names(res) <- data[, 1]
  return(res)
}

periods <- c('1min', '5min', '10min', '15min', '30min', 'hour','day','week')


finam.stock.list <- loadStockList()
#saveRDS(loadStockList(), "./data-raw/finam.stock.list.rds")
levels(finam.stock.list) <- c(levels(finam.stock.list),
                              484429, 484425, 484424, 484427, 484426,
                              484430, 484423, 484422, 484428,
                              481139, 487445, 476796,481141,
                              487598, 487537, 481317, 484958,
                              483886, 478823, 481295, 490796,
                              481299, 487518, 474242, 481300,
                              481307, 481310, 487529, 487597,
                              481311)
finam.stock.list['GDAX.BTC-USD'] <- 484429
finam.stock.list['GDAX.BTC-EUR'] <- 484425
finam.stock.list['GDAX.BTC-GBP'] <- 484424
finam.stock.list['GDAX.ETH-BTC'] <- 484427
finam.stock.list['GDAX.ETH-EUR'] <- 484426
finam.stock.list['GDAX.ETH-USD'] <- 484430
finam.stock.list['GDAX.LTC-BTC'] <- 484423
finam.stock.list['GDAX.LTC-EUR'] <- 484422
finam.stock.list['GDAX.LTC-USD'] <- 484428

finam.stock.list['BTCUSD'] <- 484429
finam.stock.list['BTCEUR'] <- 484425
finam.stock.list['BTCGBP'] <- 484424
finam.stock.list['ETHBTC'] <- 484427
finam.stock.list['ETHEUR'] <- 484426
finam.stock.list['ETHUSD'] <- 484430
finam.stock.list['LTCBTC'] <- 484423
finam.stock.list['LTCEUR'] <- 484422
finam.stock.list['LTCUSD'] <- 484428

finam.stock.list['RTSI'] <- 95


finam.stock.list['Sl'] <- 18952
finam.stock.list['PL'] <- 18947
finam.stock.list['ZW'] <- 74453
finam.stock.list['CA'] <- 18931
finam.stock.list['NI'] <- 18932
finam.stock.list['PA'] <- 18959
