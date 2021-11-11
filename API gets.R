
#' Get history price from BINANCE
#'
#' @param pair pair to dowload (eg. ETHBUSD)
#' @param from start month as string yyyy-mm
#' @param to end month of data as string yyyy-mm
#' @param tmp_file where to create tmp file that is used for dowloading
#'
#' @return
#' @export
#'
#' @examples
download.historical.data = function(pair, from, to, tmp_file = "data/history_data/tmp.zip"){
  
  d1 <- as.Date(paste0(from, "-01"), "%Y-%m-%d")
  d2 <- as.Date(paste0(to, "-01"), "%Y-%m-%d")
  dates <- format(seq(d1, d2 ,by = "month"), "%Y-%m")
  
  
  for(m in dates){
    
    # If its current month
    if(m == format(Sys.Date(), format = "%Y-%m")){
      
      
      if(file.exists(sprintf("data/history_data/%s-1d-%s.csv", pair, m))){
        currData = data.table(read.table(sprintf("data/history_data/%s-1d-%s.csv", pair, m), sep = ",", dec = ".", header = T)) 
      }else{
        currData = data.table()
      }
      
      lastDay = max(currData$Open_time)
      daysInCurrentMonth = seq(max(as.Date(lastDay)+1, as.Date(paste0(m, "-01")), na.rm = T),
                               Sys.Date()-1, by = "days")
      daysInCurrentMonth = as.character(daysInCurrentMonth)
      
      # ZMIENIC TAK, ZEBY WRITECSV APPENDOWALO ISTNIEJACY PLIK I WCZYTYWALO TYLKO BRAKUJACE DATY
      # Bedzie problem, bo nie zaktualizuje ostatniego miesiaca. plik bedzie istnial, ale nie bedzie kompletny (jesli przestanie byc ostatnim miesiacem)
      tryCatch({
        
        for(d in daysInCurrentMonth){
          download.file(
            url = sprintf("https://data.binance.vision/data/spot/daily/klines/%s/1d/%s-1d-%s.zip",
                          pair, pair, d),
            destfile = tmp_file, quiet = F)
          
          
          tmpFile = data.table(read.csv(unz(description = tmp_file,
                                            filename = sprintf("%s-1d-%s.csv", pair, d)), header = F))
          tmpFile[, V1 := as.character(as.POSIXct(V1/1000, origin = "1970-01-01", tz = "UTC"))]
          tmpFile[, V7 := as.character(as.POSIXct(V7/1000, origin = "1970-01-01", tz = "UTC"))]
          
          colnames(tmpFile) <- c("Open_time", "Open", "High", "Low", "Close", "Volume", "Close_time",
                                 "Quote_asset_vol", "No_of_trades",
                                 "Taker_buy_base_asset_vol", "Taker_buy_quote_asset_vol",
                                 "Ignore")
          currData = rbindlist(list(currData, tmpFile), use.names = T, fill = T)
        }
        
        
        write.table(currData, file = sprintf("data/history_data/%s-1d-%s.csv", pair, m),
                    sep = ",",dec = ".", row.names = F)
        
        file.remove(tmp_file)
      }, error = function(cond){ message(cond)})
    }
    
    if(!file.exists(sprintf("data/history_data/%s-1d-%s.csv", pair, m))){
      tryCatch({
        download.file(
          url = sprintf("https://data.binance.vision/data/spot/monthly/klines/%s/1d/%s-1d-%s.zip", pair, pair, m),
          destfile = tmp_file, quiet = F)
        
        tmpFile = data.table(read.csv(unz(description = tmp_file,
                                          filename = sprintf("%s-1d-%s.csv", pair, m)), header = F))
        tmpFile[, V1 := as.character(as.POSIXct(V1/1000, origin = "1970-01-01", tz = "UTC"))]
        tmpFile[, V7 := as.character(as.POSIXct(V7/1000, origin = "1970-01-01", tz = "UTC"))]
        colnames(tmpFile) <- c("Open_time", "Open", "High", "Low", "Close", "Volume", "Close_time",
                               "Quote_asset_vol", "No_of_trades",
                               "Taker_buy_base_asset_vol", "Taker_buy_quote_asset_vol",
                               "Ignore")
        write.table(tmpFile, file = sprintf("data/history_data/%s-1d-%s.csv", pair, m),
                    sep = ",",dec = ".", row.names = F)
        file.remove(tmp_file)
      }, error = function(cond){ message(cond)})
    }
    
    
  }
  cat(sprintf("Historical data for %s from %s to %s dowloaded.\n", pair, from, to))
}


binance.set.credentials <- function(priv_key = "", secret_key = ""){
  if(!is.null(priv_key)){
    Sys.setenv("BIN_PRV_KEY" = priv_key)
  }
  
  if(!is.null(secret_key)){
    Sys.setenv("BIN_SRC_KEY" = secret_key)
  }
}

binance.check.credentials <- function(){
  
  status = 1
  
  if(Sys.getenv("BIN_PRV_KEY") == ""){
    warning("Set private key using binance.set.credentials()") 
    status = 0
  }
  
  if(Sys.getenv("BIN_SRC_KEY") == ""){
    warning("Set private key using binance.set.credentials()") 
    status = 0
  }
  
  if(Sys.getenv("BIN_PRV_KEY") != "" & Sys.getenv("BIN_SRC_KEY") != ""){
    status_code = binance.query(endpoint = "/sapi/v1/capital/deposit/hisrec", sign = T) %>% 
      content("text") %>% 
      fromJSON()
    status_code = status_code$msg
    
    if(!is.null(status_code)){
      warning("Set keys using binance.set.credentials()") 
      status = 0
    }
  }
  
  return(status)
}

binance.query = function(domain = "https://api.binance.com", endpoint, method = "GET", params = list(), cnf = config(), sign = F){
  
  if (exists('used_weight')) {
    if (used_weight > 1159) {
      Sys.sleep(61 - as.integer(format(Sys.time(), "%S")))
    }
  }
  
  METHOD <- getFromNamespace(method, ns = 'httr')
  
  method = match.arg(method)
  if(sign){

    params$timestamp = as.character(round(as.numeric(.POSIXct(Sys.time(), tz = "UTC")) * 1e3))
    params$signature = digest::hmac(Sys.getenv("BIN_SRC_KEY"),
                                    object = paste(
                                      mapply(paste, names(params), params, sep = '=', USE.NAMES = FALSE),
                                      collapse = '&'),
                                    algo = "sha256") 
    cnf <- add_headers('X-MBX-APIKEY' = Sys.getenv("BIN_PRV_KEY"))
    
  }else{
    cnf = config()
  }
  
  
  res = METHOD(url = paste0(domain, endpoint),
               config = cnf,
               query = params
  )
  
  if(!is.null(res$headers$`x-mbx-used-weight`)){
    used_weight <<- as.integer((res$headers$`x-mbx-used-weight`))
  }
  
  
  
  return(res)
}

get.binance.actual.prices = function(){
  # Convert output to txt and then json to data list
  latest_price_json = binance.query(endpoint = "/api/v3/ticker/price") %>% 
    content("text") %>% 
    fromJSON()
  
  # Choose list's element with 'data' and change to data.table()
  latest_price_data = data.table(Pair = latest_price_json$symbol, actPrice = as.numeric(latest_price_json$price))
  
}


get.binance.possible.pairs = function(){
  # Convert output to txt and then json to data list
  poss_pairs = binance.query(endpoint = "/api/v3/exchangeInfo") %>% 
    content("text") %>% 
    fromJSON()
  
  poss_pairs <- data.table(poss_pairs$symbol)
  
  # Choose needed columns
  poss_pairs = poss_pairs[, .(symbol, baseAsset, quoteAsset)]
  return(poss_pairs)
}

get.binance.wallet = function(){
  wlt = binance.query(endpoint = "/sapi/v1/capital/config/getall", sign = T) %>%
    content("text") %>% 
    fromJSON() 
  res = data.table(Symbol = wlt$coin, Name = wlt$name, Free = as.numeric(wlt$free), Locked = as.numeric(wlt$locked), Freeze = as.numeric(wlt$freeze))
  
  res = res[Free + Locked + Freeze > 0]
  
  return(res)
}


get.binance.deposits = function(startTime = NULL){
  depos = binance.query(endpoint = "/sapi/v1/capital/deposit/hisrec", sign = T, params = list(start = startTime)) %>%
    content("text") %>% 
    fromJSON() 
  
  return(depos)
}

get.binance.withdraws = function(startTime = NULL){
  withs = binance.query(endpoint = "/sapi/v1/capital/withdraw/history", sign = T, params = list(start = startTime)) %>%
    content("text") %>% 
    fromJSON() 
  
  return(withs)
}

get.binance.fiat.hist <- function(transaction = 0, startTime = NULL){
  
  # 0 - deposit
  # 1 - withdraw
  
  fiats = binance.query(endpoint = "/sapi/v1/fiat/orders", sign = T,
                        params = list(transactionType = transaction, 
                                      beginTime = as.character(round(as.numeric(strptime(x = paste0(startTime," 00:00:01"),
                                                                                         format = "%Y-%m-%d %H:%M:%S"))) * 1e3))) %>%
    content("text") %>% 
    fromJSON() 
  
  fiats <- data.table(fiats$data)
  numcols = c("indicatedAmount", "amount", "totalFee")
  
  if(length(fiats) > 0){
    fiats[, side := ifelse(transaction == 0, "Deposit", "Withdraw")]
    
    fiats[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
    
    fiats[, createTime := as.POSIXct(createTime/1e3, tz = "GMT", origin = "1970-01-01")]
    fiats[, updateTime := as.POSIXct(updateTime/1e3, tz = "GMT", origin = "1970-01-01")]
    
  }
  
  return(fiats[])
}



# Only by each symbol... it takes much time to download all data.
get.binance.orders <- function(){
  
}

# Use this function to dowload all historical data on one symbol, .
get.binance.klines <- function(symbol, inter = "1d"){
  
  klines = binance.query(endpoint = "/api/v3/klines", sign = F,
                         params = list(symbol = symbol,
                                       interval = inter))%>%
    content("text") %>% 
    fromJSON() 
  
  klines <- data.table(klines)
  return(klines)
}





#' Downloading current exchange rates
#'
#' @param domain domain that is used for dowloading data. default is pro-api.coinmarketcap.com. Another choice could be sandbox-api.coinmarketcap.com
#' @param personal_key your personal key for CoinMarketCap API
#' @param convert what currency you want your crypto. Default is USD
#'
#' @return
#' @export
#'
#' @examples
download.actual.data.cmc <- function(domain = "pro-api.coinmarketcap.com", personal_key, convert = "USD"){
  
  
  time_get = format(Sys.time(), format = "%y%m%d%H%M%S")
  
  # API GET Latest price for 1:5000 crypto coins
  latest_price = httr::GET( 
    url = sprintf("https://%s/v1/cryptocurrency/listings/latest", domain), 
    
    httr::add_headers(
      `X-CMC_PRO_API_KEY` = personal_key, 
      `Accept` = "application/json",
      `Accept-Encoding` = "deflate, gzip"
    ),
    query = list(
      `start` = 1L,
      `limit` = 5000L,
      `convert` = convert
    )
  )
  
  if(latest_price$status_code != 200){
    stop(sprintf("Something went wrong. Status code: %d!", latest_price$status_code))
  }
  
  # Convert output to txt
  latest_price_txt = content(latest_price, "text")
  # Convert json to data list
  latest_price_json = fromJSON(latest_price_txt)
  # Choose list's element with 'data' and change to data.table()
  latest_price_data = data.table(latest_price_json$data)
  saveRDS(latest_price_data, sprintf("data/actual_data_snap/%s_actual_data.rds", time_get))
  cat(sprintf("Data downloaded at %s and saved in %s", 
              Sys.time(),
              sprintf("data/actual_data_snap/%s_actual_data.rds", time_get)))
}


