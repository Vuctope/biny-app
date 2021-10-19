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
download.actual.data <- function(domain = "pro-api.coinmarketcap.com", personal_key, convert = "USD"){
  
  require(httr)
  require(jsonlite)
  require(data.table)
  
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

#' Read in current rates
#'
#' @param update_file should the current rates be updated 
#' @param warn_days how old rates must be to consider them old
#' @param ... other parameters passed on download.actual.data
#'
#' @return
#' @export
#'
#' @examples
read.actual.data <- function(update_file = F, warn_days = 3, ...){
  
  if(update_file){
    download.actual.data(domain = domain,
                         personal_key = personal_key, 
                         convert = convert)
  }
  
  last_file_name = list.files("data/actual_data_snap/", pattern = "_actual_data")
  last_file_name = last_file_name[length(last_file_name)]
  
  last_update_date = as.Date(paste0("20",substr(last_file_name, 1, 6)), format = "%Y%m%d")
  time_from_update = as.integer( Sys.Date() - last_update_date)
  
  if(time_from_update >= warn_days){
    message(sprintf("Data was last updated %s (%d days ago). Consider updating it.", last_update_date, time_from_update))
  }
  
  
  latest_price_data = readRDS(sprintf("data/actual_data_snap/%s", last_file_name))
}



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

#' Read history price dowloaded from BINANCE
#'
#' @param pair read historical data for pair (symbol)
#'
#' @return
#' @export
#'
#' @examples
read.historical.data = function(pair){
  
  filesToRead = list.files(path = "data/history_data/", pattern = pair)
  

  histData = rbindlist(l = lapply(X = filesToRead,
                                          FUN = function(x){
                                            data.table(read.table(sprintf("data/history_data/%s", x),
                                                                  sep = ",", dec = ".", header = T))
                                            }
  ))
  histData[, Pair := pair]
  setcolorder(x = histData, c("Pair"))
  return(histData)
}
