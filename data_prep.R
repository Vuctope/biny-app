


list.fiats <- function(){
  c("EUR")
}

list.denomin = function(){
  c("USDT", "BUSD", "AUD", "BIDR", "BRL", "EUR", "GBP", "RUB", "TRY", "TUSD", "USDC", "DAI", "IDRT", "UAH", "NGN", "BVND", "VAI", "USDP", #FIAT
    "BTC", "BNB", "ETH")
}

convert.timezones = function(time, tz = Sys.timezone()){
  lubridate::with_tz(time = time, tzone = tz)
}

# To be developed
read.any.file <- function(x, use.fread = T){
  
  # Check the extension of file
  fl_ext = tools::file_ext(x)
  if(fl_ext %in% c("csv", "txt")){
    res = fread(x)
  }
  if(fl_ext %in% c("xlsx", "xls")){
    res = data.table(readxl::read_excel(path = x, sheet = 1))
    
  }
  
  return(res)
}

read.transaction.history = function(tr_file){
  
  trList = read.any.file(tr_file)
  
  if(all(names(trList) %in% c("Date(UTC)", "Pair", "Side", "Price", "Executed", "Amount", "Fee"))){
    # Works with csv
    cols = c("Executed", "Amount", "Fee")
    cols2 = c(cols, "Price")
    trList[, (paste0(cols, "_COIN")) := lapply(.SD, str_extract, pattern = "[A-Z].+"), .SDcols = cols]
    trList[, (cols) := lapply(.SD, str_replace, pattern = "[A-Z].+", replacement = ""), .SDcols = cols]
    trList[, (cols2) := lapply(.SD, str_replace_all, pattern = ",", replacement = ""), .SDcols = cols2]
    trList[, (cols2) := lapply(.SD, as.numeric), .SDcols = cols2]
    trList = trList[, .(Date = as.POSIXct(`Date(UTC)`,  tz = "GMT"),
                        Pair, Side, Price, Executed, Executed_COIN, Amount, Amount_COIN, Fee, Fee_COIN)]
    
  }else{
    # Works with XLSX
    posPair = get.binance.possible.pairs()
    trList <- merge(trList, posPair, by.x = "Market", by.y = "symbol", all.x = T, sort = F)
    if(any(is.na(trList$nominator))) stop("Lack of pair, I'm sorry.")
    cols = c("Price", "Amount", "Total", "Fee")
    trList[, (cols) := lapply(.SD, str_replace_all, pattern = ",", replacement = ""), .SDcols = cols]
    trList[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    trList = trList[, .(Date = as.POSIXct(`Date(UTC)`, tz = "GMT"),
                        Pair = Market, Side = Type, Price,
                        Executed = Amount, Executed_COIN = baseAsset,
                        Amount = Total, Amount_COIN = quoteAsset,
                        Fee, Fee_COIN = `Fee Coin`)]
    
    
  }
  
  setnames(trList, old = names(trList), new = tolower(names(trList)))
  
  return(trList)
}

add.transaction.history = function(old_list, new_list){
  
  if(is.character(old_list)){
    oldList = read.transaction.history(old_list)
  }else{
    oldList = old_list
  }
  
  if(is.character(new_list)){
    newList = read.transaction.history(new_list)
  }else{
    newList = new_list
  }
  
  # Remove transactions from newList, that are present in oldList
  newList = newList[!as.character(date) %in% as.character(oldList$date)]
  
  newList = rbindlist(l = list(newList, oldList))
  return(newList)
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
    download.actual.data.cmc(domain = domain,
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
  
  # if(length(filesToRead)==0){
  #   download.historical.data()
  # }
  
  histData = rbindlist(l = lapply(X = filesToRead,
                                  FUN = function(x){
                                    data.table(read.table(sprintf("data/history_data/%s", x),
                                                          sep = ",", dec = ".", header = T))
                                  }
  ))
  histData[, Pair := pair]
  setcolorder(x = histData, c("Pair"))
  
  histData[, Open_time := as.POSIXct(Open_time)]
  histData[, Close_time := as.POSIXct(Close_time)]
  
  return(histData[])
}

count.ex.rate = function(from, to = "USDT", actual_price = NULL){
  
  if(any(from != to)){
    tmp = data.table(Pair = paste0(from, to))
    
    if(is.null(actual_price)){
      actual_price = download.actual.prices()
    }
    
    tmp = merge(tmp, actual_price, all.x = T, by = "Pair", sort = F)
    tmp[from == to, actPrice := 1]
    ex = tmp$actPrice
  }else{
    ex = rep(1, length(from))
  }
  
  return(ex)
}


count.coins.amount = function(transactions){
  # Counting wallet

  # Nominator - which crypto is being bought
  crypto_nomin = transactions[,  .(NOMINS = sum(executed[side == "BUY"]) - sum(executed[side == "SELL"])),
                              by = .(executed_coin)]
  # Denominator - which crypto you are using to pay
  crypto_denomin = transactions[, .(DENOMINS = sum(amount[side == "SELL"]) - sum(amount[side == "BUY"])),
                                by = .(amount_coin)]
  # Fees - which crypto are used as fees for exchange
  crypto_fees = transactions[, .(FEES = sum(fee)), by = .(fee_coin)]


  # Made for later - possible to count in other than USD stablecoins


  crypto_wallet <- merge(crypto_nomin, crypto_denomin, by.x = "executed_coin", by.y = "amount_coin", all = T)
  crypto_wallet <- merge(crypto_wallet, crypto_fees, by.x = "executed_coin", by.y = "fee_coin", all = T)
  crypto_wallet[is.na(crypto_wallet)] <- 0

  # Current wallet (in crypto)
  crypto_wallet[, BAL := NOMINS + DENOMINS - FEES]
  return(crypto_wallet[])

}

count.coins.balance = function(transactions, actual, coin = "USDT"){
  # Convert all amounts and fees to BNB
  
  transactions[, amount_bnb := amount / count.ex.rate("BNB", to = amount_coin, actual_price = actual)]
  transactions[, fee_bnb := fee / count.ex.rate("BNB", to = fee_coin, actual_price = actual)]
  transactions[is.na(fee_bnb), fee_bnb := fee * count.ex.rate(fee_coin, to = "BNB", actual_price = actual)]
  
  transactions[, amount_show := amount_bnb * count.ex.rate("BNB", to = coin, actual_price = actual)]
  transactions[, fee_show := fee_bnb * count.ex.rate("BNB", to = coin, actual_price = actual)]
  
  # if still NAS in fees or amounts - maybe only native currency is allowed
  transactions[is.na(amount_show), amount_show := amount * count.ex.rate(amount_coin, to = coin, actual_price = actual)]
  transactions[is.na(fee_show), fee_show := fee * count.ex.rate(fee_coin, to = coin, actual_price = actual)]

  
  # How much crypto was bought, avg price and total spend
  # Count mean price in chosen currency - doesn't concern fee yet!!!!
  crypto_purchases = transactions[, .(COIN = sum(executed),
                                      PAY = sum(amount_show)),
                                  by = .(executed_coin, side)]
  crypto_purchases[, MEAN_PRICE := PAY/COIN]
  crypto_purchases = dcast(crypto_purchases, formula = executed_coin ~ side, value.var = c("COIN", "PAY", "MEAN_PRICE"))
  crypto_purchases[is.na(crypto_purchases)] <- 0
  crypto_purchases[, COIN_USED := coin]
  return(crypto_purchases[])
}

get.fiat.history = function(startTime = "2021-05-01"){
  cash_deposited = get.binance.fiat.hist(transaction = 0, startTime = startTime)
  cash_withdrawed = get.binance.fiat.hist(transaction = 1, startTime = startTime)
  fiat_hist = rbindlist(l = list(cash_deposited, cash_withdrawed))
  return(fiat_hist[])
}

# Prepare table for Wallet
prep.wallet = function(wallet, transactions, actual, coin){
  
  transPrices = count.coins.balance(transactions = transactions, actual = actual, coin = coin)
  transPrices = transPrices[, .(executed_coin, PAY_BUY, PAY_SELL, MEAN_PRICE_BUY, MEAN_PRICE_SELL)]
  
  # coinAvlbl <- merge(coinAvlbl, actual[, .(symbol, quote.USD.price, quote.USD.market_cap)], by.x = "Executed_COIN", by.y = "symbol", all.x = T)
  wallet[, Bal :=  Free + Locked + Freeze]
  
  # Almost every coin is supported to BNB or USDT exchange so i convert it to BNB then USDT and then to chosen coin
  wallet[, Pair := paste0(Symbol, "USDT")]
  wallet <- merge(wallet, actual, by = "Pair", all.x = T)
  wallet[ Pair == "USDTUSDT", actPrice := 1]
  wallet[, actPriceCoin := actPrice*count.ex.rate("USDT", to = coin, actual_price = actual)]
  # merge with overall buys and sells
  wallet <- merge(wallet, transPrices, all = T, by.x = "Symbol", by.y = "executed_coin")
  
  
  wltTbl = wallet[, .(Symbol, Bal, Locked, 
                      Spent = PAY_BUY, Earned = PAY_SELL, ActValue = actPriceCoin * Bal,
                      SpentAvg = MEAN_PRICE_BUY, EarnedAvg = MEAN_PRICE_SELL, actPriceCoin)]
  wltTbl[is.na(Bal), Bal := 0]
  wltTbl[is.na(Locked), Locked := 0]

  wltTbl = wltTbl[order(-ActValue)]
  
return(wltTbl[])
}

# Prepare table for Transactions
prep.transactions <- function(transactions){
  crypto_trans = transactions[, .(Date = format(date, format = "%Y-%m-%d %H:%M:%S"),
                                  Pair = pair,
                                  Side = side,
                                  Executed = executed,
                                  Price = sprintf("%f (fee: %f%s)", amount, fee, fee_coin),
                                  AvgPrice = price)]
  return(crypto_trans[])
}


# aggregate.data <- function(x, interval){
#   
#   if(interval )
#   x[, month := lubridate::ym(substr(Day, 1, 7))]
#   x[, month := lubridate::ym(Day)]
# }

# Plot transactions in time
# TODO: Pomyslec, czy powinienem tutaj dawac coin czy pair? jesli kupie za eth, to oddzielnie? czy przeliczenie na usd po kursie w danym dniu? Jesli po kursie, to po ktorym?
plot.transactions <- function(transactions, pair_history, interval = "d"){
  
  tr.plot <- plot_ly(pair_history, x = ~as.Date(Open), type="candlestick", name = "",
                     open = ~Open, close = ~Close,
                     high = ~High, low = ~Low)
  tr.plot <- add_trace(tr.plot, data = transactions[side == "SELL"], x = ~as.Date(date), y = ~price,  size = ~executed, color = I("red"),
                       inherit = F,  mode = 'markers', name = "SELL", text = ~paste("Price: ", price, '<br>Executed:', executed))
  tr.plot <- add_trace(tr.plot, data = transactions[side == "BUY"], x = ~as.Date(date), y = ~price,  size = ~executed, color = I("green"),
                       inherit = F,  mode = 'markers', name = "BUY", text = ~paste("Price: ", price, '<br>Executed:', executed))
  tr.plot = layout(tr.plot,
                   xaxis = list(rangeslider = list(visible = F), title = "Date"),
                   yaxis = list(title = "Price"),
                   showlegend = F)
  tr.plot
  
}


plot.balance <- function(transactions, actual){
  
}


