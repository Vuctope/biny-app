options(scipen = 30, digits = 10)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(tools)



list.denomin = function(){
  c("USDT", "BUSD", "AUD", "BIDR", "BRL", "EUR", "GBP", "RUB", "TRY", "TUSD", "USDC", "DAI", "IDRT", "UAH", "NGN", "BVND", "VAI", "USDP", #FIAT
    "BTC", "BNB", "ETH")
}

list.possible.pairs = function(){
  suppressWarnings({
    symbol_list = read.actual.data(update_file = F)
    symbol_list = symbol_list$symbol
  })
  
  denominators = list.denomin()
  
  # No FIATs in symbol_list, adding them based on list.denomin
  symbol_list <- unique(c(symbol_list, denominators))
  
  possible_pairs = data.table(nominator = rep(symbol_list, each = length(denominators)),
                              denominator = denominators)
  possible_pairs[, pair := paste0(nominator, denominator)]
  return(possible_pairs)
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
    trList = trList[, .(Date = as.POSIXct(`Date(UTC)`),  Day = lubridate::date(`Date(UTC)`),
                        Pair, Side, Price, Executed, Executed_COIN, Amount, Amount_COIN, Fee, Fee_COIN)]
    
  }else{
    # Works with XLSX
    posPair = list.possible.pairs()
    trList <- merge(trList, posPair, by.x = "Market", by.y = "pair", all.x = T, sort = F)
    if(any(is.na(trList$nominator))) stop("Lack of pair, I'm sorry.")
    cols = c("Price", "Amount", "Total", "Fee")
    trList[, (cols) := lapply(.SD, str_replace_all, pattern = ",", replacement = ""), .SDcols = cols]
    trList[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    trList = trList[, .(Date = as.POSIXct(`Date(UTC)`),  Day = lubridate::date(`Date(UTC)`),
                        Pair = Market, Side = Type, Price,
                        Executed = Amount, Executed_COIN = nominator,
                        Amount = Total, Amount_COIN = denominator,
                        Fee, Fee_COIN = `Fee Coin`)]
    
    
  }
  
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
  newList = newList[!as.character(Date) %in% as.character(oldList$Date)]
  
  newList = rbindlist(l = list(newList, oldList))
  return(newList)
}

# works fine for now!
# trList = read.transaction.history("data/210810_trade_history.csv")
# trList = add.transaction.history(old_list = trList, new_list = "data/Eksportuj Historiê Transakcji-2021-10-19 21_51_01.xlsx")

# trList[, .(Executed = sum(Executed), 
#            Amount = sum(Amount),
#            Fee = sum(Fee)),
#        by = .(Executed_COIN, Amount_COIN, Fee_COIN, Side)]
# 
# sum(trList[Executed_COIN == "EUR" & Side == "SELL"]$Executed)

count.ex.rate = function(ex.rate = 1){
  ex = ex.rate
  return(ex)
}




count.coins.amount = function(transactions){
  # Counting wallet
  
  # Nominator - which crypto is being bought
  crypto_nomin = transactions[,  .(NOMINS = sum(Executed[Side == "BUY"]) - sum(Executed[Side == "SELL"])),
                              by = .(Executed_COIN)]
  # Denominator - which crypto you are using to pay
  crypto_denomin = transactions[, .(DENOMINS = sum(Amount[Side == "SELL"]) - sum(Amount[Side == "BUY"])),
                                by = .(Amount_COIN)]
  # Fees - which crypto are used as fees for exchange
  crypto_fees = transactions[, .(FEES = sum(Fee)), by = .(Fee_COIN)]
  
  
  # Made for later - possible to count in other than USD stablecoins
  
  
  crypto_wallet <- merge(crypto_nomin, crypto_denomin, by.x = "Executed_COIN", by.y = "Amount_COIN", all = T)
  crypto_wallet <- merge(crypto_wallet, crypto_fees, by.x = "Executed_COIN", by.y = "Fee_COIN", all = T)
  crypto_wallet[is.na(crypto_wallet)] <- 0
  
  # Current wallet (in crypto)
  crypto_wallet[, BAL := NOMINS + DENOMINS - FEES]
  return(crypto_wallet)
  
}

count.coins.balance = function(transactions){
  
  # How much crypto was bought, avg price and total spend
  # Count mean price in chosen currency - doesn't concern fee yet!!!!
  crypto_purchases = transactions[, .(COIN = sum(Executed),
                                      PAY = sum(Amount)*count.ex.rate(ex.rate = 1)),
                                  by = .(Executed_COIN, Side)]
  crypto_purchases[, MEAN_PRICE := PAY/COIN]
  crypto_purchases = dcast(crypto_purchases, formula = Executed_COIN ~ Side, value.var = c("COIN", "PAY", "MEAN_PRICE"))
  crypto_purchases[is.na(crypto_purchases)] <- 0
  return(crypto_purchases)
}


# Prepare table for Wallet2
prep.wallet = function(transactions){
  
  coinAvlbl = count.coins.amount(transactions = transactions)
  coinBal = count.coins.balance(transactions = transactions)
  wltTbl = merge(coinAvlbl[, .(Executed_COIN, BAL)], 
                 coinBal[, .(Executed_COIN,
                             BUYSELL = paste(COIN_BUY, COIN_SELL, sep = "/"),
                             PAY_BUY, MEAN_PRICE_BUY,
                             PAY_SELL, MEAN_PRICE_SELL)])
  
  wltTbl[, VALUE_NOW := "NOT READY"]
  
  return(wltTbl)
}

# Prepare table for Transactions
prep.transactions <- function(transactions){
  crypto_trans = transactions[, .(Date,
                                  Pair,
                                  Side,
                                  Executed,
                                  Price = sprintf("%f (fee: %f%s)", Amount, Fee, Fee_COIN),
                                  AvgPrice = Amount/Executed)]
  return(crypto_trans)
}

# Plot transactions in time
# TODO: Pomyslec, czy powinienem tutaj dawac coin czy pair? jesli kupie za eth, to oddzielnie? czy przeliczenie na usd po kursie w danym dniu? Jesli po kursie, to po ktorym?
plot.transactions <- function(dt, coin, session = NULL){
  
  
  tr.plot = ggplot() +
    geom_point(data = dt, aes(x = Day, y = Price, color = Side, size = Executed)) +
    theme(legend.position = "none")
  
  if(!is.null(session)){
    ggplotly(tr.plot)
  }
}
