options(scipen = 30)
library(data.table)
library(stringr)



trList = fread("part-00000-9b34cdf4-1915-4031-bfe1-3e7d72818db7-c000.csv")
str(trList)

setnames(trList, "Date(UTC)", "Date")

cols = c("Executed", "Amount", "Fee")
trList[, (cols) := lapply(.SD, str_replace_all, pattern = ",", replacement = ""), .SDcols = cols]


trList[, (paste0(cols, "_COIN")) := lapply(.SD, str_extract, pattern = "[A-Z].+"), .SDcols = cols]
trList[, (cols) := lapply(.SD, str_replace, pattern = "[A-Z].+", replacement = ""), .SDcols = cols]
trList[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]


trList[, .(Executed = sum(Executed), 
           Amount = sum(Amount),
           Fee = sum(Fee)),
           by = .(Executed_COIN, Amount_COIN, Fee_COIN, Side)]

sum(trList[Executed_COIN == "EUR" & Side == "SELL"]$Executed)



# Counting wallet

# Nominator - which crypto is being bought
crypto_nomin = trList[,  .(NOMINS = sum(Executed[Side == "BUY"]) - sum(Executed[Side == "SELL"])),
                    by = .(Executed_COIN)]
# Denominator - which crypto you are using to pay
crypto_denomin = trList[, .(DENOMINS = sum(Amount[Side == "SELL"]) - sum(Amount[Side == "BUY"])),
                        by = .(Amount_COIN)]
# Fees - which crypto are used as fees for exchange
crypto_fees = trList[, .(FEES = sum(Fee)), by = .(Fee_COIN)]

crypto_wallet <- merge(crypto_nomin, crypto_denomin, by.x = "Executed_COIN", by.y = "Amount_COIN", all = T)
crypto_wallet <- merge(crypto_wallet, crypto_fees, by.x = "Executed_COIN", by.y = "Fee_COIN", all = T)
crypto_wallet[is.na(crypto_wallet)] <- 0

# Current wallet (in crypto)
crypto_wallet[, BAL := NOMINS + DENOMINS - FEES]

# Avarage buy cost
trList[Side == "BUY", .(BOUGHT = sum(Executed) - sum(Fee[Executed_COIN == Fee_COIN]),
                        PRICE = sum(Amount)), 
       by = .(Executed_COIN, Amount_COIN)]


count.ex.rate = function(exchange.rate = 1){
 ex = 1
 return(ex)
}
