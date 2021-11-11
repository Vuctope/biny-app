set.usr = function()
{
  Sys.setenv("PG_UID" = rstudioapi::showPrompt(title = "Database user", message = "User name:", default = "postgres"))
  Sys.setenv("PG_PWD" = rstudioapi::askForPassword("Database password"))
  
}

set.conn = function()
{
  if(Sys.getenv("PG_UID") ==  "" | Sys.getenv("PG_PWD") == ""){
    set.usr()
  }
  
  con = tryCatch(dbConnect(drv = odbc::odbc(),
                           Driver   = "PostgreSQL ODBC Driver(UNICODE)",
                           Server   = "localhost",
                           Database = "binyapp",
                           UID      = Sys.getenv("PG_UID"),
                           PWD      = Sys.getenv("PG_PWD"),
                           Port     = 5432), 
                 error=function(e) {
                   message(paste("Wrong credentials. Try again!"))
                   Sys.setenv("PG_UID" = "")
                   Sys.setenv("PG_PWD" = "")
                   # Choose a return value in case of error
                   return(NA)
                 })
  
  return(con)
}

s.query = function(qry)
{
  con <- set.conn()
  
  q = data.table(DBI::dbGetQuery(conn = con, qry))
  
  q = q[,  lapply(.SD, trimws), .SDcols = names(q)]
  
  dbDisconnect(conn = con)
  
  return(q)       
}

append.history = function(dt, id)
{
  if(is.null(id) | !exists("id"))
    stop("Must provide id")
  
  hash_id = hmac(id, object = id, algo = "sha256")
  
  dt_w = copy(dt)
  
  # przesuwam na poczatek, zeby Wolumen byl w dobrym miejscu
  dt_w[, usr_id := hash_id]
  setcolorder(dt_w, neworder = c("usr_id"))
  
  con <- set.conn()
  
  maxDate = s.query(sprintf("SELECT max(date) max_date FROM usr_data.transactions_history where usr_id = '%s'", hash_id))

  if(!is.na(maxDate$max_date))
    dt_w = dt_w[date > maxDate$max_date]
  
  
  if(nrow(dt_w) > 0){
    
    DBI::dbAppendTable(conn = con, name = DBI::Id(schema = "usr_data", table = "transactions_history"), value = dt_w, row.names = NULL)
    message("\n------------------------New entrances added ------------------------\n")
    
  }else{
    message("\n------------------------No new entrances------------------------\n")
  }
  
  dbDisconnect(conn = con)
  
}

get.transactions = function(id)
{
  if(is.null(id) | !exists("id"))
    stop("Must provide id")
  
  hash_id = hmac(id, object = id, algo = "sha256")
  tr_list = s.query(sprintf("SELECT * FROM usr_data.transactions_history where usr_id = '%s'", hash_id))
  
  numcols = c("price", "executed", "amount", "fee")
  
  tr_list[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
  tr_list[, date := as.POSIXct(date, tz = "GMT")]
  tr_list[, usr_id := NULL]
  return(tr_list[])
  
}


