#Warning in binance.check.credentials() :
# Timestamp for this request was 1000ms ahead of the server's time.Set keys using binance.set.credentials()
# WHEN THIS MESSAGE SHOWS, JUST SYNC COMPUTER TIME
#
# To add: difference between what was bought and what is in wallet - these are assets that are on other accounts/wallets
# the only thing is that i cannot count how much profit it gives



library(DBI)
library(ggplot2)
library(plotly)
library(httr)
library(jsonlite)
library(data.table)
library(stringr)
library(lubridate)
library(tools)
library(quantmod)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(digest)
library(callr) #function for running another R session
# library(Rcpp)

options(scipen = 30, digits = 10)

source("data_prep.R")
source("API gets.R")
# source("DB_gets.R")
source("renders.R")

# Read all data - for testing reasons before defining UI
coin = "USDT"
fiats = list.fiats()
#transactions = prep.transaction.history(transactions = get.history.for.pairs(type = "trade"))
if(file.exists("latest_trans_data.rds")){
    transactions = readRDS("latest_trans_data.rds")
}


ui <- fluidPage(
    
    dashboardPage(
        dashboardHeader(title = "BinyApp (Binance Shiny)",
                        leftUi = tagList( 
                            actionButton(inputId = "btnUpdate", label = "Refresh", icon = icon("redo")
                            )
                        )
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Wallet", tabName = "tabWallet", icon = icon("wallet")),
                menuItem("Transactions", tabName = "tabTrans", icon = icon("exchange-alt")),
                menuItem("Inspect Pair", tabName = "tabCoin", icon = icon("search-dollar"))
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "tabWallet",
                        fluidRow(
                            valueBoxOutput("boxEurIn", width = 4),
                            valueBoxOutput("boxDolIn", width = 4),
                            valueBoxOutput("boxDolOut", width = 4)
                            
                        ),
                        fluidRow(
                            DTOutput(outputId = "tblWallet")
                        )
                        
                ),
                tabItem(tabName = "tabTrans",
                        fluidRow(DTOutput(outputId = "tblTrans"), width = 12)
                ),
                tabItem(tabName = "tabCoin",
                        fluidRow(
                            uiOutput(outputId = 'slctPairUi')
                        ),
                        # fluidRow(
                        
                        # ),
                        fluidRow(
                            box(plotlyOutput(outputId = "pltPairHist"))
                        ),
                        fluidRow(
                            box(DTOutput(outputId = "tblTransPair"), width = 12)
                        )
                        
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Define all reactive values
    rvUpdate <- reactiveVal(NA)
    rvStatus <- reactiveVal(0)
    rvActual <- reactiveVal()
    rvTrans <- reactiveVal()
    rvWallet <- reactiveValues(wallet = NULL,
                               fiats = NULL,
                               fiat_history = NULL)
    rvPair <- reactiveValues(pair = NULL, pairHist = NULL, transHist = NULL)
    
    # Show modal to add credentials
    showModal(modalDialog(
        title = "Need your credentials",
        textInput("privKey", label = NULL, placeholder = "Insert your private key here"),
        textInput("secretKey", label = NULL, placeholder = "Insert your secret key here"),
        textOutput("txtStatus"),
        easyClose = F,
        footer = tagList(
            actionButton(inputId = "btnCredentials", label = "Dowload data", icon = icon("arrow-up")),
        )
    ))
    
    # Check if credentials are OK
    observeEvent(input$btnCredentials, {
        binance.set.credentials(priv_key = input$privKey, secret_key = input$secretKey)
        
        status = binance.check.credentials()
        
        if(status == 0){
            output$txtStatus <- renderText({"Wrong credentials!"})
            cat("Credentials not added\n")
            
        }else{
            output$txtStatus <- renderText({"Welcome!"})
            cat("Credentials added\n")
        }
        
        rvStatus(status)
        
        if(rvStatus() == 1){
            removeModal() 
            rvUpdate(0)
        }
    })
    
    observeEvent(input$btnUpdate, {
        rvUpdate(input$btnUpdate)
    })
    
    # tabWallet
    
    observeEvent(rvUpdate(), {
        if(rvStatus() == 1){
            
            rvActual(get.binance.actual.prices())
            rvTrans({
                if(exists("transactions")){
                    transactions
                }else{
                    prep.transaction.history(transactions = get.history.for.pairs(type = "trade"))
                }
            })
            rvWallet$wallet = prep.wallet(wallet = get.binance.wallet(),
                                          transactions = rvTrans(),
                                          actual = rvActual(),
                                          coin = "USDT")
            
            rvWallet$fiat_history = get.fiat.history("2020-05-01")
            
            cat("No. of updates:", input$btnUpdate,"\n")
            
            fiat_status = sum(rvWallet$fiat_history[side == "Deposit" &  status == "Successful"]$amount) - sum(rvWallet$fiat_history[side == "Withdraw" & status == "Successful"]$amount)
            
            output$boxEurIn <- renderVB(value = paste0(fiat_status, " (", rvWallet$wallet[Symbol == "EUR"]$Bal, " in Wallet)"),
                                        subtitle = "EUR invested",
                                        icon = icon("euro-sign"))
            output$boxDolIn <- renderVB(value = sum(rvTrans()[Executed_COIN == "EUR" & Amount_COIN %in% c("USDT", "BUSD")]$Amount),
                                        subtitle = "UDST/BUSD invested",
                                        icon = icon("dollar-sign"))
            
            output$boxDolOut <- renderVB(value = sum(rvWallet$wallet$ActValue, na.rm = T),
                                         subtitle = "Actual balance",
                                         icon = icon("dollar-sign"))
            
            output$tblWallet <- renderOwnDT({rvWallet$wallet})
        }
        
        
    })
    
    # tabTrans
    observeEvent(rvUpdate(), {
        if(rvStatus() == 1){
            output$tblTrans <- renderOwnDT({prep.transactions(rvTrans())})
        }
    })
    
    
    
    # tabPair
    
    
    observeEvent(eventExpr = input$slctPair, {
        rvPair$pair <- input$slctPair
        rvPair$pairHist <- get.binance.klines(symbol = input$slctPair)
        rvPair$transHist <- rvTrans()[Pair == input$slctPair]
        
    })
    
    output$slctPairUi <- renderUI({
        box(selectizeInput(inputId = "slctPair", label = "Select Pair",
                           choices = sort(unique(rvTrans()$Pair)), selected = "ETHUSDT"))
    })
    
    observeEvent(rvPair, {
        output$pltPairHist <- renderPlotly({
            cat("Plot of", rvPair$pair, "\n")
            plot.transactions(transactions = rvPair$transHist,
                              pair_history = rvPair$pairHist)
        })
    })
    # 
    # 
    # output$tblTransPair <- renderOwnDT({rvPair$transHist})
    # onStop(function() {
    #     cat("Stopping app")
    #     saveRDS(rvTrans(), "latest_trans_data.rds")
    # })
    
}

# onStart = function() {
#     cat("Doing application setup\n")
#     
#    
# }
# Run the application 
shinyApp(ui = ui, server = server)
