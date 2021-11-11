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
# library(Rcpp)

options(scipen = 30, digits = 10)

source("data_prep.R")
source("API gets.R")
source("DB_gets.R")
source("renders.R")

# Read all data - for testing reasons before defining UI
coin = "USDT"
fiats = list.fiats()


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
                menuItem("Coin", tabName = "tabCoin", icon = icon("search-dollar"))
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
                        # fluidRow(
                        #     box(selectizeInput(inputId = "slctPair", label = "Select Pair",
                        #                        choices = sort(unique(trHist$pair)), selected = "ETHUSDT"))
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
    rvPair <- reactiveValues(pair = NULL, pairHist = NULL, transHist = NULL, transHistTab = NULL)
    
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
            rvTrans(get.transactions(id = Sys.getenv("BIN_PRV_KEY")))
            
            rvWallet$wallet = prep.wallet(wallet = get.binance.wallet(),
                                          transactions = rvTrans(),
                                          actual = rvActual(),
                                          coin = "USDT")
            
            rvWallet$fiat_history = get.fiat.history("2020-05-01")
            
            cat("No. of updates:", input$btnUpdate,"\n")
            
            fiat_status = sum(rvWallet$fiat_history[side == "Deposit"]$amount) - sum(rvWallet$fiat_history[side == "Withdraw"]$amount)
            
            
            output$boxEurIn <- renderVB(value = fiat_status,
                                        subtitle = "EUR invested",
                                        icon = icon("euro-sign"))
            output$boxDolIn <- renderVB(value = sum(rvTrans()[executed_coin == "EUR" & amount_coin %in% c("USDT", "BUSD")]$amount),
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
    
    
    
    # tabCoin
    
    
    
    # observeEvent(eventExpr = input$slctPair, {
    #     rvPair$pair <- input$slctPair
    #     rvPair$pairHist <- read.historical.data(pair = input$slctPair)
    #     rvPair$transHist <- trHist[pair == input$slctPair]
    #     rvPair$transHistTab <- prep.transactions(trHist[Pair == input$slctPair])
    # })
    # 
    # isolate(output$pltPairHist <- renderPlotly({
    #     cat("Plot of", rvPair$pair, "\n")
    #     plot.transactions(transactions = rvPair$transHist,
    #                       pair_history = rvPair$pairHist)
    # }))
    # 
    # 
    # output$tblTransPair <- renderOwnDT({rvPair$transHistTab})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
