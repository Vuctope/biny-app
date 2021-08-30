#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)


ui <- fluidPage(
    
    dashboardPage(
        dashboardHeader(title = "Biny (Binance Shiny)"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Wallet", tabName = "tabWallet", icon = icon("wallet")),
                menuItem("Transactions", tabName = "tabTrans", icon = icon("exchange-alt")),
                menuItem("Coin", tabName = "tabCoin", icon = icon("search-dollar"))
            )
        ),
        
        dashboardBody(
            tabItem(tabName = "tabTrans",
                    fluidRow(
                        valueBoxOutput("boxEur", width = 6),
                        valueBoxOutput("boxDol", width = 6)
                        
                    ),
                    fluidRow(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30)
                    ),
                    fluidRow(
                        plotOutput("distPlot")
                    )   
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$boxEur <- renderVB(value = "999.99", subtitle = "EUR invested", icon = icon("euro-sign"))
    output$boxDol <- renderVB(value = "999.99", subtitle = "UDST/BUSD invested", icon = icon("dollar-sign"))
}

# Run the application 
shinyApp(ui = ui, server = server)
