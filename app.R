library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
bcl <- read.csv("bcl-data.csv",stringsAsFactors = FALSE)
print(str(bcl))

ui <- dashboardPage(skin = "green",
    dashboardHeader( title = "Innocent's Liquor Store"),
    dashboardSidebar(tags$style(HTML('.skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
    background-color: #063e4c;
}')),
                     sidebarMenu(
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("Main Menu", tabName = "widgets", icon = icon("th"))
                     ),
                     sidebarMenu(
                         
                     sliderInput("priceInput", "Price", min = 0, max = 100,
                                  value = c(25, 40), pre = "$")
                     ),
                     radioButtons("typeInput", "Product type",
                                  choices = c("BEER","SPIRITS", "WINE"),
                                  selected = "WINE"),
                     selectInput("countryInput", "Country",
                                 choices = c("CANADA", "FRANCE", "ITALY","BELGIUM","UNITED STATES OF AMERICA","PORTUGAL","NEW ZEALAND","SOUTH AFRICA","IRELAND"))
        
                     
            
                     
        
         
         
    ),
    dashboardBody(
        titlePanel(h1("Statistics Summary")),
        
        
            #  fluidRow(
            #      infoBox("New Orders", 10 * 2, icon = icon("shopping-cart")),
            #      infoBoxOutput("progressBox"),
            #     infoBoxOutput("approvalBox"),
            #     infoBox("Payment", 10 * 2, icon = icon("credit-card"),color = "green", fill = TRUE),
            #     infoBoxOutput("progressBox2"),
            #     infoBoxOutput("approvalBox2"),
            #     infoBox("New Orders", 10 * 2, icon = icon("thumbs-up"),color = "yellow", fill = TRUE),
            #     infoBoxOutput("progressBox2"),
            #     infoBoxOutput("approvalBox2")
            # 
            # ),
            
            
            fluidRow(
                column(width = 4,
                       
                       box(
                          icon = icon("table") ,title = "Graph", width = NULL, solidHeader = TRUE, status = "primary",
                           plotOutput("coolplot")
                           # br(),br(),
                           # tableOutput("results")
                       )
                       
                      
                       
                ),
                column(width = 8,
                       box(
                           title = "Data", width = NULL, solidHeader = TRUE, status = "primary",
                           
                           # 
                           tableOutput("results")
                        ))
            ),
        
                
        
            
        
       
)
)
server <- function(input, output) { 
    #  output$coolplot <- renderPlot({
    #     filtered <-
    #         bcl %>%
    #         filter(Price >= input$priceInput[1],
    #                Price <= input$priceInput[2],
    #                Type == input$typeInput,
    #                Country == input$countryInput
    #         )
    #     ggplot(filtered, aes(Alcohol_Content)) +
    #         geom_histogram()
    # })
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country",
                    sort(unique(bcl$Country)),
                    selected = "CANADA")
    })  
    
    filtered <- reactive({
        if (is.null(input$countryInput)) {
            return(NULL)
        }    
        
        bcl %>%
            filter(Price >= input$priceInput[1],
                   Price <= input$priceInput[2],
                   Type == input$typeInput,
                   Country == input$countryInput
            )
    })
    
    output$coolplot <- renderPlot({
        if (is.null(filtered())) {
            return()
        }
        ggplot(filtered(), aes(Alcohol_Content)) +
            geom_histogram(colour ="blue")
    })
    
    output$results <- renderTable({
         filtered()
    })
    
    }
shinyApp(ui, server)
# shinyApp(ui = ui, server = server)
