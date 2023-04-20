library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(ggplot2)


etsy <- read.csv("etsy_clink.csv")

ui <- dashboardPage(
    dashboardHeader(title = "Etsy DIY Dashboard"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Seller Selection",
                 tabName = "selsel",
          selectizeInput("izbira", "Choose your sellers", choices = NULL, multiple = TRUE)
       ),
        menuItem("Table Tab",
                 tabName = "table_tab"),
        menuItem("Output Tab",
                 tabName = "output_tab")
      )),


    
    dashboardBody(
      tabItems(
        tabItem(tabName = "table_tab",
                fluidRow(box(textInput(inputId = "sr_in", 
                                label = "Search for text"))),
                fluidRow(box(dataTableOutput("shiny_table")))),
        tabItem(tabName = "output_tab",
                box(plotOutput("price_hist")))
        )
      )
    )

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "izbira", choices = etsy$seller_name, server = TRUE)
  
  
  
  output$shiny_table <- renderDataTable({
    
    if (is.null(input$izbira)) {
      
      etsy1 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T))
      
      datatable(etsy1, escape = F)
    } else {
      
      etsy2 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T))
      
      datatable(etsy2, escape = F)
    }
    
  })

  

  output$price_hist <- renderPlot({
    
    if (is.null(input$izbira) & is.null(input$sr_in)) {
      
      sell_choices <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        select(price_EUR) %>%
        log()
      
      sell_choices <- sell_choices[, ]
      
      sell_choices_me <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        select(price_EUR) %>%
        log() %>%
        summarise(mean(price_EUR))
      
      sell_choices_me <- sell_choices_me[, ]
      
      etsy %>%
        ggplot(aes(x = log(price_EUR))) +
        geom_histogram() +
        geom_vline(xintercept = sell_choices, color = "red", size = 0.5, alpha = 0.5) +
        geom_vline(xintercept = sell_choices_me, color = "blue", size = 1)
      
    } else {
    
    sell_choices2 <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
      select(price_EUR) %>%
      log()
    
    sell_choices2 <- sell_choices2[, ]
    
    sell_choices_me2 <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
      select(price_EUR) %>%
      log() %>%
      summarise(mean(price_EUR))
    
    sell_choices_me2 <- sell_choices_me2[, ]
    
    etsy %>%
    ggplot(aes(x = log(price_EUR))) +
    geom_histogram() +
    geom_vline(xintercept = sell_choices2, color = "red", size = 0.5, alpha = 0.5) +
    geom_vline(xintercept = sell_choices_me2, color = "blue", size = 1)
    
    }
  })
  
}

shinyApp(ui = ui, server = server)


