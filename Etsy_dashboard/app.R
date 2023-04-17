library(shiny)

etsy <- read.csv("etsy_diy_clean.csv")

# Define UI for app with table 
ui <- fluidPage(
  
            selectInput("filcat",
                        "Category to filter with",
                        colnames(etsy)),
            
            textOutput("greeting"),
            dataTableOutput("table")
)

# Define server 
server <- function(input, output) {

    output$greeting <- renderText({input$filcat})
    
    
    output$table <- renderDataTable(
      etsy %>%
        arrange(desc(!!rlang::sym(input$filcat))) %>%
        head(10)
    )
}

# Run the app
shinyApp(ui = ui, server = server)


