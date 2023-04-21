library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(ggplot2)

etsy <- read.csv("etsy_clink.csv") %>%
  select(-X)


ui <- dashboardPage(
    dashboardHeader(title = "Etsy DIY Dashboard"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Table Tab",
                 tabName = "table_tab"),
        menuItem("Output Tab",
                 tabName = "output_tab")
      )),


    
    dashboardBody(
      tabItems(
        tabItem(tabName = "table_tab",
                fluidRow(box(uiOutput("seller_selector")),
                         box(uiOutput("word_search"))),
                fluidRow(box(dataTableOutput("shiny_table"), width = 12))),
        tabItem(tabName = "output_tab",
                box(plotOutput("price_hist"), width = 12),
                fluidRow(box(valueBoxOutput("number_sellers"), valueBoxOutput("sellers_total"), valueBoxOutput("number_sales"), width = 12)),
                fluidRow(box(valueBoxOutput("number_products"), valueBoxOutput("products_average"), valueBoxOutput("products_range"), width = 12))
        )
      )
    )
)

server <- function(input, output, session) {

  output$seller_selector <- renderUI({
    
    selectizeInput(inputId = "izbira",
                   label = "Choose your sellers",
                   choices = etsy %>%
                     select(seller_name) %>%
                     distinct() %>%
                     arrange(seller_name),
                   multiple = TRUE)
  })
  
  output$word_search <- renderUI({
    
    textInput(inputId = "sr_in", 
              label = "Search for text")
    
  })
  
  output$shiny_table <- renderDataTable({
    
    if (is.null(input$izbira)) {
      
      etsy1 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T))
      
      datatable(etsy1, escape = F, options = list(searching = F))
    } else {
      
      etsy2 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T))
      
      datatable(etsy2, escape = F, options = list(searching = F))
    }
    
  })

  output$price_hist <- renderPlot({
    
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      etsy %>%
        ggplot(aes(x = log(price_EUR))) +
        geom_histogram(bins = 50) +
        scale_x_continuous(breaks = c(-2.3, 0, log(5), log(10), log(20), log(50), log(100), log(300), log(500)),
                           labels = c("0.1", "1", "5", "10", "20", "50", "100", "300", "500")) +
        ggtitle("Price Distribution",
                subtitle = "X axis is log-transformed, but translated back to original values") +
        xlab("Price in EUR") +
        ylab("Number of items") +
        theme_bw()
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      sell_choices <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      select(price_EUR) %>%
      log()
    
    sell_choices <- sell_choices[, ]
    
    sell_choices_me <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      select(price_EUR) %>%
      log() %>%
      summarise(mean(price_EUR))
    
    sell_choices_me <- sell_choices_me[, ]
    
    etsy %>%
    ggplot(aes(x = log(price_EUR))) +
    geom_histogram(bins = 50) +
      scale_x_continuous(breaks = c(-2.3, 0, log(5), log(10), log(20), log(50), log(100), log(300), log(500)),
                         labels = c("0.1", "1", "5", "10", "20", "50", "100", "300", "500")) +
    geom_vline(xintercept = sell_choices, color = "red", size = 0.5, alpha = 0.5) +
    geom_vline(xintercept = sell_choices_me, color = "blue", size = 1) +
      ggtitle("Price Distribution",
              subtitle = "X axis is log-transformed, but translated back to original values") +
      xlab("Price in EUR") +
      ylab("Number of items") +
      theme_bw()

    
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      sell_choices2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        select(price_EUR) %>%
        log()
      
      sell_choices2 <- sell_choices2[, ]
      
      sell_choices_me2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        select(price_EUR) %>%
        log() %>%
        summarise(mean(price_EUR))
      
      sell_choices_me2 <- sell_choices_me2[, ]
      
      etsy %>%
        ggplot(aes(x = log(price_EUR))) +
        geom_histogram(bins = 50) +
        scale_x_continuous(breaks = c(-2.3, 0, log(5), log(10), log(20), log(50), log(100), log(300), log(500)),
                           labels = c("0.1", "1", "5", "10", "20", "50", "100", "300", "500")) +
        geom_vline(xintercept = sell_choices2, color = "red", size = 0.5, alpha = 0.5) +
        geom_vline(xintercept = sell_choices_me2, color = "blue", size = 1) +
        ggtitle("Price Distribution",
                subtitle = "X axis is log-transformed, but translated back to original values") +
        xlab("Price in EUR") +
        ylab("Number of items") +
        theme_bw()
      
    } else {
      
    sell_choices3 <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
      select(price_EUR) %>%
      log()
    
    sell_choices3 <- sell_choices3[, ]
    
    sell_choices_me3 <- etsy %>%
      filter(seller_name %in% input$izbira) %>%
      filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
      select(price_EUR) %>%
      log() %>%
      summarise(mean(price_EUR))
    
    sell_choices_me3 <- sell_choices_me3[, ]
    
    etsy %>%
    ggplot(aes(x = log(price_EUR))) +
    geom_histogram(bins = 50) +
      scale_x_continuous(breaks = c(-2.3, 0, log(5), log(10), log(20), log(50), log(100), log(300), log(500)),
                         labels = c("0.1", "1", "5", "10", "20", "50", "100", "300", "500")) +
    geom_vline(xintercept = sell_choices3, color = "red", size = 0.5, alpha = 0.5) +
    geom_vline(xintercept = sell_choices_me3, color = "blue", size = 1) +
      ggtitle("Price Distribution",
              subtitle = "X axis is log-transformed, but translated back to original values") +
      xlab("Price in EUR") +
      ylab("Number of items") +
      theme_bw()
    
    }
  })
  
  output$number_sellers <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      num_sel <- etsy %>%
        distinct(seller_name) %>%
        nrow()
      
      valueBox(
        value = num_sel,
        subtitle = "Number of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      num_sel1 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        distinct(seller_name) %>%
        nrow()
      
      valueBox(
        value = num_sel1,
        subtitle = "Number of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      num_sel2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        distinct(seller_name) %>%
        nrow()
      
      valueBox(
        value = num_sel2,
        subtitle = "Number of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
    } else {
      
      num_sel3 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        distinct(seller_name) %>%
        nrow()
      
      valueBox(
        value = num_sel3,
        subtitle = "Number of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
    }
    
    
    
  })
  output$sellers_total <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      sel_tot <- etsy %>%
        group_by(seller_name) %>%
        summarise(tot = mean(seller_revenue_EUR)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = paste(sel_tot, "€", sep = ""),
        subtitle = "Total Revenue of selected Sellers",
        color = "red",
        icon = icon("box")
      )
        
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      sel_tot2 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(seller_revenue_EUR)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = paste(sel_tot2, "€", sep = ""),
        subtitle = "Total Revenue of selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      sel_tot3 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(seller_revenue_EUR)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = paste(sel_tot3, "€", sep = ""),
        subtitle = "Total Revenue of selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
    } else {
      
      sel_tot4 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(seller_revenue_EUR)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = paste(sel_tot4, "€", sep = ""),
        subtitle = "Total Revenue of selected Sellers",
        color = "red",
        icon = icon("box")
      )
    }
  })
  output$number_sales <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      sel_tot <- etsy %>%
        group_by(seller_name) %>%
        summarise(tot = mean(num_sales)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = round(sel_tot, 0),
        subtitle = "Number of Sales of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      sel_tot2 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(num_sales)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = round(sel_tot2, 0),
        subtitle = "Number of Sales of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      sel_tot3 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(num_sales)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = round(sel_tot3, 0),
        subtitle = "Number of Sales of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
      
    } else {
      
      sel_tot4 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        group_by(seller_name) %>%
        summarise(tot = mean(num_sales)) %>%
        summarise(st = sum(tot)) %>%
        ungroup()
      
      valueBox(
        value = round(sel_tot4, 0),
        subtitle = "Number of Sales of Selected Sellers",
        color = "red",
        icon = icon("box")
      )
    }
  })
  output$number_products <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      num_prod <- etsy %>%
        summarise(all = n())
      
      valueBox(
        value = num_prod,
        subtitle = "Number of Products",
        color = "blue",
        icon = icon("coins")
      )
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      num_prod1 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        summarise(all = n())
      
      valueBox(
        value = num_prod1,
        subtitle = "Number of Products",
        color = "blue",
        icon = icon("coins")
      )
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      num_prod2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        summarise(all = n())
      
      valueBox(
        value = num_prod2,
        subtitle = "Number of Products",
        color = "blue",
        icon = icon("coins")
      )
      
    } else {
      
      num_prod3 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        summarise(all = n())
      
      valueBox(
        value = num_prod3,
        subtitle = "Number of Products",
        color = "blue",
        icon = icon("coins")
      )
    }
    
    
    
  })
  output$products_average <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      avg_prc <- etsy %>%
        summarise(tot = mean(price_EUR))
      
      valueBox(
        value = paste(round(avg_prc, 1), "€", sep = ""),
        subtitle = "Average Price of Products",
        color = "blue",
        icon = icon("coins")
      )
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      avg_prc1 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        summarise(tot = mean(price_EUR))
      
      valueBox(
        value = paste(round(avg_prc1, 1), "€", sep = ""),
        subtitle = "Average Price of Products",
        color = "blue",
        icon = icon("coins")
      )
      
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      avg_prc2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        summarise(tot = mean(price_EUR))
      
      valueBox(
        value = paste(round(avg_prc2, 1), "€", sep = ""),
        subtitle = "Average Price of Products",
        color = "blue",
        icon = icon("coins")
      )

    } else {
      
      avg_prc3 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        summarise(tot = mean(price_EUR))
      
      valueBox(
        value = paste(round(avg_prc3, 1), "€", sep = ""),
        subtitle = "Average Price of Products",
        color = "blue",
        icon = icon("coins")
      )
    }
    
    
    
  })
  output$products_range <- renderValueBox({
    
    if (is.null(input$izbira) & input$sr_in == "") {
      
      rng_prc <- etsy %>%
        reframe(tot = range(price_EUR))
      
      valueBox(
        value = paste(paste(round(rng_prc$tot[1], 1), "€", sep = ""), " - ", paste(round(rng_prc$tot[2], 1), "€", sep = "")),
        subtitle = "Price Range of Products",
        color = "blue",
        icon = icon("coins")
      )
      
      
    } else if (!is.null(input$izbira) & input$sr_in == "") {
      
      rng_prc1 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        reframe(tot = range(price_EUR))
      
      valueBox(
        value = paste(paste(round(rng_prc1$tot[1], 1), "€", sep = ""), " - ", paste(round(rng_prc1$tot[2], 1), "€", sep = "")),
        subtitle = "Price Range of Products",
        color = "blue",
        icon = icon("coins")
      )
      
      
    } else if (is.null(input$izbira) & input$sr_in != "") {
      
      rng_prc2 <- etsy %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        reframe(tot = range(price_EUR))
      
      valueBox(
        value = paste(paste(round(rng_prc2$tot[1], 1), "€", sep = ""), " - ", paste(round(rng_prc2$tot[2], 1), "€", sep = "")),
        subtitle = "Price Range of Products",
        color = "blue",
        icon = icon("coins")
      )
      
    } else {
      
      rng_prc3 <- etsy %>%
        filter(seller_name %in% input$izbira) %>%
        filter(grepl(as.character(input$sr_in), product_name, ignore.case = T)) %>%
        reframe(tot = range(price_EUR))
      
      valueBox(
        value = paste(paste(round(rng_prc3$tot[1], 1), "€", sep = ""), " - ", paste(round(rng_prc3$tot[2], 1), "€", sep = "")),
        subtitle = "Price Range of Products",
        color = "blue",
        icon = icon("coins")
      )
    }
    
    
    
  })
}

shinyApp(ui = ui, server = server)

