library(dplyr)
library(stringr)
library(purrr)
library(rvest)
library(RSelenium)

#Setting up a remote driver for RSelenium
remDr <- remoteDriver(
  remoteServerAddr = "172.29.80.1", #Added the IP address
  port = 4445L
) 

remDr$open()

# Scraping for DIY Plans

plani <- data.frame() #Initialize empty dataframe

for (page_result in 1:250) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=diy+plan&ref=pagination&page=", page_result) #Navigating thorugh pages
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize() #Maximizing window size, so that all the results are displayed
  
  Sys.sleep(3)

   
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page) #Reading the page
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim() #Extracting product name
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text() #Extracting prices
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href") #Extracting the link
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text() #Extracting the seller name, get duplicates
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)] #Remove duplicates
  
  plani <- rbind(plani, 
                 data.frame(ime = product_name,
                            cena = product_prices,
                            link = product_links,
                            prodajalec = sellerji))
}



#Scraping results for DIY Bed

beds <- data.frame()

for (page_result in 1:144) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=diy+bed&ref=pagination&page=", page_result)
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize()
  
  Sys.sleep(3)
  
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)]
  
  beds <- rbind(beds, 
                 data.frame(ime = product_name,
                            cena = product_prices,
                            link = product_links,
                            prodajalec = sellerji))
}




#Scraping results for DIY Furniture

furnitures <- data.frame()

for (page_result in 1:250) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=diy+furniture&ref=pagination&page=", page_result)
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize()
  
  Sys.sleep(3)
  
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)]
  
  furnitures <- rbind(furnitures, 
                data.frame(ime = product_name,
                           cena = product_prices,
                           link = product_links,
                           prodajalec = sellerji))
}


#Scraping results for DIY Blueprint

blueprints <- data.frame()

for (page_result in 1:250) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=DIY+blueprint&ref=pagination&page=", page_result)
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize()
  
  Sys.sleep(3)
  
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)]
  
  blueprints <- rbind(blueprints, 
                      data.frame(ime = product_name,
                                 cena = product_prices,
                                 link = product_links,
                                 prodajalec = sellerji))
}


#Scraping results for Build plan

build_plans <- data.frame()

for (page_result in 1:119) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=build+plan&ref=pagination&page=", page_result)
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize()
  
  Sys.sleep(3)
  
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)]
  
  build_plans <- rbind(build_plans, 
                      data.frame(ime = product_name,
                                 cena = product_prices,
                                 link = product_links,
                                 prodajalec = sellerji))
}

#Scraping results for Blueprint interior design

blueprint_id <- data.frame()

for (page_result in 1:17) {
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=blueprint+interior+design&ref=pagination&page=", page_result)
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  remDr$maxWindowSize()
  
  Sys.sleep(3)
  
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(2)
  
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,length(sellerji), by = 2)]
  
  blueprint_id <- rbind(blueprint_id, 
                       data.frame(ime = product_name,
                                  cena = product_prices,
                                  link = product_links,
                                  prodajalec = sellerji))
}


remDr$close() #Closing the remote driver

#Saving the results to csv files

write.csv(plani, file = "diy_plans.csv")
write.csv(beds, file = "diy_beds.csv")
write.csv(furnitures, file = "diy_furniture.csv")
write.csv(blueprints, file = "diy_blueprints.csv")
write.csv(build_plans, file = "diy_build_plans.csv")
write.csv(blueprint_id, file = "diy_blueprint_interior_design.csv")
