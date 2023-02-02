library(dplyr)
library(stringr)
library(purrr)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(
  remoteServerAddr = "172.26.224.1",
  port = 4445L
) 

remDr$open()

plani <- data.frame()

for (page_result in 1:3) {
  
  Sys.sleep(2)
  
  link <- paste0("https://www.etsy.com/search?q=interior+plan&ref=pagination&page=", page_result)
  remDr$navigate(link)
  remDr$maxWindowSize()
  
  Sys.sleep(2)
  
  page <- remDr$getPageSource()[[1]]
  page <- read_html(page)
  
  product_name <- page %>% html_nodes("li.wt-list-unstyled h3.wt-text-caption") %>% html_text() %>% str_trim()
  product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()
  product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")
  sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()
  sellerji <- sellerji[seq(2,128, by = 2)]
  
  plani <- rbind(plani, 
                 data.frame(ime = product_name,
                            cena = product_prices,
                            link = product_links,
                            prodajalec = sellerji))
}





remDr$navigate("https://www.etsy.com/search?q=interior+plan&ref=pagination&page=1")
remDr$maxWindowSize()

page <- remDr$getPageSource()[[1]]
page <- read_html(page)


product_name <- page %>%
  html_nodes("li.wt-list-unstyled .v2-listing-card__info h3") %>%
  html_text() %>%
  str_trim()

product_prices <- page %>% html_nodes(".wt-text-title-01 .wt-text-title-01 .currency-value") %>% html_text()

product_links <- page %>% html_nodes("li.wt-list-unstyled div.js-merch-stash-check-listing a.listing-link") %>% html_attr("href")

sellerji <- page %>% html_nodes(".wt-text-caption p") %>% html_text()

sellerji <- sellerji[seq(2,128, by = 2)]
