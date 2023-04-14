library(tidyverse)
library(rvest)
library(tictoc)
library(furrr)


plan(multisession(workers = 5)) #5 workers was maximum for scraping without errors

######DIY PLANS


plans <- read.csv("diy_plans.csv")

links1 <- plans$link[1:1000] #Divided the plans in several batches, because I didn't scrape everything at once.
links2 <- plans$link[1001:3000]
links3 <- plans$link[3001:6000]
links4 <- plans$link[6001:10000]
links5 <- plans$link[10001:15408]


tic()
diy_plans5 <- future_map(links5, function(x) { #Here only the last part was scrapped, adjust accordingly
  page <- read_html(x)
  
  Sys.sleep(rnorm(1, mean = 2, sd = 0.1))
  
  c(
    page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>% #saving number of sales
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>% #Saving number of reviews
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>% #Saving seller rating
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>% #Saving seller name
      html_text() %>%
      str_trim()
  )
})
toc()

#Everything is saved in lists, which we will reshape later

#diy_plans_extra <- c(diy_plans1c, diy_plans2c) Joining all of the smaller batches
saveRDS(diy_plans_extra, "diy_plans_extra.Rdata") #Saving the end list


###### DIY BEDS
beds <- read.csv("diy_beds.csv")
links1 <- beds$link[1:4000]
links2 <- beds$link[4001:9216]

tic()
diy_beds2 <- future_map(links2, function(x) {
  page <- read_html(x)
  
  Sys.sleep(rnorm(1, mean = 2, sd = 0.1))
  
  c(page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>%
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>%
      html_text() %>%
      str_trim()
  )
})
toc()

#diy_beds_extra <- c(diy_beds1, diy_beds2)
saveRDS(diy_beds_extra, "diy_beds_extra.Rdata")



######DIY BUILD PLANS
buildp <- read.csv("diy_build_plans.csv")
links1 <- buildp$link[1:3500]
links2 <- buildp$link[3501:4500]
links3 <- buildp$link[4501:6000]
links4 <- buildp$link[6001:7616]

tic()
diy_buildplans4 <- future_map(links4, function(x) {
  page <- read_html(x)
  
  Sys.sleep(2)
  
  c(page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>%
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>%
      html_text() %>%
      str_trim()
  )
})
toc()


#diy_buildplans_c1 <- c(diy_buildplans1, diy_buildplans2, diy_buildplans3)

#saveRDS(diy_buildplans_c1, "diy_buildplans_extra_1.Rdata")
#diy_buildplans_c1 <- readRDS("diy_buildplans_extra_1.Rdata")

#diy_buildplans_extra <- c(diy_buildplans_c1, diy_buildplans4)

saveRDS(diy_buildplans_extra, "diy_buildplans_extra.Rdata")


######DIY BLUEPRINTS
bluepr <- read.csv("diy_blueprints.csv")
links1 <- bluepr$link[1:3000]
links2 <- bluepr$link[3001:6000]
links3 <- bluepr$link[6001:9000]
links4 <- bluepr$link[9001:13000]
links5 <- bluepr$link[13001:16000]

tic()
diy_blueprints5 <- future_map(links5, function(x) {
  page <- read_html(x)
  
  Sys.sleep(2)
  
  c(page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>%
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>%
      html_text() %>%
      str_trim()
  )
})
toc()


#diy_blueprints_extra_1 <- c(diy_blueprints1, diy_blueprints2, diy_blueprints3, diy_blueprints4)

#diy_blueprints_extra_1 <- readRDS("diy_blueprint_extra_1.Rdata")
#diy_blueprints_extra <- c(diy_blueprints_extra_1, diy_blueprints5)

saveRDS(diy_blueprints_extra, "diy_blueprint_extra.Rdata")


######DIY BLUEPRINT INTERIOR DESIGN
bluepr <- read.csv("diy_blueprint_interior_design.csv")
links <- bluepr$link

tic()
diy_blueprint_interior_design <- future_map(links, function(x) {
  page <- read_html(x)
  
  Sys.sleep(2)
  
  c(page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>%
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>%
      html_text() %>%
      str_trim()
  )
})
toc()

saveRDS(diy_blueprint_interior_design, "diy_blueprint_interior_design.Rdata")

######DIY FURNITURE
furni <- read.csv("diy_furniture.csv")
links1 <- furni$link[1:5000]
links2 <- furni$link[5001:10000]
links3 <- furni$link[10001:15400]

tic()
diy_furniture3 <- future_map(links3, function(x) {
  page <- read_html(x)
  
  Sys.sleep(2)
  
  c(page %>%
      html_nodes(".wt-display-inline-flex-xs span.wt-text-caption") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("h2.wt-mr-xs-2.wt-text-body-03") %>%
      html_text() %>%
      str_trim(),
    
    page %>%
      html_nodes("#reviews .wt-align-items-center .wt-display-inline-block") %>%
      html_text()%>%
      str_trim(),
    
    page %>%
      html_nodes("#listing-page-cart .wt-text-body-01 span") %>%
      html_text() %>%
      str_trim()
  )
})
toc()

#diy_furniture_extra_1 <- c(diy_furniture1, diy_furniture2)

#saveRDS(diy_furniture_extra_1, "diy_furniture_extra_1.Rdata")

#diy_furniture_extra_1 <- readRDS("diy_furniture_extra_1.Rdata")

#diy_furniture_extra <- c(diy_furniture_extra_1, diy_furniture3)

saveRDS(diy_furniture_extra, "diy_furniture_extra.Rdata")
