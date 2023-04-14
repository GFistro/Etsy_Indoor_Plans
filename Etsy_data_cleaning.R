#Etsy Interior design blueprints data wrangling

library(tidyverse)
library(stringi)
library(data.table)


## Imports
diy_plans <- readRDS("diy_plans_extra.Rdata") #reading the scraped data in list form
diy_plans_clean <- etsy_clean_importdata(diy_plans) #Cleaning the data with our function
plans_data <- read.csv("plans_data.csv") #Reading the original data
diy_plans_clean <- transpose(diy_plans_clean) #Transposing the list
plans_sales <- unlist(diy_plans_clean[1]) #Extracting out components
plans_reviews <- unlist(diy_plans_clean[2])
plans_ratings <- unlist(diy_plans_clean[3])
plans_sellers <- unlist(diy_plans_clean[4])
diy_plans_complete <- cbind(plans_data, plans_sales, plans_reviews, plans_ratings, plans_sellers) #Binding the components together

diy_plans_complete <- diy_plans_complete %>%
  select(-c(X, sales, num_reviews, star_rating)) #Removing duplicated data

write.csv(diy_plans_complete, "diy_plans_complete.csv") #Saving the final dataset



#DIY Beds
diy_beds <- readRDS("diy_beds_extra.Rdata")
diy_beds_clean <- etsy_clean_importdata(diy_beds)

beds_data <- read.csv("beds_data.csv")
diy_beds_clean <- transpose(diy_beds_clean)

beds_sales <- unlist(diy_beds_clean[1])
beds_reviews <- unlist(diy_beds_clean[2])
beds_ratings <- unlist(diy_beds_clean[3])
beds_sellers <- unlist(diy_beds_clean[4])
diy_beds_complete <- cbind(beds_data, beds_sales, beds_reviews, beds_ratings, beds_sellers)

diy_beds_complete <- diy_beds_complete %>%
  select(-c(X, sales, num_reviews, star_rating))

write.csv(diy_beds_complete, "diy_beds_complete.csv")


#DIY Buildplans
diy_buildplans <- readRDS("diy_buildplans_extra.Rdata")
diy_buildplans_clean <- etsy_clean_importdata(diy_buildplans)

buildplans_data <- read.csv("build_plans_data.csv")
diy_buildplans_clean <- transpose(diy_buildplans_clean)

buildplans_sales <- unlist(diy_buildplans_clean[1])
buildplans_reviews <- unlist(diy_buildplans_clean[2])
buildplans_ratings <- unlist(diy_buildplans_clean[3])
buildplans_sellers <- unlist(diy_buildplans_clean[4])

diy_buildplans_complete <- cbind(buildplans_data, buildplans_sales, buildplans_reviews, buildplans_ratings, buildplans_sellers)

diy_buildplans_complete <- diy_buildplans_complete %>%
  select(-c(X, sales, num_reviews, star_rating))

write.csv(diy_buildplans_complete, "diy_buildplans_complete.csv")



#DIY BLueprint
diy_blueprint <- readRDS("diy_blueprint_extra.Rdata")
diy_blueprint_clean <- etsy_clean_importdata(diy_blueprint)

diy_blueprint_clean[lengths(diy_blueprint_clean) == 2] #One entry has a length of only 2
which(sapply(diy_blueprint_clean, length) == 2) #entry no 1016
diy_blueprint_clean[[1016]][2] <- "82 reviews" #We add the missing information manually as it is available from other records of same seller
diy_blueprint_clean[[1016]][3] <- "4.5 out of 5 stars"
diy_blueprint_clean[[1016]][4] <- "MakerWorksLLC"

blueprint_data <- read.csv("blueprints_data.csv")
diy_blueprint_clean <- transpose(diy_blueprint_clean)

blueprint_sales <- unlist(diy_blueprint_clean[1])
blueprint_reviews <- unlist(diy_blueprint_clean[2])
blueprint_ratings <- unlist(diy_blueprint_clean[3])
blueprint_sellers <- unlist(diy_blueprint_clean[4])

diy_blueprint_complete <- cbind(blueprint_data, blueprint_sales, blueprint_reviews, blueprint_ratings, blueprint_sellers)

diy_blueprints_complete <- diy_blueprint_complete %>%
  select(-c(X, sales, num_reviews, star_rating))

write.csv(diy_blueprints_complete, "diy_blueprints_complete.csv")



#DIY Furniture
diy_furniture <- readRDS("diy_furniture_extra.Rdata")
diy_furniture_clean <- etsy_clean_importdata(diy_furniture)
#Applying extra cleaning steps, as the function didn't clean some instances
diy_furniture_clean <- lapply(diy_furniture_clean, str_remove_all, pattern = "CZK [0-9]\\,[0-9][0-9][0-9]\\.[0-9][0-9]\\+ per meter")
diy_furniture_clean <- lapply(diy_furniture_clean, str_remove_all, pattern = "CZK [0-9][0-9][0-9]\\.[0-9][0-9]\\+ per meter")
diy_furniture_clean <- lapply(diy_furniture_clean, str_remove_all, pattern = "CZK [0-9][0-9][0-9]\\.[0-9][0-9]\\ per meter")

diy_furniture_clean <- lapply(diy_furniture_clean, stri_remove_empty)

which(sapply(diy_furniture_clean, length) == 2) #Same as before, this time it's record 9782
diy_furniture_clean[9782] <- diy_furniture_clean[9783] #We substitute it with a record of the same seller

diy_furniture_clean <- transpose(diy_furniture_clean)

furniture_data <- read.csv("furniture_data.csv")

furniture_sales <- unlist(diy_furniture_clean[1])
furniture_reviews <- unlist(diy_furniture_clean[2])
furniture_ratings <- unlist(diy_furniture_clean[3])
furniture_sellers <- unlist(diy_furniture_clean[4])

diy_furniture_complete <- cbind(furniture_data, furniture_sales, furniture_reviews, furniture_ratings, furniture_sellers)

diy_furniture_complete <- diy_furniture_complete %>%
  select(-c(X, sales, num_reviews, star_rating))

write.csv(diy_furniture_complete, "diy_furniture_complete.csv")

