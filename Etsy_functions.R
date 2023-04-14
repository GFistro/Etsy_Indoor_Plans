
etsy_clean_importdata <- function(x) {
  
  diy_plans2 <- list() #Make a new list
  
  
  for(i in seq_along(x)) {
    diy_plans2[i] <- ifelse(length(x[[i]]) == 0, NA, x[i]) #If there was missing data during scraping, assign NA
  }
  
  #Removing additional info that was scraped with our elements
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "Order placed")
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "Order ships")
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "Delivered!")
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "See details")  
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "^Mar [0-9][0-9]-[0-9][0-9]")
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "^Mar [0-9][0-9]-Apr [0-9]")
  diy_plans2 <- lapply(diy_plans2, str_remove_all, pattern = "^Mar [0-9][0-9]")
  
  
  
  diy_plans2 <- lapply(diy_plans2, stri_remove_empty) #Removing empty leftovers
  
  
  diy_plans2 <- lapply(diy_plans2, \(i) if(length(i) == 3) {c('0 sales', i)}else{i}) #For specific records with missing info we add 0 sales
  
  return(diy_plans2)
}

