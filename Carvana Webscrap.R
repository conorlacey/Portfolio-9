rm(list = ls())

library(tidyverse)
library(rvest)
library(stringi)
library(glue)
library(robotstxt)

# Data Scrape -------------------------------------------------------------
root <- "https://www.carvana.com/cars/subaru?email-capture=&page="
numbers <- seq(from = 1, to = 46, by = 1)
urls <- glue("{root}{numbers}")

numbers1 <- seq(from = 1, to = 10, by = 1)
numbers2 <- seq(from = 11, to = 20, by = 1)
numbers3 <- seq(from = 21, to = 30, by = 1)
numbers4 <- seq(from = 31, to = 40, by = 1)
numbers5 <- seq(from = 41, to = 46, by = 1)

urls1 <- glue("{root}{numbers1}")
urls2 <- glue("{root}{numbers2}")
urls3 <- glue("{root}{numbers3}")
urls4 <- glue("{root}{numbers4}")
urls5 <- glue("{root}{numbers5}")

scrape_page <- function(url) {

page <- read_html(url)

#Title and year
cars <- page %>% 
  html_nodes(".year-make") %>% 
  html_text()

year <- substring(cars,1,4)

car <- substring(cars, 6, nchar(cars))

#Monthly 
page %>% 
  html_nodes(".middle-frame") %>% 
  html_text()

#Cost 
cost <- page %>% 
  html_nodes(".text-2xl") %>% 
  html_text()
cost

#Miles 
miles <- page %>% 
  html_nodes(".trim-mileage") %>% 
  html_text()

data.frame(car = car, year = year, cost = cost, miles = miles)
}

cars.df.1 <- map_dfr(urls1,scrape_page)
Sys.sleep(15*60)
cars.df.2 <- map_dfr(urls2,scrape_page)
Sys.sleep(15*60)
cars.df.3<- map_dfr(urls3,scrape_page)
Sys.sleep(15*60)
cars.df.4 <- map_dfr(urls4,scrape_page)
Sys.sleep(15*60)
cars.df.5 <- map_dfr(urls5,scrape_page)

cars.df <- bind_rows(cars.df.1, 
                     cars.df.2,
                     cars.df.3,
                     cars.df.4,
                     cars.df.5)

write_rds(cars.df, "cars.RDS")

cars.df <- read_rds("cars.RDS")

cars.df$year <- cars.df$year %>% as.numeric()

cars.df$cost <- cars.df$cost %>% 
  str_sub(2,nchar(cars.df$cost)) %>% 
  str_replace(",", "") %>% 
  as.numeric()

for ( i in 1:906){
  while (str_sub(cars.df$miles[i],1,1) != "•"){ 
    cars.df$miles[i]<-str_sub(cars.df$miles[i],2,nchar(cars.df$miles[i]))
  }
}

cars.df$miles <- cars.df$miles %>% 
  str_replace("•", "") %>% 
  str_replace(",", "") %>% 
  str_replace(" miles", "") %>% 
  str_replace(" ", "") %>% 
  as.numeric()

head(cars.df)
