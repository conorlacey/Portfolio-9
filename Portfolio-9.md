Portfolio-9
================
Conor Lacey
2023-04-18

``` r
suppressWarnings(library(tidyverse))
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
suppressWarnings(suppressMessages(library(rvest)))
suppressWarnings(library(stringi))
suppressWarnings(library(glue))
suppressWarnings(library(robotstxt))
```

### Introduction

Ok so I need to buy a new car so I’m going to scrape some data from
carvana! Specifically I’m looking for a subaru so I will get the link to
that specific search space from the website.
<https://www.carvana.com/cars/subaru?email-capture=&page=1>

### Make Urls

First I need to write a function to scrape one page.

``` r
url <- "https://www.carvana.com/cars/subaru?email-capture=&page=1"

scrape_page <- function(url) {

page <- read_html(url)

#Title and year
cars <- page %>% 
  html_nodes(".year-make") %>% 
  html_text()

year <- substring(cars,1,4) %>% as.numeric()

car <- substring(cars, 6, nchar(cars))

#Monthly 
page %>% 
  html_nodes(".middle-frame") %>% 
  html_text()

#Cost 
cost <- page %>% 
  html_nodes(".text-2xl") %>% 
  html_text()

#Miles 
miles <- page %>% 
  html_nodes(".trim-mileage") %>% 
  html_text()

data.frame(car = car, year = year, cost = cost, miles = miles)
}

head(scrape_page(url))
```

    ##                car year    cost                       miles
    ## 1 Subaru Crosstrek 2016 $23,590 2.0i Limited • 51,811 miles
    ## 2    Subaru Ascent 2022 $39,990 Onyx Edition • 12,462 miles
    ## 3  Subaru Forester 2022 $33,590       Premium • 3,588 miles
    ## 4 Subaru Crosstrek 2019 $28,990 2.0i Limited • 23,356 miles
    ## 5  Subaru Forester 2017 $22,990 2.5i Limited • 78,471 miles
    ## 6       Subaru BRZ 2018 $30,590      Limited • 23,341 miles

### Scrape Multiple URLS

Ok now we have a function that will scrape a page. However, now let’s
scrape all 46.

``` 46
root <- "https://www.carvana.com/cars/subaru?email-capture=&page="
numbers <- seq(from = 1, to = 46, by = 1)
urls <- glue("{root}{numbers}")

cars.df <- map_dfr(urls,scrape_page) #This will bind the data from each page
```

Uh oh? Houston we have a problem. If you look up this error “HTTP error
429”, you will find we are having issues with being rate limited. Rate
limiting is preventing our bot from operating as it is exceeding some
constraint. The constraint is network traffic or in other words we are
going through too many pages at a time on Carvana’s website.

### Scrape One At a Time

There a few ways to solve this and perhaps the most efficient way would
be to make an API request of their car inventory. However, they
unfortunately do not make this available so we will go with the waiting
around approach. What i mean by this is we can simply just pause the
programming until the rate limit resets. I suspect this one takes about
15 minutes and I also suspect it’ll begin to rate limit after scrapping
about 20 pages.

Therefore, we shall write a program that scrapes 20 pages at a time and
will pause for 15 minutes after scraping 20 pages to allow for the rate
limit to reset. This may seem inefficient, but we are only wanting 46
pages and personally this is fine by me if I wait about 30 minutes. I’ll
just answer some emails while this runs.

``` one
umbers1 <- seq(from = 1, to = 10, by = 1)
numbers2 <- seq(from = 11, to = 20, by = 1)
numbers3 <- seq(from = 21, to = 30, by = 1)
numbers4 <- seq(from = 31, to = 40, by = 1)
numbers5 <- seq(from = 41, to = 46, by = 1)

urls1 <- glue("{root}{numbers1}")
urls2 <- glue("{root}{numbers2}")
urls3 <- glue("{root}{numbers3}")
urls4 <- glue("{root}{numbers4}")
urls5 <- glue("{root}{numbers5}")

cars.df.1 <- map_dfr(urls1,scrape_page)
cars.df.2 <- map_dfr(urls2,scrape_page)
Sys.sleep(15*60) #This will pause the programming for 15 minutes
cars.df.3<- map_dfr(urls3,scrape_page)
cars.df.4 <- map_dfr(urls4,scrape_page)
Sys.sleep(15*60) #This will pause the programming for 15 minutes
cars.df.5 <- map_dfr(urls5,scrape_page)

#Bind the data 
cars.df <- bind_rows(cars.df.1, 
                     cars.df.2,
                     cars.df.3,
                     cars.df.4,
                     cars.df.5)
#Save the data file
write_rds(cars.df, "cars.RDS")
```

### Final Edits

Ok now we have a our data. However, the data frame still needs some
editing. Specifically the miles column.

``` r
cars.df <- read_rds("cars.RDS")
head(cars.df)
```

    ##                car year    cost                       miles
    ## 1 Subaru Crosstrek 2016 $23,990 2.0i Limited • 51,811 miles
    ## 2   Subaru Impreza 2021 $21,990         base • 12,011 miles
    ## 3 Subaru Crosstrek 2019 $28,990 2.0i Limited • 23,356 miles
    ## 4  Subaru Forester 2017 $22,990 2.5i Limited • 78,471 miles
    ## 5   Subaru Outback 2018 $21,590 2.5i Premium • 95,221 miles
    ## 6       Subaru WRX 2019 $29,590  WRX Premium • 32,051 miles

I don’t want the entire string, I just want to extract the actual miles
on the vehicle. To do this we will include all characters beyond “•”.

``` r
cars.df$miles <- cars.df$miles %>% 
  str_sub(str_locate(cars.df$miles, "•")[,1]+2)

head(cars.df)
```

    ##                car year    cost        miles
    ## 1 Subaru Crosstrek 2016 $23,990 51,811 miles
    ## 2   Subaru Impreza 2021 $21,990 12,011 miles
    ## 3 Subaru Crosstrek 2019 $28,990 23,356 miles
    ## 4  Subaru Forester 2017 $22,990 78,471 miles
    ## 5   Subaru Outback 2018 $21,590 95,221 miles
    ## 6       Subaru WRX 2019 $29,590 32,051 miles

Perfect now let’s get rid of all the characters I do not want and make
this column numeric.

We shall do the same thing for column cost.

``` r
cars.df$miles <- cars.df$miles %>% 
  str_replace(",", "") %>% 
  str_replace(" miles", "") %>% 
  as.numeric()

cars.df$cost <- cars.df$cost %>% 
  str_sub(2,) %>% 
  str_replace(",", "") %>% 
  as.numeric()

head(cars.df)
```

    ##                car year  cost miles
    ## 1 Subaru Crosstrek 2016 23990 51811
    ## 2   Subaru Impreza 2021 21990 12011
    ## 3 Subaru Crosstrek 2019 28990 23356
    ## 4  Subaru Forester 2017 22990 78471
    ## 5   Subaru Outback 2018 21590 95221
    ## 6       Subaru WRX 2019 29590 32051

Perfect! Now I have my subaru data set!
