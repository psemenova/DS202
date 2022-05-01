library(rvest)
library(tidyverse)

url <- "https://www.the-numbers.com/weekend-box-office-chart"
html <- read_html(url) # 'read_html' gets all the information from a URL
html

tables <- html %>% html_table(fill=TRUE) # extracts all tables from the sources html inta a list of data frames
length(tables)
tables[[1]]
A = tables[[2]] # get the second data frame in tables 
names(A)
names(A)[1:2] <- c("Rank", "Rank.Last.Week")
str(A)

A = A %>% mutate(TotalGross = parse_number(TotalGross),
                 PerTheater = parse_number(PerTheater),
                 Theaters = parse_number(Theaters),
                 Gross = parse_number(Gross))

