library(lubridate)

# Converter functions and instants in time
mdy("03-01-2018") + 1 # get the next date # mdy - month day year - input order
# date: ymd, mdy, dmy, …
# time: hm, hms, …
# date & time: ymd_hms, mdy_hm,

ymd("22-4-4")

# Accessor functions 
# - year, month, week, wday, mday, yday, hour, minute, ... 
month(now()) #current month
wday(now(), label = TRUE) # current day
year(now()) #current year

# Intervals and Durations
now() + years(1) # give us next year with the rest of the data the same
end_date <- now() 
span <- end_date - years(1)
span 

end_date - days(10)

# Example: Movies
library(classdata)
library(tidyverse)
library(lubridate)

summary(box$Date) # date variable: allows date calculus
box %>% ggplot(aes(x = Date)) + geom_histogram(binwidth=7)
# Is there a seasonal effect in the number of movies in the box office?
box %>% ggplot(aes(x = month(Date, label=TRUE))) + geom_bar()

# inspect the budget data set from the classdata package
View(budget)
# make sure the variable Release Date is a date format
str(budget) 
# plot a histogram of the variable
budget %>% ggplot(aes(x = ReleaseDate)) + geom_histogram()
budget %>% ggplot(aes(x = month(ReleaseDate, label=TRUE))) + geom_bar()
# merge (join) budget and box office data (by movie name)
dim(box)
dim(budget)
box_budget_1 = left_join(box, budget, by = "Movie")
dim(anti_join(box, budget, by = "Movie"))
dim(anti_join(budget, box, by = "Movie"))
box_budget = inner_join(box, budget, by = "Movie")
# is the time between the release of a movie and the date is equal to the number of weeks in theaters?

# box_budget$Diff = week(box_budget$Date) - week(box_budget$ReleaseDate)  # a good strategy? = no
box_budget$Diff = as.duration(box_budget$Date - box_budget$ReleaseDate) / dweeks(1)


#  -------------------------------------
# VISUALIZING TIME: TIME SERIES 

data(nasa, package = "GGally")
head(nasa)


nasa %>% filter(x == 1, y == 1) %>%
  ggplot(aes(x = time, y = temperature)) + geom_point()

nasa %>% filter(x == 1, y == 1) %>%
  ggplot(aes(x = time, y = temperature)) + geom_line()

nasa %>% filter(x == 1, y %in% c(1, 10)) %>%
  ggplot(aes(x = time, y = temperature, group=id, color = id)) + geom_line()


