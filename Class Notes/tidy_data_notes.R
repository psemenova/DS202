vignette("tidy-data", package="tidyr")
library(tidyr)
library(dplyr)
library(classdata)
library(ggplot2)
data(french_fries, package="reshape2")

# Gather - take multiple columns and collapse into key-value pairs 
ffm <- french_fries %>% gather(key = scale, value = score, 5:9)
ffm %>% head()

# Spread - a key-value pair across multiple columns
ffm %>% spread(key = rep, value = score)
ffm %>% spread(key = rep, value = score) %>%
  ggplot(aes(x = `1`, y = `2`)) + geom_point() +
  facet_wrap(~scale) + geom_abline(colour = "grey50")

# Separate - one column into multiple columns 


### Example: Box office gross 
head(box, 4)
# key variables = movie & distributor

box.movie = box %>% select(Movie, Distributor, Week, Date) # or box.movie = box[,c(3, 4, 10,11)]

## count the number of recordings for the same movie 
box.movie %>% group_by(Movie) %>% summarise(n = n()) %>% arrange(desc(n))
box.movie %>% filter(Movie == "The Lion King")

movies = box %>% select(Movie, Distributor) %>% unique()
#extract the smallest date and the number of weeks it has been run
movies <- box %>% group_by(Movie, Distributor) %>%
  summarise(
    firstDate = Date[which.min(Week)], ## which.min -- gets the location of the smallest value
    firstWeek = min(Week, na.rm=TRUE), # get the
    theater = Thtrs.[which.min(Week)],
    total.gross = max(Total.Gross, na.rm=TRUE)) %>% arrange(desc(total.gross))

# Messy 


