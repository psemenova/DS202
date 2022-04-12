library(tidyverse)
data(nasa, package="GGally")
nasa %>% filter(id=="1-1") %>%
  ggplot(aes(x = time, y= temperature)) + geom_point()

nasa %>% filter(id=="1-1") %>%
  ggplot(aes(x = time, y= temperature)) + geom_point() +
  geom_smooth(method="lm")

nasa %>% filter(id=="1-1") %>%
  ggplot(aes(x = time, y= temperature)) + geom_point() +
  geom_smooth(method="lm") +
  geom_text(aes(x = time, y= temperature + .1, label=temperature), colour="darkorange", 
            data=nasa %>% filter(id=="1-1", time==50))

nasa %>% filter(id=="1-1") %>%
  ggplot(aes(x = time, y= temperature)) + geom_point() +
  geom_smooth(method="lm") +
  geom_text(aes(x = time, y= temperature, label=id, colour=id),  
            data=nasa %>% filter(id=="1-1", time==50))

nasa %>% filter(id=="1-1") %>%
  ggplot(aes(x = time, y= temperature, colour=id)) + geom_point() +
  geom_smooth(method="lm") +
  geom_text(aes(x = time, y= temperature, label=id), 
            data=nasa %>% filter(id=="1-1", time==50))


data(box, package="classdata")
box.summary = box %>% group_by(Movie, Distributor) %>% 
  summarise(
    max.date = max(Date),
    max.week = max(Week),
    max.total.gross = max(Total.Gross, na.rm = TRUE),
    first.week.gross = min(Total.Gross, na.rm = TRUE) # problem, min and max are the same if we say Total.Gross = max(Total.Gross)
  )

# Plot a time line for each movie: plot total gross by week that the movie is out
# Plot a time line for each movie: plot total gross by week that the movie is out
# Color the label of these three movies with a color of your choice.

box %>% filter(Thtrs. > 100, Week < 50) %>%
  ggplot(aes(x = Week, y = Total.Gross, 
             group = interaction(Movie, Distributor))) + 
  geom_line()

box.summary1 = box.summary %>% arrange(desc(max.total.gross)) %>% head(3)
box1 = box %>% filter(Movie %in% box.summary1$Movie) 

box %>% filter(Thtrs. > 100, Week < 50) %>%
  ggplot(aes(x = Week, y = Total.Gross, 
             group = interaction(Movie, Distributor))) + 
  geom_line() +
  geom_text(aes(x = max.week, y = max.total.gross, label = Movie, color = Movie), data = box.summary1) +
  geom_line(aes(x = Week, y = Total.Gross, group = Movie, color = Movie), data = box1)



