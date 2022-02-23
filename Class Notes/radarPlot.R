library(tidyverse)
library(classdata)
library(reshape2)


head(iris)


mean.iris <-
  iris %>% 
  group_by(Species) %>% 
  summarise_all("mean") %>% 
  melt()

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(mean.iris, aes(x = variable, y = value)) +
  geom_polygon(aes(group = Species, color = Species), fill = NA, size = 2) +
  geom_line(aes(group = Species, color = Species), size = 2) +
  facet_wrap(~ Species) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = "none") +
  coord_radar()


ggplot(mean.iris, aes(x = variable, y = value)) +
  geom_polygon(aes(group = Species, color = Species), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = Species, color = Species), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()


#pipe operator

#two ways to graph the same thing
ggplot(data = filter(fbi, Type=="Murder"), 
       aes(x = Year, y = Count)) + geom_point()

fbi %>%  #pipe operator
  filter(Type=="Murder") %>%
  ggplot(aes(x = Year, y = Count)) + 
  geom_point()

fbi %>% filter(Type == "Burglary", Year == 2014) %>% head()

#arrange function, sorts the data by the values in one or more variables
fbi %>% arrange(desc(Year), Type, desc(Count)) %>% head()

#select
fbi %>% arrange(desc(Year), Type, desc(Count)) %>%
  select(Type, Count, Year) %>% head()
#have to select the data in the same order 

#mutate - introduce a new variable into a data set or transform/upate an old variable
fbi %>% mutate(Rate = Count/Population * 70000) %>% head()

#summarize function - observations into a (set of ) one-number statistic(s)
fbi %>% 
  summarise(mean_rate= mean(Count/Population * 70000, na.rm = TRUE),
  sd_rate = sd(Count/Population*70000, na.rm=TRUE))

fbi %>% #summarize and group_by
  group_by(Type) %>%
  summarise(mean_rate = mean(Count/Population*70000, na.rm=TRUE), 
            sd_rate = sd(Count/Population*70000, na.rm = TRUE))


