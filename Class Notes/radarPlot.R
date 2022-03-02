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
# fbi %>% mutate(Rate = Count/Population * 70000) %>% head()

#summarize function - observations into a (set of ) one-number statistic(s)
fbi %>% 
  summarise(mean_rate= mean(Count/Population * 70000, na.rm = TRUE),
  sd_rate = sd(Count/Population*70000, na.rm=TRUE))

fbi %>% #summarize and group_by
  group_by(Type) %>%
  summarise(mean_rate = mean(Count/Population*70000, na.rm=TRUE), 
            sd_rate = sd(Count/Population*70000, na.rm = TRUE))

#group_by and mutate
fbi <- fbi %>% mutate(
  Rate = Count/Population * 70000, Type = factor(Type), Year = factor(Year)
)

fbi1 = fbi %>% mutate(Type = reorder(Type, Rate, median, na.rm = TRUE))
levels(fbi1$Type)
fbi1 %>% ggplot(aes(x = Type, y = Rate)) +
  geom_boxplot()


fbi1 = fbi1 %>% group_by(Type) %>% mutate(
  best = rank(Rate) # ranks from lowest rate to highest rate // (rank(-Rate)) <- highest to lowest
)
fbi %>% filter(best == 1) %>% select(Type, State, Year, Rate)


# how many times the states appeared in the top for crime 
fbi1 = fbi1 %>% group_by(Type, Year) %>% mutate(
  best = rank(Rate) # ranks from lowest rate to highest rate
)
fbi2 = fbi1 %>% filter(best < 4) %>% select(Type, State, Year, Rate, best)
fbi2 %>% ggplot(aes(x = State)) +
  geom_bar() + 
  facet_wrap(~Type, scales = 'free') +
  coord_flip()


# French Fries Data
data(french_fries, package="reshape2")
french_fries.mean = 
french_fries %>% group_by(time) %>% summarise(
  m.potato = mean(potato, na.rm=TRUE),
  m.buttery = mean(buttery, na.rm=TRUE),
  m.grassy = mean(grassy, na.rm=TRUE),
  m.rancid = mean(rancid, na.rm=TRUE),
  m.painty = mean(painty, na.rm=TRUE)
) 


french_fries.mean %>% mutate(time = as.numeric(as.character(time))) %>%
  ggplot(aes(x = time)) + 
  geom_point(aes(y = m.potato)) +
  geom_point(shape=2, aes(y=m.rancid)) + 
  geom_point(shape=3, aes(y=m.painty)) +
  geom_line(aes(x=time, y=m.potato)) +
  geom_line(linetype = 2, aes(x=time, y=m.rancid)) +
  geom_line(linetype = 3, aes(x=time, y=m.painty)) +
  ylab("Average")

# do ratings of potato-y show a difference between the different oils over time?
# draw a plot of the average rating by time, color by treatment. 
# how does this plot look like for the rancid rating?
french_fries.trt.mean = 
  french_fries %>% group_by(time, treatment) %>% summarise(
    m.potato = mean(potato, na.rm=TRUE),
    m.buttery = mean(buttery, na.rm=TRUE),
    m.grassy = mean(grassy, na.rm=TRUE),
    m.rancid = mean(rancid, na.rm=TRUE),
    m.painty = mean(painty, na.rm=TRUE)
  )

french_fries.trt.mean %>% mutate(time = as.numeric(as.character(time))) %>% 
  ggplot(aes(x = time, col = treatment)) +
  geom_point(aes(y = m.potato)) +
  geom_point(shape=2, aes(y=m.rancid)) + 
  geom_point(shape=3, aes(y=m.painty)) +
  geom_line(aes(x=time, y=m.potato)) +
  geom_line(linetype = 2, aes(x=time, y=m.rancid)) +
  geom_line(linetype = 3, aes(x=time, y=m.painty)) +
  ylab("Average")


