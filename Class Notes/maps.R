library(tidyverse)
library(classdata)

states <- map_data("state")
head(states)

states %>% ggplot(aes(x = long, y = lat)) + geom_point() # dotted outline
states %>% ggplot(aes(x = long, y = lat)) + geom_path(aes(group = group)) # line outline
states %>% ggplot(aes(x = long, y = lat)) + geom_polygon(aes(group = group)) # filled in 
states %>% ggplot(aes(x = long, y = lat)) + geom_polygon(aes(group = group, fill=lat)) # filled in by color for lat


counties <- map_data("county")
counties %>% ggplot(aes(x = long, y = lat)) + geom_polygon(aes(group = group, fill = (subregion == "washington")))

counties %>% group_by(subregion) %>% summarise(n = length(unique(group))) %>% arrange(desc(n))


data(fbi, package="classdata")
fbi14 <- fbi %>% filter(Year == 2014)
head(fbi14)

fbi14$region <- tolower(fbi14$State)

nomatch1 <- fbi14 %>% anti_join(states, by="region")
# States for which we do not have map data
unique(nomatch1$State)
nomatch2 <- states %>% anti_join(fbi14, by="region")
# States for which we do not have crime data
unique(nomatch2$State)

fbi.map <- fbi14 %>% left_join(states, by="region")
fbi.map %>% filter(Type=="Burglary") %>% 
  ggplot(aes(x = long, y = lat, fill=Count/Population*70000)) +
  geom_polygon(aes(group=group))


map %>% ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group)) +
  geom_point(aes(x=longitude, y = latitude), data = content)

