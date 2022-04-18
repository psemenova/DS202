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
counties %>% ggplot(aes(x = long, y = lat)) + geom_polygon(aes(group = group, fill = (subregion == "cook")))

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


library(tidyverse)
library(maps)

states <- map_data("state")
acc <- read.csv("https://raw.githubusercontent.com/DS202-at-ISU/labs/master/data/fars2016/accident.csv", stringsAsFactors = FALSE)

acc2016.mainland = acc %>% filter(YEAR == 2016, LONGITUD < 0, LONGITUD >-140, LATITUDE <55)


states %>% ggplot(aes(x = long, y = lat)) +
  geom_path(aes(group = group)) +
  geom_point(data = acc2016.mainland,
             aes(x = LONGITUD,y = LATITUDE), size = 0.05)


acc2016.mainland1 = acc2016.mainland %>% mutate(drunk = (DRUNK_DR > 0))
states %>% ggplot(aes(x = long, y = lat)) +
  geom_path(aes(group = group)) +
  geom_point(data = acc2016.mainland1,
             aes(x = LONGITUD,y = LATITUDE, col = drunk), size = 0.05)

states %>% ggplot(aes(x = long, y = lat)) +
  geom_path(aes(group = group)) +
  geom_point(data = acc2016.mainland1 %>% filter(FATALS >=3),
             aes(x = LONGITUD,y = LATITUDE, col = drunk), size = 0.5)


state.code = read.csv("StateNames.csv")
names(state.code)[1] = "region"
state.code$region = tolower(state.code$region)

acc2016.mainland2 = left_join(acc2016.mainland1, state.code, by = c("STATE" = "code"))

acc2016.state = acc2016.mainland2 %>% group_by(region) %>% 
  summarise(n = n()) %>% ungroup()


states.acc = left_join(states , acc2016.state, by = "region")
states.acc %>% ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = n)) # color by the number of accidents in 2016 


states.acc %>% ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = n))



