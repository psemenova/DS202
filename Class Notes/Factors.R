library(tidyverse)
library(classdata)
library(ggplot2)

crime.type = fbi$Type
levels(crime.type)
#first parameter = what we are reordering, second = based on what we are reordering
level.reorder = levels(reorder(fbi$Type, fbi$Count, fun = mean, na.rm=TRUE))
#create a new factor, based on original factor but the levels are corresponding to the new levels
crime.type.reorder = factor(crime.type, levels = level.reorder) 
crime.type.reorder[c(1, 1000, 3001, 10000)]
crime.type[c(1, 1000, 3001, 10000)]

#changing the level's names
levels(crime.type.reorder)[1] = "Murder" #don't forget to put `levels` to change the first level and not the first element 
crime.type.reorder[c(1, 1000, 3001, 10000)]

#Visualize factors
crime.type = fbi$Type
levels(crime.type)[1] = "Assault"
levels(crime.type)[5] = "Motor.theft"
levels(crime.type)[6] = "Murder"
fbi$Type = crime.type
ggplot(fbi, aes(x = Type, fill= Year)) + geom_bar() #nothing happens, forgot to make a bar plot for count
ggplot(fbi, aes(x = Type, fill= as.factor(Year))) + 
  geom_bar(aes(weight = Count))

#reorder 
level.reorder = levels(reorder(fbi$Type, fbi$Count, fun = mean, na.rm=TRUE))
crime.type.reorder = factor(crime.type, levels = level.reorder)
fbi$Type = crime.type.reorder
ggplot(fbi, aes(x = Type, fill= as.factor(Year))) + 
  geom_bar(aes(weight = Count))

#Survival on the Titanic, new data set
head(titanic)

ggplot(titanic, aes(x = Survived)) + geom_bar()
ggplot(titanic, aes(x = Sex)) + geom_bar()
ggplot(titanic, aes(x = Age)) + geom_bar()
ggplot(titanic, aes(x = Class)) + geom_bar()

ggplot(titanic, aes(x = Sex, fill = as.factor(Survived))) + 
  geom_bar()
ggplot(titanic, aes(x = Sex, fill = as.factor(Survived))) + 
  geom_bar(position = "fill") #the fill shows proportions more clearly but doesn't show the number of survival
ggplot(titanic, aes(x = Sex, fill = as.factor(Survived))) + 
  geom_bar(position = position_dodge2()) #shows side by side instead of stacking 


#two and more factor variables
library(ggmosaic)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Sex), fill = Survived, weight = 1)) +
  facet_grid(Age~Class)

#Fbi data, how to show relationship between class, gender, and survive?
fbi1 = fbi[fbi$State %in% c("California", "New York"), ]
fbi1 = fbi1[fbi1$Year %in% c(1971, 2016), ]
ggplot(data = fbi1, aes(x = State, fill=Type)) +
  geom_bar(aes(weight = Count), position = position_dodge()) + #total crimes in each state in the two years
  facet_wrap(~Year)

ggplot(data = fbi1, aes(x = Type, fill = State)) +
  geom_bar(aes(weight = Count / Population * 10 ^ 5), position = position_dodge()) +
  ylab("count") +
  facet_wrap(~Year) + coord_flip()





