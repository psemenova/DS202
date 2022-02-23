library(tidyverse)
library(classdata)

a = c(1, 15, 3, 20, 5, 8, 9, 10, 1, 3)

a < 20
(a^2 >= 100) | (a^2 < 10)
(a == 1) | (a == 3)
( a / 2 - round(a / 2)) == 0 # mod(a) --> to find if the values are even or odd
a %% 2 # Mod funtion (another way)

a[a < 20] #subset of a with all the values that are less than 20

fbi1 = filter(fbi, Year == 2014)
fbi2 = filter(fbi, Type == "Larceny.theft", 
              State %in% c("Iowa", "Minnesota"),
              Year < 2000 | Year > 2010)

fbi3 = filter(fbi, Type == "Larceny.theft", State == "Iowa")
fbi3 %>% ggplot(aes(x = Year, y = Count / Population * 10^5)) + 
  geom_point() + 
  geom_line() + 
  ylab("Rate per 10 thousand population") +
  ggtitle("Iowa Larceny theft rate over time")

fbi3$Rate = fbi3$Count / fbi3$Population * 10^5
fbi3$Rate.diff =  c(NA, diff(fbi3$Rate))
filter(fbi3, Rate.diff < 0) #years that the trend is decreasing 

fbi4 = filter(fbi, Year == 2009, Type == "Larceny.theft")
fbi4 %>% ggplot(aes(x = State)) +
  geom_bar(aes(weight = Count / Population * 10^5)) +
  coord_flip() +
  ylab("Rate per 10 thousand population")
  
fbi5 = filter(fbi, Year > 2013, Type == "Murder.and.nonnegligent.Manslaughter")
quan90 = quantile(fbi5$Count / fbi5$Population, 0.9)
fbi6 = filter(fbi5, Count / Population >= quan90)

#updating a vector
a <- 1:4
a
a[2:3] <- 0
a #replace the second and third value with zero
replace(a, a == 0, -1) #second way to replace all the zero's with a -1

gap = read.csv("~/Desktop/Iowa State University/Comp Sci/ds202/Homework/Homework 2/gapminder-5060.csv")
filter(gap, country == "Canada", year == 1957)
gap$lifeExp = replace(gap$lifeExp, (gap$country == "Canada") & (gap$year == 1957), 69.96)


# Plot box plots
twoyear = dplyr::filter(fbi, Year %in% c(1961, 2014))
twoyear %>% ggplot(aes(x = Year, y = Count)) + geom_boxplot() # year by default is a continuos variable, so only one plot it shown
#fix -> make year a factor to only show the two years
twoyear %>% ggplot(aes(x = as.factor(Year), y = Count)) + 
  geom_boxplot() + 
  facet_wrap(~Type, scales = "free_y") #free y lets each plot have their own y axis 

#Data Types checking and casting
temp = fbi$Year[1 : 10]
temp.ch = as.character(temp) #numerical to character
temp.factor = as.factor(temp.ch) #numerical to factor (gives us levels) (can do temp.ch or temp, either one)
temp.ch.nu = as.numeric(temp.ch) # from chacater to numeric
temp.factor.nu = as.numeric(temp.factor) # from factor to numeric (must first change to character and then to numeric) -- wrong way
as.numeric(as.character(temp.factor)) # have to be careful when changing from factor to numeric -- correct way

fbi2018 = fbi %>% filter(Year == 2018, Type == "Murder.and.nonnegligent.Manslaughter")
fbi2018 %>% ggplot(aes(x = State)) +
  geom_bar(aes(weight = Count)) + coord_flip()

levels(fbi$Type)
fbi.temp = factor(fbi$Type, levels = c("Burglary", 
                                    "Larceny.theft", 
                                    "Motor.vehicle.theft",
                                    "Robbery",
                                    "Aggravated.assault",
                                    "Legacy.rape",
                                    "Rape",
                                    "Murder.and.nonnegligent.Manslaughter")) 
#create a factor variable, even if fbi$Type is a factor this lets us see the levels
levels(fbi.temp) #manually reorder the levels of the factor, have to be careful with putting in the exact name of the levels

#order by the average count of each crime
levels(reorder(fbi$Type, fbi$Count, na.rm=TRUE)) # na.rm=TRUE remove the NA values
#order by the median count of each crime
levels(reorder(fbi$Type, fbi$Count, FUN = median, na.rm = TRUE))

#reorder the bar plot from smallest to largest by state and count
fbi2018 = fbi %>% filter(Year == 2018, Type == "Murder.and.nonnegligent.Manslaughter")
fbi2018$State = as.factor(fbi2018$State) #first make it a factor to reorder it
fbi2018$State = factor(fbi2018$State, levels = levels(reorder(fbi2018$State, fbi2018$Count))) #reorder the variables using factor
fbi2018 %>% ggplot(aes(x = State)) +
  geom_bar(aes(weight = Count)) + coord_flip()

fbiwide1 = fbiwide[fbiwide$Year %in% c(1961, 1971, 1981, 1991, 2001, 2011), ]
fbiwide2 = fbiwide[fbiwide$State %in% c("California", "Colorado", "Iowa", "Illinois"), ]