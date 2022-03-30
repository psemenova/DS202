library(Lahman)
library(tidyverse)
library(classdata)
View(LahmanData)

#HallOfFame = HallOfFame %>% filter(yearID == 2017)

str(HallOfFame)

HallOfFame$inducted == "Y"
player2017s = HallOfFame$playerID[HallOfFame$inducted == "Y"] # players that are successfuly inducted
Master[Master$playerID %in% player2017s, ]

# Join
dim(Master) #look dimension of data
dim(HallOfFame) #look dimension of data

Master.num = Master %>% group_by(playerID) %>% tally() #count the number of recordings for each playerid (can also use summarise)
max(Master.num$n) # if it is 1 then everyone has only one recording

Master1 = Master 
names(Master1)[1] = "playerid" #change column to lowercase 

#anti_join(HallOfFame, Master1, by="playerID") # error because missing playerID from Master1 
anti_join(HallOfFame, Master1, by=c("playerID" = "playerid")) #fix, say that the two columns are equal

anti_join(HallOfFame, Master, by="playerID") # to see if any recordings don't match
HallOfFame.master = left_join(HallOfFame, Master, by="playerID")
dim(HallOfFame.master)

# HallOfFame.Master %>% filter(deathDate == NA) # doesn't work because can't directly compare to NA
sum(is.na(HallOfFame.master$deathYear))
HallOfFame.alive.2017 = HallOfFame.master %>% filter(is.na(deathDate) == TRUE, inducted == "Y") 

HallOfFame.master %>% filter(nameLast == "Sosa")

anti_join(Master, HallOfFame, by="playerID")

NA + 32
a = c(NA, 1, 2, 3, 4, 5)
sum(a, na.rm = TRUE)
sum(a)
a[is.na(a) == FALSE]
# a == NA // wrong
a == "NA" # consider as a character
is.na(a)
a = c(NA, 1, 2, 3, 4)
b = c(2, 4, 5, NA, 6)
d = c(3, NA, 4, 5, 6)
e = c(-1, 2, NA, 3, 7)
DF = data.frame(a, b, d, e)
DF
na.omit(DF)

DF1 = DF[, c(1, 2)]
DF1
na.omit(DF1)


box.na = is.na(box)
colSums(box.na)
dim(box)
6269 / dim(box)
temp = rowSums(box.na)
temp[1 : 100]
hist(temp)
table(temp)

box.rank.last.week.na = box %>% filter(is.na(Rank.Last.Week) == TRUE) %>% select(Rank, Movie, Week)
box.rank.last.week.na %>% ggplot(aes(y = Week)) + geom_boxplot()

box1 = na.omit(box) # remove about 1/5th of the data
dim(box)
dim(box1)

# box$Rank.Last.Week <- na.omit(box$Rank.Last.Week) # wrong




