a = c(1,2,3)
b = c(5:9)
1 - a

a
b
#set working directory -- setad("~ds202")
# read data -- topics <- read.csv("topics.csv")
topics <- read.csv("topics.csv")
topics <- topics %>% select(~Entry.id)



