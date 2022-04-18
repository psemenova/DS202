library(tidyverse)
# color scales 

# default continuous color scheme
# scale_color_gradient -> used for continuous 
p1 <- mpg %>% filter(year == 2008) %>%
  ggplot(aes(x = cty, y = hwy, colour = cyl)) +
  geom_point()
p1 + scale_color_continuous()

# default discrete color scheme
p2 <- mpg %>% filter(year == 2008) %>%
  ggplot(aes(x = cty, y = hwy, colour = factor(cyl))) +
  geom_point()
p2 + scale_color_discrete()


library(RColorBrewer)
display.brewer.all()

p <- mpg %>% ggplot(aes(x = displ, y =  cty, colour= factor(class))) + geom_point()
p + theme_grey()


p <- mtcars %>% ggplot(aes(x = wt, y =  mpg, colour= factor(cyl))) + geom_point()
p + theme_light()
p + theme_dark()


library(ggthemes)
p + theme_excel() 
p + theme_fivethirtyeight()
