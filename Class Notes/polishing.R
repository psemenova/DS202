library(tidyverse)
# COLOR SCALES

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
p2 + scale_color_brewer(palette = "Set1")
p2 + scale_color_brewer(palette = "Dark2")
p2 + scale_color_brewer(palette = "Greens")
p2 = p2 +  scale_color_brewer(palette = "Set1")
p2
p2 = p2 + xlab("city miles per gallon") + ylab("highway miles per gallon") + ggtitle("city vs. highway")
p2 = p2 + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 22)) # center the title
p2 = p2 + labs(color = "number of cyl") # change name of legend
p2 = p2 + theme(legend.position = "bottom") # move the legend to the bottom of the plot
p2


# THEMES 
library(RColorBrewer)
display.brewer.all()

p <- mpg %>% ggplot(aes(x = displ, y =  cty, colour= factor(class))) + geom_point()
p + theme_grey() #default -> grey background/white grid
p + theme_bw() # white background/black grid


p <- mtcars %>% ggplot(aes(x = wt, y =  mpg, colour= factor(cyl))) + geom_point()
p + theme_light()
p + theme_dark()


library(ggthemes)
p + theme_excel() 
p + theme_fivethirtyeight()


# element_line/text/blank

# CHANGING ELEMENTS MANUALLY
mpg %>% ggplot(aes(x = manufacturer)) + geom_bar() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

