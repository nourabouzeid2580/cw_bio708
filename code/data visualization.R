
#call every single time 
library(tidyverse)
# run

#contrl shitf r = creates a section label, helps stay organized 
# point figure ------------------------------------------------------------

##basic point figure #
# aes() statnds for aesthetic and helps create a basis for creating graphs.

iris %>% 
  ggplot(
    mapping = aes( x = Sepal.Length, 
                  y = Sepal.Width)
    ) + 
    geom_point()

#run

# error example missing aes() ---------------------------------------------

## error, no aes() 
# control + shift + c helps to create the #
# iris %>% 
#   ggplot(
#     x = Sepal.Length, 
#     y = Sepal.Width
#   ) + geom_point()

# run the above to see the error when aes() is missing in code


# color by species --------------------------------------------------------

# another way to get plot 
ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 y = Sepal.Width)
)  + 
  geom_point()

# Color by species way one
ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species)
)  + 
  geom_point()

# Color by everything together 
ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 y = Sepal.Width)
)  + 
  geom_point(color = "red")
#diffirent colors like "salmon" "steelblue"


# error for color ---------------------------------------------------------


# Color by species error
# ggplot(
#   data = iris, 
#   mapping = aes( x = Sepal.Length,
#                  y = Sepal.Width),
#   color = Species,
# )  + 
#   geom_point()
# run to see error with misordering color function



# line figure -------------------------------------------------------------

df0 <- tibble(
  x = rep(1:50, 3),
  y = x * 2
)

df0 %>% 
  ggplot(
    mapping = aes( x = x, 
                   y = y)
  ) + 
  geom_line()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

# messing around with code 
# attempt to incorporate color in histogram -------------------------------

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 )
)  + 
  geom_histogram(color = "red")
#this outline the bars with red, but hey it worked lol


# another color attempt this time by species on histogram -----------------

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 color = Species)
)  + 
  geom_histogram()
#this one shows all of the colors now, this is so cool!


# this time using fill instead of color for histogram ----------------------

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 fill = Species)
)  + 
  geom_histogram()

#can you do both fill and color? 

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

# #ggplot(
#   data = iris, 
#   mapping = aes( x = Sepal.Length,
#                  fill = Species
#                  color = count)
# )  + 
#   geom_histogram()
# answer is no lol 


# this time color by speal.width in histogram -----------------------------

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Width,
                 color = Species)
)  + 
  geom_histogram()
# it looks so different, but i guess if two species have the same value, 
# they would overlap, so which one would be visible?


# change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

# change bin number
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)

# Box plot ---------------------------------------------------------------

#blank boxplot (no color)

iris %>% 
  ggplot(
    mapping = aes( x = Species, 
                   y = Sepal.Length)
  ) + geom_boxplot()

#change border color 
iris %>% 
  ggplot(
    mapping = aes( x = Species, 
                   y = Sepal.Length, 
                   color = Species)
  ) + geom_boxplot()

#fill in color of the boxes
iris %>% 
  ggplot(
    mapping = aes( x = Species, 
                   y = Sepal.Length, 
                   fill = Species)
  ) + geom_boxplot()

#another way for color 

# change fill by "Species", but consistent color
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "black")
# can change "black" to other colors,
# this is what i tried to do with fill and color


# just playing with code --------------------------------------------------


# more messing around

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 color = Species)
)  + 
  geom_histogram()


iris %>% 
  ggplot(aes(x = Sepal.Length,
             fill = Species)) +
  geom_histogram(color = "black")

# my tampering lol

iris %>% 
  ggplot(mapping = aes (x = Sepal.Length)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Length,
                 fill = Species)
)  + 
  geom_histogram()

iris %>% 
  ggplot(aes(x = Sepal.Length,
             fill = Species)) +
  geom_histogram(color = "black")

# ayyyyy it worked lol!

# sepal width now
iris %>% 
  ggplot(mapping = aes (x = Sepal.Width)) +
  geom_histogram()

ggplot(
  data = iris, 
  mapping = aes( x = Sepal.Width,
                 fill = Species)
)  + 
  geom_histogram()

iris %>% 
  ggplot(aes(x = Sepal.Width,
             fill = Species)) +
  geom_histogram(color = "black")

# this is so fun 

# back to speal length 


# fun code ----------------------------------------------------------------

# fun code "violin"

# Library
library(ggplot2)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Most basic violin chart
p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()
p
#p

data_wide <- iris[ , 1:4]

library(tidyr)
library(ggplot2)
library(dplyr)
data_wide %>% 
  gather(key="MesureType", value="Val") %>%
  ggplot( aes(x=MesureType, y=Val, fill=MesureType)) +
  geom_violin()

# 2 violins instead of 4? if i change the 
# data_wide <- iris[ , 1:4] to data_wide <- iris[ , 1:2]?

data_wide <- iris[ , 1:2]
library(tidyr)
library(ggplot2)
library(dplyr)
data_wide %>% 
  gather(key="MesureType", value="Val") %>%
  ggplot( aes(x=MesureType, y=Val, fill=MesureType)) +
  geom_violin()

# answer is yes! omg this is amazing!

# can do a lolipop plot too??!!!

# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  x=LETTERS[1:26], 
  y=abs(rnorm(26))
)

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_point() + 
  geom_segment( aes(x=x, xend=x, y=0, yend=y))

# this one is like the enrichment graphs with the volcano vplots
# library
library(ggplot2)
library(dplyr)

# Create data (this takes more sense with a numerical X axis)
x <- seq(0, 2*pi, length.out=100)
data <- data.frame(
  x=x, 
  y=sin(x) + rnorm(100, sd=0.2)
)

# Add a column with your condition for the color
data <- data %>% 
  mutate(mycolor = ifelse(y>0, "type1", "type2"))

# plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")

# i can maybe use this to change based on fold change too, kind of like the iDEP and GSEA PLOTS!!!


