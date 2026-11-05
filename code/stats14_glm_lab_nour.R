rm(list =ls())
library(ggplot2)
pacman::p_load(tidyverse, patchwork)

# 14.4 Laboratory
# 14.4.1 GLM exercise

# A publicly available stream fish distribution dataset can be accessed from an online repository:

# The dataset comprises information on the number of species found at each site (n_sp) and several 
# associated environmental variables, including distance to the sea (distance), catchment area (cat_area), 
# and environmental heterogeneity (hull_area). These environmental variables are factors that are known to 
# influence fish distributions. To analyze this dataset, develop a Generalized Linear Model (GLM) as follows:
#   
#   Use fish species richness n_sp as a response variable of the model with an appropriate probability
# distribution (family). Identify which predictor variable(s) significantly influence the response variable.
# The mtcars dataset contains information about 32 car models, including their performance and design 
# characteristics. The variable am indicates the type of transmission, where 0 = automatic and 1 = manual.
# 
# Using this dataset, develop a GLM to explore:
#   
#   Which variables significantly influence the probability that a car has a manual transmission? 
#   Choose an appropriate probability distribution (family). Consider including the following predictors: 
#   miles per gallon (mpg), horsepower (hp), and weight (wt).
# 
# Develop the same GLM but with a normal distribution (family = "gaussian"). Compare the results to 
# the model with an appropriate probability distribution.



url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

url
df_fish

m_fish <- glm(n_sp ~ distance + cat_area + hull_area, 
              data = df_fish, 
              family = "poisson")

summary (m_fish)

m_am <- glm(am ~ mpg + hp + wt, 
            data = mtcars, 
            family = "binomial")

summary(m_am)


m_am_normal <-  glm(am ~ mpg + hp + wt, 
                    data = mtcars, 
                    family = "gaussian")
summary(m_am_normal)

# effect size -------------------------------------------------------------


# 14.4.2 Effect size

df_fish <- df_fish %>% 
  mutate( 
    std_dist = c(scale(distance)), 
    std_cat = c(scale(cat_area)), 
    std_hull = c(scale(hull_area)))
  


m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                  data = df_fish, 
                  family = "poisson")  


summary(m_fish)
summary(m_fish_std)



# offset term -------------------------------------------------------------


# 14.4.3 Offset term

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

df_offset <- df_offset %>% 
  mutate(density = count / area)

m_offset <- glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

summary(m_offset)

g_area <- df_offset %>% 
  ggplot(
    aes( x = area, 
              y = count) 
    ) + geom_point() 

g_nitrate <- df_offset %>% 
  ggplot(
    aes( x = nitrate, 
         y = count) 
  ) + geom_point() 

g_den <- df_offset %>% 
  ggplot(
    aes( x = nitrate, 
         y = density) 
  ) + geom_point() 

g_area | g_nitrate | g_den


m_pois_off <- glm( count ~ nitrate + offset(log(area)), 
     data = df_offset, 
     family = "poisson")

m_normal <- glm( density ~ nitrate, 
                   data = df_offset, 
                   family = "gaussian")

summary(m_pois_off)
summary(m_normal)

# overdispersion ----------------------------------------------------------


# 14.4.4 Overdispersion

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)

df_tadpole

# This dataset contains the data of tadpole counts (tadpole), aquatic vegetation cover (aqveg), 
# and pond permanence (the number of days persist; permanence).
# 
# Plot the relationships between (a) tadpole and aqveg and (b) tadpole and permanence.
# 
# Discuss in a group that which variable may have significant influence.
# 
# Develop a GLM (choose an appropriate distribution) explaining tadpole with aqveg and permanence.

glm(tadpole ~ aqveg + permanence, 
    data = df_tadpole, 
    family = "poisson")

df_tadpole %>% 
  ggplot( 
    aes(x = aqveg,
         y = tadpole)) + geom_point() 
    
df_tadpole %>% 
  ggplot( 
    aes(x = permanence,
        y = tadpole)) + geom_point() 

mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)


MASS::glm.nb(tadpole ~ aqveg + permanence, 
             data = df_tadpole) %>% 
  summary()

# my work -----------------------------------------------------------------

# scrap work lol

# df_fish %>% 
#   ggplot(
#     aes( x = n_sp, 
#          y = distance)
#   ) + 
#   geom_point()
# 
# 
# 
# fish_lm <- lm(n_sp ~ distance,
#               data = df_fish
# )
# fish_lm
# 
# summary(fish_lm)
# 
# df_fish %>% 
#   ggplot(
#     aes( x = n_sp, 
#          y = distance)
#   ) + 
#   geom_point() + 
#   geom_abline(
#     intercept = coef(fish_lm)[1],
#     slope = coef(fish_lm)[2], 
#     color = "red"
#   )
# 
# fish_pois <- glm( n_sp ~ distance, 
#                   data = df_fish, 
#                   family = "poisson")
# fish_pois
# 
# summary(fish_pois)
# 
# 
# df_fish %>% 
#   ggplot(
#     aes( x = n_sp, 
#          y = distance)
#   ) + 
#   geom_point() + 
#   geom_abline(
#     intercept = coef(fish_pois)[1],
#     slope = coef(fish_pois)[2], 
#     color = "red"
#   )
# 
# 
# # distance and species richness
# glm(n_sp ~ distance,
#     data = df_fish,
#     family = "poisson")
# 
# # distance and catchment area
# glm(n_sp ~ cat_area,
#     data = df_fish,
#     family = "poisson")
# 
# 
# # distance and environmental heterogeneity
# glm(n_sp ~ hull_area,
#     data = df_fish,
#     family = "poisson")
