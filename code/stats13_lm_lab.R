
pacman:: p_load(tidyverse, patchwork)
rm(list =ls())
library(ggplot2)



# 13.4.1 Normality Assumption


df_tooth <- as_tibble(ToothGrowth)

df_tooth

m_tooth <- lm (len ~ supp * dose, 
              data = df_tooth)
m_tooth

summary(m_tooth)

eps <- resid(m_tooth)

shapiro.test(eps)


# 13.4.2 Model Interpretation

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))
df_pred

# generate prediction

y_pred <- predict(m_tooth, newdata = df_pred)
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)


# visualization

ToothGrowth %>% 
  ggplot(aes(
    x = dose, 
    y = len, 
    color = supp
  )) +
  geom_point(alpha = 0.2) +
  geom_line(
    data = df_pred, # switch data frame!
    aes(y = y_pred)
    
  ) + theme_bw()



# 13.4.3 Multicollinearity

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

mv 

## true regression coefficients
b <- c(0.05, 1.00)

b

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))
df_y


# A scatter plot of y (y-axis) versus x1 (x-axis)

gx1 <- df_y %>% 
  ggplot( aes( 
    x = x1, 
    y = y, 
    )) + geom_point()

# A scatter plot of y (y-axis) versus x2 (x-axis)

gx2 <- df_y %>% 
  ggplot( aes( 
    x = x2, 
    y = y, 
  )) + geom_point()

# Create a scatter plot of x1 and x2.

gx1 | gx2

gx1 + gx2

# Examine the Pearson’s correlation between x1 and x2 using a function cor(). (?cor() for its usage).


# model analysis
lm( y ~ x1 + x2, 
    data = df_y) %>% 
  summary()

# check predictor relationship

df_y %>%  
  ggplot(aes( 
    x = x1, 
    y = x2)) + 
      geom_point()

# correlation coef

cor(df_y$x1, df_y$x2)

# greater than .6 or .7


# 13.4.4 Experimental Design
# You are designing an experiment to examine the effects of nutrient level and predation on prey fish growth. 

# The proposed treatment combinations are:

# Low nutrient and predator present

#  interpertation: there would be low prey fish growth, due to high predation and low nutrient availability 

# High nutrient and predator present

# High nutrient and predator absent

# answer: would need to have low nutrient and predator absent environment in order to assess if change in growth is
# attributed to only nutrient or only predation, or independent main effect of either without the fourth sampling environment.
 
# In addition, you plan to record the initial length of the prey fish (a continuous variable), 
# as it may influence subsequent growth.
 
# Question: Select the most appropriate statement regarding what this experiment can assess:
   
# 1. This experiment can capture the effects of nutrient, predation, and their interaction.
 
# 2. This experiment can capture the effects of nutrient and predation. However, the nutrient effect may be
#    limited to the presence of predators, and the predator effect is limited to the high nutrient condition.

# answer: the second statement is the most reasonable
 
# 3. This experiment can capture the effects of nutrient and predation. Main effects can be assessed across all 
#    nutrient conditions.
 
# 4. This experiment cannot capture either main effects or the interaction.
 
