# 14 Generalized Linear Model

rm(list =ls())
library(ggplot2)
pacman::p_load(tidyverse, patchwork)


# Poisson -----------------------------------------------------------------


# 14.1 Count Data
# 14.1.1 Plant Density
# Recall our data set of garden plant counts (Chapter 9):

df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)


# draw figure relating nitrate to plant count 

df_count %>% 
  ggplot(
    aes( x = nitrate, 
         y = count)
    ) + 
  geom_point()


# fit lm() to this daata, and draw a predicted line, 
# use geom_abline() for prediction/fitted line 
# y = count, x = nitrate 

m_lm <- lm(count ~ nitrate, data = df_count)
m_lm

summary(m_lm)

df_count %>% 
  ggplot(
    aes( x = nitrate, 
         y = count)
  ) + 
  geom_point() + 
  geom_abline(
    intercept = coef(m_lm)[1],
    slope = coef(m_lm)[2], 
    color = "red"
  )

# alternative to ordinary regeression 
# "Poisson" regresssion for count data 

m_pois <- glm( count ~ nitrate, 
     data = df_count, 
     family = "poisson")
m_pois

summary(m_pois)

# use intercept and slope as is to draw prediction
# geom_abline lol figure is so off
df_count %>% 
  ggplot(
    aes( x = nitrate, 
         y = count)
  ) + 
  geom_point() + 
  geom_abline(
    intercept = coef(m_pois)[1],
    slope = coef(m_pois)[2], 
    color = "red"
  )

# link function, log(lambda) = alpha + beta * x for Poisson

x <- seq( min(df_count$nitrate), 
          max(df_count$nitrate), 
          length = 100)


a <- coef(m_pois)[1] # intercept in a log-scale 
b <- coef(m_pois)[2] # slope in log-scale 

log_y_pred <- a + b * x
y_pred <- exp( log_y_pred)

df_pred <- tibble( y_pred = y_pred, 
                   nitrate = x)
df_pred


df_count %>% 
  ggplot(
    aes( x = nitrate, 
         y = count)
    ) + 
  geom_point() + 
  geom_line(
    data = df_pred, 
     aes(y = y_pred), 
    color = "darkblue"
  )


# compare results between lm() and glm()
# what are the differences if any?

summary(m_lm)

summary(m_pois)



# Binomial ----------------------------------------------------------------


# 14.2 Proportional Data
# 14.2.1 Mussel Egg Fertilization

df_mussel <- read_csv("data_src/data_mussel.csv")
print(df_mussel)


# figure
df_mussel %>% 
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(
    aes(x = density,
        y = p)
    ) +
    geom_point(color = "red")


# binomial regression 
# n_examined is the possible maximum success
# n_fertilized is the success numbers, 
# binomial distributions take two parameters, 
#  Bin(N, p) where N is the number of trials and p is the success probability 

cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, 
    data = df_mussel, 
    family = "binomial")

m_binom

summary(m_binom)

# draw prediction?

z <- seq(min(df_mussel$density), 
         max(df_mussel$density), 
         length = 100) 

theta <- coef(m_binom) 
logit_p <- theta[1] + theta[2] * z
p_pred <- exp(logit_p /(1 + exp(logit_p))) 


df_p <- tibble( p_pred = p_pred, 
                   density = z) 
df_p

df_mussel %>% 
  mutate( p = n_fertilized / n_examined) %>% 
  ggplot(
    aes( x = density, 
         y = p)
  ) + 
  geom_point(color = "red") + 
  geom_line(
    data = df_p, 
    aes(y = p_pred), 
    color = "darkblue"
  )

# presence and absence = binomial 
