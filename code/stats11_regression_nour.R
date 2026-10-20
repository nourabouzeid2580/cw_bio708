# 12 : regression 

library(tidyverse)
pacman:: p_load(tidyverse)
rm(list =ls())
library(ggplot2)


df_algae <- read_csv("data_src/data_algae.csv")
print(df_algae)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)



# coef() extracts estimated coefficients
# e.g., coef(m)[1] is (Intercept)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line


# estimate Std. / error = t-value (for one variable)



# extract coeffecients

theta <-coef(m)

se <- sqrt(diag(vcov(m)))

(t_value <- theta / se)


# residual is the deviation from the line


# unexplained variation ---------------------------------------------------

# extract residual 


# manual 

eps0 <- df_algae$biomass - (alpha + beta * df_algae$conductivity)

# shortcut
eps <- resid(m)

plot(eps0 ~ eps)


# visualize error 

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot( 
    aes( x = conductivity, 
         y = biomass)
    ) + 
  geom_point() +
  geom_abline(
    intercept = alpha, 
    slope = beta
  ) + 
  geom_segment(
    aes(
      x = conductivity, 
      xend = conductivity, 
      y = biomass,
      yend = biomass - eps
    ), 
    linetype = "dashed"
  )


# original deviation in y 

y <- df_algae$biomass
ss0 <- sum((y - mean(y))^2)
ss1 <- sum(eps^2)

rsq <- 1 - ss1 /ss0

ss1 / ss0
# ss1 / ss0 number means how much variation remains unexplained, 
# and proportion of variance unexplained even after the effect 
# of the conductivity 

summary(m)















