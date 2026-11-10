# likliehood

rm(list =ls())
library(ggplot2)
pacman::p_load(tidyverse, patchwork)

# poisson example for liklihood -------------------------------------------

# probab of observ 3 given lambda = 3.5
(pr3 <- dpois(x = 3, lambda = 3.5))

# try 0.1-10 as poss val for lambda 
# find best value of lambda that maximizes Pr(x )
lambda <- seq(0.1, 10, by = 0.1)

hdf_pois <- tibble(lambda = lambda, 
                   pr = dpois(3, lambda = lambda)) %>% 
  arrange(desc(pr))

# vector case y = [3, 2, 5]

# probability of observing 3 data points together
# use lambda = 3.5 
y <- c(3, 2, 5)
y
pr <- dpois(x = y, lambda = 3.5)
pr

# manual calculation
lik <- pr[1] * pr[2] * pr[3]
lik

# product
lik <- prod(pr)

# liklihood for each lambda value 
# prob of observing vector y 
lambda <- seq(0.01, 10, by = 0.01)
lik_i <- sapply(lambda, 
       function(x) {
         prod(dpois(y, lambda =x))
       })

df_pois3 <- tibble(
  lambda = lambda, 
  lik = lik_i
) %>% 
  arrange(desc(lik))

df_pois3

# sample mean of y 
mean(y)

# why sample mean matches the MLE maximum likelihood estimate
# Pr(y = 3) with lambda = 3.5
dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5)) / factorial(3) 

# visualization
df_pois3 %>%  
  ggplot(
    aes( x = lambda, 
         y = lik)
  ) + 
  geom_line( color = "blue") +
  labs( x = "lambda value", 
        y = "Likelihood") 

# visualization with log(liklihood)
df_pois3 %>%  
  ggplot(
    aes( x = lambda, 
         y = log(lik))
  ) + 
  geom_line( color = "blue") +
  labs( x = "lambda value", 
        y = "Likelihood") 

## get liklihood from glm 
df_count <- read_csv("data_src/data_garden_count.csv")

m_pois <- glm(count ~ nitrate, 
    data = df_count, 
    family = poisson)
summary(m_pois)

logLik(m_pois)

