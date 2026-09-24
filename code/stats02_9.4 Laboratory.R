

# 9.4.1 -------------------------------------------------------------------

# 9.4.1 Normal Distribution
# The function rnorm() produces a random variable that follows a
# Normal distribution with a specified mean and SD. Using this function,

# Generate a variable with 50 observations.
# Create a figure similar to Figure 9.3



library(tidyverse)
library(ggplot2)
rm(list =ls())
pacman::p_load(tidyverse, 
               patchwork)

count <- abs(rnorm(n = 50, mean = 10, sd = 2))

count 

df_count <- tibble(count=count)

(g_hist <- df_count %>% 
    ggplot(aes(x = count)) + 
    geom_histogram()) +
  labs(y = "Bud Counts") +
geom_vline(aes(xintercept = mean(count)))

x <- seq(min(df_count$count), max(df_count$count), length = 100)

mu <- mean(df_count$count)
sigma <- sd(df_count$count)
pd <- dnorm(x, mean = mu, sd = sigma)
pd

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density", 
       x = "Bud Count") # re-label

x_min <- floor(min(df_count$count)) # floor takes the integer part of the value
x_max <- ceiling(max(df_count$count)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, 
                sd = sigma) - pnorm(bin[i],
                                    mean = mu, 
                                    sd = sigma)
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "limegreen") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "limegreen")





# 9.4.2 -------------------------------------------------------------------
# The function rpois() produces a random variable that follows a
# Poisson distribution with a specified mean. Using this function,

# Generate a variable with 1000 observations.
# Create a figure similar to Figure 9.7

library(tidyverse)
library(ggplot2)
rm(list =ls())
pacman::p_load(tidyverse, 
               patchwork)

count <- abs(rpois(n = 1000, lambda = 100))

count 

df_count <- tibble(count=count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin


# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(min(count), max(count), by = 1) 

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label


df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size



df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed", 
            color = "maroon1") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq),  
               color = ("maroon1"))

