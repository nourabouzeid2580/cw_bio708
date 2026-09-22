# Probability 

library(tidyverse)
library(ggplot2)

# 9.1 Continuous variables - probability density function 

rm(list =ls())
pacman::p_load(tidyverse, 
               patchwork)

# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0

# make histogram of data for plant height
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# Probability Density Function from textbook

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label

# 

# textbook definitions of dnorm() -----------------------------------------

# In R, the function dnorm() can be used to calculate the probability density for a given value. 

# The first argument, x, should be a vector of values for which you want to calculate the probability density.

# The second argument, mean, and the third argument, sd, correspond to the mean and standard deviation of the distribution, 
# respectively. Note that we must provide mean and sd to calculate the probability density.

# To encompass the entire range of observed heights, we can use min(x) and max(x)  as the lower and upper limits, respectively.

# pd <- dnorm(x, mean = mu, sd = sigma)

# draw probability distribution run function -------------------------------------------
x <- seq(min(df_h0$height),
    max(df_h0$height),
    length = 100)
x

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

mu 
sigma
pd

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, 
             y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density",
       x = "Plant Height") # re-label and this can be changed 


# Textbook pnorm() defintion ---------------------------------------------------------------

# The shape of the curve appears quite similar to what we observed; however, 
# the scale of the y-axis is different. This is because the y-axis represents “probability density.” 
# To convert it into actual “probability,” we need to calculate the area under the curve. 
# In R, we can utilize the pnorm() function for this purpose. It calculates the probability of a variable being 
# less than the specified value, which is provided in the first argument q.

# convert probability density into frequency 

pnorm(q = 10, mean = mu, sd = sigma)


# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)

# # To make the estimates comparable to the frequency data, you can calculate the probability for each 1 cm bin. 
# The expected frequency can be obtained by multiplying the probability by the sample size, which in this case is 1000. 
# This allows you to estimate the number of observations you would expect in each 1 cm bin based on the calculated probabilities

x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value, floor rounds down 
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer, ceiling rounds up
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability

for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1],
                mean = mu, 
                sd = sigma) - 
    pnorm(bin[i],
          mean = mu, 
          sd = sigma)
}



# in class code -----------------------------------------------------------

# p <- NULL
# 
# for (i in 1:(length(bin) - 1)) {
#   # p_up - probability up to bin[i + 1]
#   p_up <- pnorm(bin[i + 1], mean = mu, sd = sigma)
#   # p_down - probability up to bin[i - 1]
#   p_low <- pnorm(bin [i], mean = mu, sd = sigma)
# }
# 
# p[i] <- p_up - p_low


# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin


df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>%
mutate(freq = p * nrow(df_h0))

df_prob

# overlay combine data and pdf

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # don't forget plus!!!!! # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# Discrete Variable -------------------------------------------------------


# textbook ----------------------------------------------------------------

# There are several significant differences compared to the plant height example when 
# considering the density of plants in the garden:
# #   
# #   The possible values are discrete (count data) since we are counting the number of plant 
#   individuals in each plot.
# # 
# # The possible values are always positive, as counts cannot be negative.
# # 
# # The distribution appears to be non-symmetric around the sample mean.
# # 
# # Given these characteristics, a Normal distribution may not be an appropriate choice for 
# representing such a variable. Instead, it would be more suitable to use a Poisson distribution to 
# characterize the observed distribution of the discrete variable.
# 
# # In a Poisson distribution, the mean parameter (λ) serves as the sole parameter2. 
# Probability distributions that describe
# discrete variables are expressed using a probability mass function (PMF):


# Probability Mass Function

df_count <- read_csv("data_src/data_garden_count.csv")

df_count

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0)# relative position of each bin


# 9.2.2 PMF to frequency distribution

# vector of x values, create a vector of 0 to 10 with an interval one, must be integer of > 0

# poisson fit
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat) # discreet representation 


# figrue 

# figure
tibble(y = pm, 
       x = x) %>% # data frame
  ggplot(aes(x = x,
             y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label 

# To convert the y-axis from probability to frequency, multiply the probabilities by
# the sample size (total number of observations) to obtain the expected frequency. 
# As in the plant height example, you can plot the expected frequency on the histogram, 
# providing a visual representation of the distribution of the discrete variable.

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size 

# overlay 
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") + 
  geom_point(data = df_prob,
             aes(x = x, 
                 y = freq))

# with color 

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed", 
            color = "steelblue") + 
  geom_point(data = df_prob,
             aes(x = x, 
                 y = freq), 
             color = "steelblue")
