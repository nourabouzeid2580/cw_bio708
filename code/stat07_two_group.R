#  two-group comparison : t-test 


pacman:: p_load(tidyverse)
rm((list = ls()))
library(tidyverse) # call add-in packages every time you open new R session


# read out the data 
df_fl <- read_csv("data_src/data_fish_length.csv")

df_fl

# base R
unique(df_fl$lake) # finds unique things within data set 

# dplyr - give tibble 
distinct(df_fl, lake)


# get mean and sd body size 

df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

df_fl_mu

# so first we group the data by lake, so either a or b lake, then summarize mean lanegth and call it 
# mu_l for mu_lake and set it to mean of length and do the same for sd and when we print we will see the 
#  mean and sd for both lakes in tibble format

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points

df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l),
               color = "blue3") +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3, 
             color = "blue3") + # can change the size to 20 nd it will be a big blob lol 
  labs(x = "Lake", # x label
       y = "Fish body length") # y label

# next we make a figure, using ggplot, and setting the x as length and this
# time instead of histogram and such we will use jitter to get a scatterplot-ish looking ordeal
# so by running the jitter line only, we get a baseline 
# then we run it with the geom_segment and we get the lines for the means, and when we run it with the
# geom point we get the point at the mean and not just the line too
# i added a color feature to make it more prominent and apparent



# 10.2 t-test in R ----------------------------------------------------------------------

# For this simple data set, we can use t-test. R provides t.test() function to perform this analysis:

x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

x

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

y

t.test(x, y, var.equal = TRUE)

# the printed ran code, will show : 

# data:  x and y
# t = -3.2473, df = 98, p-value = 0.001596
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.3124399 -0.7995601
# sample estimates:
#   mean of x mean of y 
# 13.350    15.406 

# this shows p-value, t value, etc. 

# 10.3 Test Statistic -----------------------------------------------------

# The starting point is to define what we want to look at.
# Since our focus is on examining the difference in means, it is logical to estimate the 
# disparity between the sample means in each lake.
# Let me denote the sample means as  μa and μb for Lake a and Lake b, respectively. 
# We can estimate the difference between the means using the following approach:

# Getting t-value 
# take another look at df_fl_mu

df_fl_mu 

# pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
  pull(mu_l)

# lake a
v_mu[1]

# lake b
v_mu[2]

# difference
v_mu[1] - v_mu[2]


# # Fortunately, we can utilize sample variances to address such uncertainties. 
# The t-statistic is a common indicator of the difference of means 
# that takes into consideration the uncertainty associated with sample means.

# We can calculate this value in R manually:

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

df_t

# pull values as a vector

# mean vector
v_mu <- pull(df_t, mu_l)

# variance vector 
v_var <- pull(df_t, var_l)

# samlple size vector 
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

t_value


# Null Hypothesis ---------------------------------------------------------

# 10.3.2 Null Hypothesis
# The observed t-statistic serves a dual purpose in accounting for both the disparity
# of means and the accompanying uncertainty. However, the significance of this t-statistic
# remains unclear.

# To address this, the concept of the Null Hypothesis is utilized to substantiate
# the observed t-statistic. The Null Hypothesis, no difference between groups or 
# μa =  μb , allows us to draw the probability distribution of the test-statistic. 
# Once we know the probability distribution, we can estimate the probability of observing 
# the given t-statistic by random chance if there were no difference in mean body size between the lakes.
# Let’s begin by assuming that there is no difference in the mean body size of fish populations 
# between the lakes, meaning the true difference in means is zero 
# (μa − μb = 0; without hats in this formula!). 
# Under this assumption, we can define the probability distribution of t-statistics,
# which represents how t-statistics are distributed across a range of possible values.

# This probability distribution is known as the Student’s t-distribution and
# is characterized by three parameters: the mean, the variance, and the degrees of freedom (d.f.). 
# Considering that we are evaluating the difference in body size as the test statistic, 
# the mean (of the difference) is assumed to be zero. The variance is estimated using 
# (where d.f. can be considered as a measure related to the sample size3).


# R has a function to draw the Student’s t-distribution; let’s try that out:
  
# produce 500 values from -5 to 5 with equal interval

x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)

y1 <- dt(x, df = 10 - 2)

# draw figure
tibble(x, y, y1) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y = y1),
            color = "red") +
  geom_vline(xintercept = t_value,
             color = "blue2") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "blue2") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 



# draw entire range
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "blue2") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "blue2") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 


# In a probability distribution, the area under the curve corresponds to the probability. 
# Notably, the area under the curve that falls below or above the observed t-statistic 
# (indicated by vertical red lines) is very small (Figure 10.3), meaning that the observed 
# difference in body size is very unlikely to occur under the null hypothesis (no difference in true means).

# The function pt() allows us to calculate the area under the curve:
  

# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2)

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)
  

# The p-value, i.e., the probability of observing t-statistics 
# less than ( pr_below) or greater than (pr_above) the observed 
# t-statistic under the null hypothesis, can be estimated as the sum of pr_below and pr_above.

# p_value

p_value <- pr_below + pr_above

p_value



# 10.4 t-test with unequal variance ---------------------------------------

# The above example assumes the relative similarity of variance between groups,
# which could be an unrealistic assumption. Luckily, there is a variant of t-test “Welch’s t-test,” 
# in which we assume unequal variance between groups. 

# The implementation is easy – set var.equal = FALSE in t.test():

t.test(x, y, var.equal = FALSE)

# Test it out!!!

# 10.4 t-test with unequal variance

x <- df_fl %>%
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = FALSE)

t.test(x, y, var.equal = TRUE)

# Welch Two Sample t-test
# 
# data:  x and y
# t = -3.2473, df = 95.846, p-value = 0.001606
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.3127929 -0.7992071
# sample estimates:
#   mean of x mean of y 
# 13.350    15.406 







