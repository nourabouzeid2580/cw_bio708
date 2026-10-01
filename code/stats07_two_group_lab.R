
# 10.5 Laboratory

# 10.5.1 Influence of Sample Size -----------------------------------------

# Create the following vectors with rnorm():

# xs with mean  10 , SD 5 , and sample size  10
# ys with mean  12 , SD  5 , and sample size  10
# xl with mean  10, SD 5, and sample size 100
# yl with mean 12 , SD  5 , and sample size 100

pacman:: p_load(tidyverse)
rm(list =ls())
library(ggplot2)
library(tidyverse)

xs <- rnorm(n = 10, mean = 10, sd = 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)
xl <- rnorm(n = 100, mean = 10, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)


# Perform Welch’s t-test for xs vs. ys, and xl vs. yl and compare p-values. 
# Set var.equal = TRUE in t.test().

xs
ys
xl
yl

t.test(xs, ys, var.equal = TRUE)
# output 
# Two Sample t-test

# data:  xs and ys
# t = -0.67226, df = 18, p-value = 0.51
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.993647  3.087765
# sample estimates:
#   mean of x mean of y 
# 10.08729  11.54023 



t.test(xl, yl, var.equal = TRUE)

# output
# Two Sample t-test
# 
# data:  xl and yl
# t = -3.4357, df = 198, p-value = 0.0007202
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.880553 -1.050306
# sample estimates:
#   mean of x mean of y 
# 9.582963 12.048393 


# 10.5.2 Effects of Uncertainty -------------------------------------------

# We have four vectors - a1 and a2 & b1 and b2.

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# Perform the following analysis:
   
# Estimate sample means and SDs for each vector. 
# To do so, create a tibble() object with group (consist of characters a1, a2, b1, b2) 
# and value columns (consist of values above). 
# Then use group_by() and summarize() functions to estimate means and SDs for each group.


df_ab <-tibble(
  group = c(rep("a1", length (a1)),
            rep("a2", length (a2)),
            rep("b1", length(b1)), 
            rep("b2", length(b2))), 
  value = c(a1, a2, b1, b2)
)

df_ab


df_mu <- df_ab %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sig = sd(value))
df_mu


# another method

df_ab<- tibble(a1 = a1,
               a2 = a2, 
               b1 = b1, 
               b2 = b2) %>% 
  pivot_longer( 
    cols = everything(),
    names_to = "group", 
    values_to = "value")

df_ab

df_ab %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(
    aes(
      x = group, 
      y = value
    )
  ) + 
  geom_jitter( 
    height = 0, # must be zero!!!!
    width = 0.1, 
    alpha = 0.5 
    ) + 
  geom_segment(
    data = df_mu %>% 
      filter(group %in% c("a1", "a2")),
    aes(
      y = mu - sig, 
      yend = mu + sig
  )
    ) + 
  geom_point(
    data = df_mu %>% 
      filter(group %in% c("a1", "a2")),
    aes( y = mu), 
    size = 2.5
  )


t.test(a1, a2)
t.test(b1, b2)



# 10.5.3 Simulate null hypothesis -----------------------------------------

# The t-values under the equal-variance assumption can be calculated 
# using t.test(x, y, var.equal = TRUE)$statistic, where x and y are the two vectors being compared. 
# To simulate the distribution of t-values:

#   Assign mean(df_fl$length) to mu and sd(df_fl$length) to sig.


df_fl <- read_csv("data_src/data_fish_length.csv")
df_fl
mu <- mean(df_fl$length)
sig<-sd(df_fl$length)

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)
v<- t.test( x, y, var.equal = TRUE)$statistic


v <- NULL
R<- 100
for (i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(n = 50, mean = mu, sd = sig)
  v[i]<- t.test( x, y, var.equal = TRUE)$statistic
}


a <- df_fl %>% 
  filter(lake =="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a,b, var.equal = TRUE)$statistic

t_obs


# draw figure

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram() +
  geom_vline(xintercept = t_obs, 
             color = "red")+
  geom_vline(xintercept = -t_obs, 
             color ="blue")


mean(abs(v) > abs(t_obs))
t.test(a,b, var.equal = T)


# now to increase the number of simulations!

# with 1000 ---------------------------------------------------------------

df_fl <- read_csv("data_src/data_fish_length.csv")
df_fl
mu <- mean(df_fl$length)
sig<-sd(df_fl$length)

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)
v<- t.test( x, y, var.equal = TRUE)$statistic


v <- NULL
R<- 1000
for (i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(n = 50, mean = mu, sd = sig)
  v[i]<- t.test( x, y, var.equal = TRUE)$statistic
}


a <- df_fl %>% 
  filter(lake =="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a,b, var.equal = TRUE)$statistic

t_obs


# draw figure

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram() +
  geom_vline(xintercept = t_obs, 
             color = "red")+
  geom_vline(xintercept = -t_obs, 
             color ="blue")


mean(abs(v) > abs(t_obs))
t.test(a,b, var.equal = T)


# 50,000 ------------------------------------------------------------------

df_fl <- read_csv("data_src/data_fish_length.csv")
df_fl
mu <- mean(df_fl$length)
sig<-sd(df_fl$length)

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)
v<- t.test( x, y, var.equal = TRUE)$statistic


v <- NULL
R<- 50000
for (i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(n = 50, mean = mu, sd = sig)
  v[i]<- t.test( x, y, var.equal = TRUE)$statistic
}


a <- df_fl %>% 
  filter(lake =="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a,b, var.equal = TRUE)$statistic

t_obs


# draw figure

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram() +
  geom_vline(xintercept = t_obs, 
             color = "red")+
  geom_vline(xintercept = -t_obs, 
             color ="blue")


mean(abs(v) > abs(t_obs))
t.test(a,b, var.equal = T)


