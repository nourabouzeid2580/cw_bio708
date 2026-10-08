
pacman:: p_load(tidyverse)
rm(list =ls())
library(ggplot2)
library(tidyverse)

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake)

# geom_violin() - function for violin plots
# geom_jitter() - jittered points

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + 
  # transparency
  geom_jitter(height = 0,
              width = 0.1,
              alpha = 0.2)


# 11.1.1 Between-group variability

# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

print(df_g)

s_b <- sum(df_g$ss)
print(s_b)


#11.1.2 Within-group variability
# within group variability 

df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

# deviation from group mean for each fish

# filter() & slice(): show first 3 rows each group
print(df_i %>% filter(lake == "a") %>% slice(1:3))

print(df_i %>% filter(lake == "b") %>% slice(1:3))

print(df_i %>% filter(lake == "c") %>% slice(1:3))


df_i_g <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))

print(df_i_g)

s_w <- sum(df_i_g$ss)
print(s_w)


# summary(aov(length~lake, df_anova))
# In console 

# 11.1.3 Variability to Variance


# n_distinct() count the number of unique elements
n_g <- n_distinct(df_anova$lake)
s2_b <- s_b / (n_g - 1)
print(s2_b)


s2_w <- s_w / (nrow(df_anova) - n_g)
print(s2_w)


f_value <- s2_b / s2_w
print(f_value)

# 11.2.2 Null Hypothesis

x <- seq(0, 10, by = 0.1)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "chocolate") # observed F-statistic

# pf() estimate the probability of less than q
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)



# anova in r --------------------------------------------------------------

# first argument is formula
# second argument is data frame for reference
# do not forget specify data = XXX! aov() refer to columns in the data frame
m <- aov(formula = length ~ lake,
         data = df_anova)

print(m)

## Call:
##    aov(formula = length ~ lake, data = df_anova)

summary(m)
