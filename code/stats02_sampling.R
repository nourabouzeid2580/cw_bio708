# Sampling

library(tidyverse)

rm(list = ls()) # removes everything from the previous session and works as a new objects

#create vector

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit
print(df_h1)

# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 

#creating columns and rows
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

#another one

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

# load csv data on R
read_csv("data_src/data_plant_height.csv")
df_h0 <- read_csv("data_src/data_plant_height.csv")

# show the first 10 rows
print(df_h0)


mu <- mean(df_h0$height)

sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)

print(sigma2)


# #random selection -------------------------------------------------------

df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)


df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)



# for loop ----------------------------------------------------------------

#randomly sample 10 individuals 


df_i <- df_h1 %>% 
  sample_n(size = 10)

print(df_i)

#mean for subset
mu_i <- mean(df_i$height)

#variance
var_i <- sum((df_i$height - mu_i)^2 /nrow(df_i))

# loop --------------------------------------------------------------------

# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:1000) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

#install.packages("patchwork") # install only once
library(patchwork) 

# visualization -----------------------------------------------------------

df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_mu

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_var

# layout vertically
# possible only if "patchwork" is loaded

library(patchwork) 
g_mu / g_var

#how patchwork works 
g_hor <- g_mu + g_var 
g_ver <- g_mu / g_var

g_hor
g_ver

# bias corrected version --------------------------------------------------

var_ub_i <- NULL

for (i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  var_ub_i[i] <- var(df_i$height)
}

df_sample <- df_sample %>% 
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_mu / g_var_ub / g_var_ub

# working one -------------------------------------------------------------

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(
    aes(limits= c(
      min(c(var_hat, var_ub_hat)),
      max(c(var_hat, var_ub_hat))
    ))
  )

g_mu / g_var / g_var_ub

