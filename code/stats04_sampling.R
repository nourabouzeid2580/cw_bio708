# 8.3 Laboratory


# #making sample 50 
# df_50 <- df_h0 %>% 
#   sample_n(size= 50)
# 
# df_50
# 
# #making sample 100 
# df_100 <- df_h0 %>% 
#   sample_n(size= 100)
# 
# df_100

# We used 10 plants to estimate sample means and variances. 
# Obtain 100 sub-datasets with 50 and 100 measures each, 

# load csv data on R
library(tidyverse)
rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

read_csv("data_src/data_plant_height.csv")

df_h0 <- read_csv("data_src/data_plant_height.csv")

mu_i_50 <- mu_i_100 <- var_i_50 <- var_i_100 <- NULL


for (i in 1:100) { 
  df_50 <- df_h0 %>%
    sample_n(size = 50)
  
  df_100 <- df_h0 %>%
    sample_n(size = 100)
  
  mu_i_50[i] <- mean(df_50$height)
  mu_i_100[i] <- mean(df_100$height)
  
  var_i_50[i] <-var(df_50$height)
  var_i_100[i] <-var(df_100$height)
  
}


df_sample <- tibble(mu50 = mu_i_50, 
                    mu100 = mu_i_100,
                    var50 = var_i_50,
                    var100 = var_i_100)

g50mu <- df_sample %>% 
  ggplot (aes(x = mu50)) +
  geom_histogram() + 
  scale_x_continuous(limits = c(10,25))

g50mu

g100mu <- df_sample %>% 
  ggplot (aes(x = mu100))+
  geom_histogram() + 
  scale_x_continuous(limits = c(10,25))

g100mu

g50var <- df_sample %>% 
  ggplot (aes(x = var50))+
  geom_histogram() + 
  scale_x_continuous(limits = c(10,50))

g50var

g100var <- df_sample %>% 
  ggplot (aes(x = var100)) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(10,50))

g100var

(g50mu / g100mu ) | (g50var / g100var)

df_h10 <- df_h0 %>% 
  filter(height >= 10)



# and draw histograms of sample means and unbiased variances (use var()).

# g50mu <- df_sample %>% 
#   ggplot (aes(x = mu50)) + 
#   geom_histogram() + 
#   scale_x_continuous(limits = c(10,25))
# 
# 
# g100mu <- df_sample %>% 
#   ggplot (aes(x = mu100)) + 
#   geom_histogram() + 
#   scale_x_continuous(limits = c(10, 50))
# 
# g100mu
# 
# g50var <- df_sample %>% 
#   ggplot (aes(x = var50)) + 
#   geom_histogram() + 
#   scale_x_continuous(limits = c(10,25))
# 
# g50var
# 
# g100var <- df_sample %>% 
#   ggplot (aes(x = var100)) + 
#   geom_histogram() + 
#   scale_x_continuous(limits = c(10,25))
# 
# g100var


# Sample means and unbiased variances are unbiased if samples are randomly selected. 

# What happens if samples are non-random? Suppose the investigator was unable to find plants less than 10 cm in height – 
# the following code excludes those less than 10 cm in height:
  
 
# Repeat step 1 with df_h10 instead of df_h0 and compare the results.

# df_h --------------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)




#mean for subset


#variance
