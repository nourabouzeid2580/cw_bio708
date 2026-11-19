# model comparison lab 
rm(list =ls())
library(ggplot2)
pacman::p_load(tidyverse, 
               patchwork, 
               palmerpenguins, 
               janitor, 
               MuMIn)

# clean names in penguins_raw
df_pen0 <- penguins_raw %>% 
  clean_names()

# convert Yes/No to 1/0 info
# can do it seperately like this 
sp <- unique(df_pen0$species)

df_pen <- df_pen0 %>% 
  mutate(clutch_completion =ifelse(clutch_completion == "Yes", 
                                   1, 0), # convert Yes/No to 1/0 info
         # simplify species name
         species = case_when(species == sp[1] ~ "adelie", 
                             species == sp[2] ~ "gentoo", 
                             species == sp[3] ~ "chistrap") 
  ) %>% 
  drop_na(
    culmen_length_mm, 
    culmen_depth_mm, 
    flipper_length_mm, 
    body_mass_g
  )
df_pen

# After formatting the data, our next step is to perform the following:
# Develop a statistical model that explains Clutch Completion using the variables Species,
# Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm), Body Mass (g), and Sex. 
# Use an appropriate probability distribution for this model.
# Perform an AIC-based model selection using the MuMIn::dredge() function.
# This process will help us identify the variables that are most important for predicting the 
# clutch completion status.

m <- glm(clutch_completion ~ 
           species + 
           culmen_length_mm + 
           culmen_depth_mm + 
           flipper_length_mm + 
           body_mass_g, 
          data = df_pen, 
          family = "binomial") 

options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2)

list_m <- get.models(object = m_set, subset = delta > 2)



