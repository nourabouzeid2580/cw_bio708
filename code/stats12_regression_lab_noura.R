

pacman:: p_load(tidyverse)
rm(list =ls())
library(ggplot2)



# 12.4.1 Develop regression models
# R provides a built-in data set called iris. The iris data contain data points from 
# three different species (Species column). Split this data set by species (create three separate data frames) 
# and perform regression for each species separately to 
# analyze the relationship between Sepal.Width (response variable) and Petal.Width (explanatory variable).


head(iris)


v_sp <- unique(iris$Species)

v_sp

fit1 <- iris %>% 
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)
fit1

fit2 <- iris %>% 
  filter(Species == v_sp[2]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)
fit2


fit3 <- iris %>% 
  filter(Species == v_sp[3]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)
fit3


list_lm <- lapply(X = v_sp,
                  function(s) {
                    iris %>%  
                      filter(Species == s) %>% 
                      lm(Sepal.Width ~ Petal.Width, data = .)
                  }
                    )

list_lm

fit <- list(NULL)
for (i in 1:length(v_sp)) {
  fit[[i]] <- iris %>%  
    filter(Species == v_sp[i]) %>% 
    lm(Sepal.Width ~ Petal.Width, data = .)
  
  
}




# 12.4.2 Multiple explanatory variables

# Regression analysis can involve multiple explanatory variables.
# To explore this, consider utilizing Petal.Length as an additional explanatory variable for each species. 
# Then, investigate (1) the variations in estimates of regression coefficients and (2) the differences in the
# coefficients of determination compared to the model with only a single explanatory variable.


# lm()


fit1 <- iris %>% 
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

fit1



df_sp1 <- iris %>% 
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

df_sp1



list_fit_two <- lapply(
  X = v_sp,
 FUN = function(s) {
   iris %>% 
     filter(Species == s) %>% 
     lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)
 } 
)

list_lm[[1]] %>% summary()


iris_x <- iris %>% 
  mutate(useless = rnorm(150))

iris_x


m0<- lm(Sepal.Width ~ Petal.Width, data = iris_x)
m0

m1 <- lm(Sepal.Width ~ Petal.Width + useless, data = iris_x)
m1
