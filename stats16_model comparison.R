pacman:: p_load(tidyverse, patchwork)
rm(list =ls())
library(ggplot2)

# 16.1 Model Fit and Complexity
set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100
# true intercept and slope
b <- c(0.1, 0.5)
# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)
# create a design matrix
X <- model.matrix(~x1)
# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- X %*% b
# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)
# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point(
    color = "red"
  )

# lm fit 
# correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)
summary(m1)

# add a column x2 which is irrelevant for y
df0 <- df0 %>% 
  mutate(x2 = rnorm(n))

# add x2 to the model
m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)

# extract r^2 values 
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared


# non-normal case ---------------------------------------------------------

df_count <-read_csv("data_src/data_garden_count.csv") %>% 
  mutate(moisture = runif(nrow(.), 0, 100))
df_count

m_pois0 <- glm(count~nitrate, 
                data = df_count, 
                family= "poisson")
s_pois0 <- summary(m_pois0)
m_pois1 <- glm(count ~ nitrate + moisture, 
               data = df_count, 
               family = "poisson")
s_pois1 <- summary(m_pois1)

# for model 0 with nitrate only
pseudoR0 <- 1 - s_pois0$deviance / s_pois0$null.deviance
# to account for complexity 
adj.pseudoR0 <- 1 - (s_pois0$deviance - 1) / s_pois0$null.deviance

# for model 1 with nitrate + moisture
pseudoR1 <- 1 - s_pois1$deviance / s_pois1$null.deviance
# to account for complexity 
adj.pseudoR1 <- 1 - (s_pois1$deviance - 2) / s_pois1$null.deviance


# likelihood ratio test  --------------------------------------------------

## normal distribution case - Liklihood test needs to be nested 
anova(m1, m2, test = "Chisq")

## poisson model case
anova(m_pois0, m_pois1, test = "Chisq")

# AIC ---------------------------------------------------------------------

## out-of-sample prediction - AIC does not need to be nested
## AIC = 2k - 2 ln L 

# get AIC value from a model 
# normal distribution case
AIC(m1)
AIC(m2)

# poisson case 
AIC(m_pois0)
AIC(m_pois1)
# AIC predication model not very useful for the high predictive capacity, 
# or provide causality for experimental data such as that in cell and molecular exps. 
# when you see |AIC1 - AIC2| < 2 their models predictive capacities are meaningfully different