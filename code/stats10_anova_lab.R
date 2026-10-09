# lab 

pacman:: p_load(tidyverse, pwr)
rm(list =ls())
library(ggplot2)
library(tidyverse)

df_pg <- as_tibble(PlantGrowth)

df_pg %>% 
  mutate(
    group_label = case_when(
      group == "ctrl" ~ "Control",
      group == "trt1" ~ "Treatment 1",
      group == "trt2" ~ "Treatment 2"
    )
  ) %>% 
  ggplot(aes(
    x = group_label,
    y = weight
  )) +
  geom_violin(
    draw_quantiles = 0.5,
    alpha = 0.2
  ) + 
  geom_jitter(
    width = 0.2,
    alpha = 0.2
  ) + 
  labs(
    x = "Treatment group",
    y = "Weight"
  ) +
theme_classic()

fit <- aov(weight ~ group, data = df_pg)


# summary(fit)


# power analysis
pwr::pwr.anova.test(
  k = 3,
  f = 0.5, 
  sig.level = 0.05,
  power = 0.8
)


pwr::pwr.anova.test(
  k = 3,
  n = 10,
  f = 0.5, 
  sig.level = 0.05,
)

# my code examples
pwr::pwr.anova.test(
  k = 10,
  n = 3,
  f = 0.5, 
  sig.level = 0.05,
)


pwr::pwr.anova.test(
  k = 8,
  n = 2,
  f = 0.5, 
  sig.level = 0.05,
)

pwr::pwr.anova.test(
  k = 10,
  f = 0.5, 
  sig.level = 0.05,
  power = 0.8
)
