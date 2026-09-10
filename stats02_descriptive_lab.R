
# Descriptive Statistics - Lab --------------------------------------------


# Central Tendency --------------------------------------------------------

library(tidyverse)


# Create a new vector z with length  100

z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))
z

# as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.

# arithmetic mean
(mu_z <- sum(z) / length(z))
mean(z)

# geometric mean 
exp(sum(log(z)) / length(z))

(mu_z_geo <- prod(z) ^ ( 1/ length(z)))


# median
med_z <- median(z)
med_z

# Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

df_z <- tibble(z=z)

  (g_hist <- df_z %>% 
      ggplot(aes(x = z)) + 
      geom_histogram())

# Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors 
# using a function geom_vline() .

g_hist + 
  geom_vline(xintercept = mu_z,
             color = 'pink') +
  geom_vline(xintercept = mu_z_geo, 
             color = "red") + 
  geom_vline(xintercept = med_z,
             color = "blue")

# Visually compare the values of the central tendency measures with the vertical lines drawn by geom_vline().

# 

# Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 – 4.

z_rev <- -z +max(z) + 0.5
mu_z_1 <- mean(z_rev)
mu_z_geo_1 <- exp(mean(log(z_rev)))
med_z_1 <- median(z_rev)

df_zr <- tibble(z_rev=z_rev)

(g_hist <- df_zr %>% 
    ggplot(aes(x = z_rev)) + 
    geom_histogram())

 (g_hist <- df_zr %>% 
                   ggplot(aes(x = z_rev)) + 
                   geom_histogram()) + geom_vline(xintercept = mu_z_1) +
  geom_vline(xintercept = mu_z_geo_1, 
             color = "purple") + 
  geom_vline(xintercept = med_z_1,
             color = "salmon") 

# Variation -------------------------------------------------------------------------

w <- rnorm(100, mean = 10, sd =1)
head(w) 

# Convert the unit of w to “milligram” and create a new vector m.
m <- 1000 * w

# Calculate SD and MAD for w and m.

sd_w <- sqrt(sum((w - mean(w))^2) / length(w))
sd_w
(mad_w <- median(abs(w - median(w))))
mad_w


sd_m <- sqrt(sum((m - mean(m))^2) / length(m))
sd_m
(mad_m <- median(abs(m - median(m))))
mad_m



# Calculate CV and MAD/Median for w and m.

sd_w / mean(w)
sd_m / mean(m)

mad_w / median(w)
mad_m / median(m)



