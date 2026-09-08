#Descriptive Statistics 

#call packages everytime
library(tidyverse)

#7.1.2 R Exercise

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

#run to check
x
y


# Arithmetic mean ---------------------------------------------------------


# arithmetic mean code = sum(vector) / length(vector) or mean(vector)
# adding () for all code automatically prints
#arithmetic mean always larger
(mu_x <- sum(x) / length(x))
mu_x
mean(x)

(mu_y <- sum(y) / length(y))
mu_y
mean(y)


# Geometric Mean ----------------------------------------------------------

#geometric mean code = (geo_vector <- prod(vector) ^ ( 1 / length(x))) or = exp(sum(log(vector) / length(y)))
prod(x) #large number 
#way one
(mu_x_geo <- prod(x) ^ ( 1/ length(x)))
#way 2
exp(sum(log(x)) / length(x))

#now for vector y 
(mu_y_geo <- prod(y) ^ ( 1/ length(y)))
exp(sum(log(y)) / length(y))



# Median -------------------------------------------------------------------


# median =  insensitive to outliers 
med_x <- median(x)
med_y <- median(y)
med_x
med_y

#7.2.1 Variation Measures = sensitive to outliers 


# Central Tendency -------------------------------------------------------- 
#ctrl +shift + R


# Variance ----------------------------------------------------------------

#variance
(x - mean(x))^2

# then take sum
(sum((x - mean(x))^2))

#then divide by length(vector)
var_x <- (sum((x - mean(x))^2) / length(x))
var_x


#now for vector y 

(y - mean(y))^2

sum((y - mean(y))^2)

var_y <- sum(sum((y - mean(y))^2) / length(y))
var_y


# Standard Deviation ------------------------------------------------------

#standard deviation

sqrt(var_x)
sqrt(var_y)


# InterQuartile Range -----------------------------------------------------

# 25% 
x_lower <- quantile(x, 0.25)
x_lower

# 75%
x_higher <- quantile(x, 0.75)
x_higher

# IQR
iqr_x <- x_higher - x_lower
iqr_x

#can make the quartile ranges as a vector and then call them and subtract them
y_q <- quantile (y, c(0.25, 0.75))
y_q
iqr_y <- y_q[2] - y_q[1]
iqr_y

# y_q[2] = 0.75 !
# y_q[1] = 0.25 !


# MAD ---------------------------------------------------------------------
#absolute deviation
# then take median of it 

ad_x <- abs(x-median(x))
ad_x
median(ad_x)

# for y
mad_y <- median(abs(y - median(y)))
print(mad_y)


# Coefficient of Variation  -----------------------------------------------

sd_x <- sqrt(var_x)
sd_x

cv_x <- sd_x /mu_x
cv_x


# Extra -------------------------------------------------------------------
# can make your own/define your own function and save it!
mycv <- function(v) {
  
  mu <- mean(v)
  s <- sd(v)
  cv <- s / mu 
  
  return(cv)
} 

#run
mycv
#can use your code as a function! and it runs the same way
mycv(x)
