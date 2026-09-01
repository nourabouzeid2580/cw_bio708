library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#filter

filter(iris_sub, Species == "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Species != "virginica")

filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

filter(iris_sub, Sepal.Length > 5)

filter(iris_sub, Sepal.Length >= 5)

filter(iris_sub, Sepal.Length < 5)

filter(iris_sub, Sepal.Length <= 5)

# Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")

# same; "," works like "&"
filter(iris_sub,
       Sepal.Length < 5, Species == "setosa")
# Either Sepal.Length is less than 5 OR Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 | Species == "setosa")

arrange(iris_sub, Sepal.Length)

arrange(iris_sub, desc(Sepal.Length))

#select

select(iris_sub, Sepal.Length)

select(iris_sub, c(Sepal.Length, Sepal.Width))

select(iris_sub, -Sepal.Length)

select(iris_sub, -c(Sepal.Length, Sepal.Width))

# select columns starting with "Sepal"
select(iris_sub, starts_with("Sepal"))

# remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))

# select columns ending with "Sepal"
select(iris_sub, ends_with("Width"))

# remove columns ending with "Sepal"
select(iris_sub, -ends_with("Width"))

# mutate
# nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))

# create a vector from 1 to x_max
x <- 1:x_max

# add as a new column
# named `x` as `row_id` when added
mutate(iris_sub, row_id = x)

# twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

iris_sub_row <- mutate(iris_sub, row_id = x)

iris_sub

#Pipe %>% 

df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

print(df_vir_sl)

#select(filter(iris_sub, Species == "virginica"), Sepal.Length)

iris_sub %>% 
  filter(Species == "virginica")
  #select(Sepal.Length)
  print(df_vir_sl)
  
  print(df_vir_sl)
  
#excersise 5.3.2
iris_pipe <- filter(iris_sub, Species == "setosa") %>% 
  mutate( pw_two_times = 2*Petal.Width)

#iris_pipe <- mutate(iris_pipe, row_id = x) # not needed iris_pipe/iris sub inside mutate 

print(iris_pipe) 


# Grouping 
iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()

#Summarize
iris_sub %>% 
  group_by(Species) %>% # gives species name and means
  summarize(mu_sl = mean(Sepal.Length))


  