library(dplyr)
library(readr)
library(rstanarm)

df <- read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data', col_names = FALSE)

nombres <- read_csv("~/Documents/Computer Science/Generalized Linear Models/Trabajo Final/nombres.txt", col_names = FALSE)

names(df) <- nombres %>% 
  mutate(
    var_names = gsub(
      "(.*) (.*) (.*)", 
      "\\2",
      X1
    )
  ) %>% 
  pull(var_names)

df %>% 
  select(state, ViolentCrimesPerPop) %>% 
  na.omit

glm()