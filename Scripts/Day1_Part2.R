library(here)
library(tidyverse)

num_in <- read_csv(here("Input", "input_day1.txt"), col_names = FALSE)

num_cross <- expand_grid(x=num_in$X1, y=num_in$X1, z=num_in$X1)

num_cross %>%
  mutate(Sum=x+y+z,
         Product=x*y*z) %>%
  filter(near(Sum, 2020))
