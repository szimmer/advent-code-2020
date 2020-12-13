library(here)
library(tidyverse)

input_in <- read_file(here("Input", "input_day13.txt"))
# input_in <- read_file(
# "939
# 7,13,x,x,59,x,31,19"
# )

input_split <- str_split(input_in, "\n", simplify = TRUE) %>%
  as.vector()

departure_time <- input_split[1] %>% as.numeric()

busses <- 
  str_replace_all(input_split[2], ",x", "") %>%
  str_split(pattern = ",", simplify = TRUE) %>%
  as.numeric() 
  
prevbus <- (departure_time %/% busses)*busses
nextbus <- prevbus+busses
bestbus <- which.min(nextbus-departure_time)

(nextbus[bestbus]-departure_time)*busses[bestbus] 

