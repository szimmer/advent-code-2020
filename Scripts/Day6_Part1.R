library(tidyverse)
library(here)

input_in <- read_file(here("Input", "input_day6.txt"))


## split input whenever there is a blank line to show new group
## remove the new lines
## remove all blanks
## split at every character, now we have a vector for each group of their selects
## find the unique in each group
## find the length of the unique in each group
## take the sum

str_split(input_in, pattern="\n\n", simplify =  TRUE) %>% 
  str_replace_all("\n", " ") %>%
  str_replace_all(" ", "") %>%
  str_split("") %>%
  map(unique) %>%
  map_int(length) %>%
  sum()
