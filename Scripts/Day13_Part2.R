library(here)
library(tidyverse)

input_in <- read_file(here("Input", "input_day13.txt"))
# input_in <- read_file(
# "939
# 7,13,x,x,59,x,31,19"
# )

input_split <- str_split(input_in, "\n", simplify = TRUE) %>%
  as.vector()
busses_v <- str_split(input_split[2], pattern=",", simplify=TRUE) %>%
  as.vector()

input_bus <- tibble(bus = busses_v) %>%
  mutate(
    bus=str_replace_all(bus, "x", NA_character_),
    bus=as.numeric(bus),
    Ord=row_number()
  ) %>%
  filter(!is.na(bus))

busses <- input_bus %>% pull(bus)
ts <- input_bus %>% pull(Ord)

time0 <- 100000000000000
remain <- TRUE
while(remain){
  if (time0 %% 1000 == 0){print(sprintf("%013.000f", time0))}
  if (!all(near((ts+time0) %% busses, 0))){
    time0 <- time0 + 1
  } else{
    time0 <- time0 + 1
    remain <- FALSE
  }
  
}

sprintf("%020.000f", time0) # too low
