library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day3.txt"),
                       delim=" ", col_names = FALSE)

width <- input_in %>% slice(1) %>% pull(X1) %>% str_length()
length <- nrow(input_in)

input <- input_in %>%
  rename(X=X1) %>%
  mutate(Row=row_number()) %>%
  separate(X, sep=1:(width-1), into=str_c("X", 1:width)) %>%
  pivot_longer(-Row) %>%
  mutate(Column=parse_number(name))


Column <- seq(1, by=3, length.out=length)
Column <- Column %% width
Column[Column==0] <- width

Hits <- tibble(Row=1:length, Column=Column)

Hits %>% 
  left_join(input, by=c("Row", "Column")) %>%
  filter(value=="#") %>%
  nrow()

# not 25