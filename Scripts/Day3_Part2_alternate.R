library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day3.txt"),
                       delim=" ", col_names = FALSE)

width <- input_in %>% slice(1) %>% pull(X1) %>% str_length()
length <- nrow(input_in)

input <- input_in %>%
  rename(X=X1) %>%
  separate(X, sep=1:(width-1), into=str_c("X", 1:width)) %>%
  as.matrix()

down_the_hill <- function(right, down){
  Row <- seq(1, by=down, to = length)
  
  Column <- seq(1, by=right, length.out=length(Row))
  Column <- ((Column-1) %% width)+1
  
  sum(input[cbind(Row, Column)]=="#")
}

trees <- map2_int(c(1, 3, 5, 7, 1), c(1, 1, 1, 1, 2), down_the_hill)

prod(trees)
