library(here)
library(tidyverse)
library(simecol)

# input_in <- read_csv(
# "L.LL.LL.LL
# LLLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLLL
# L.LLLLLL.L
# L.LLLLL.LL",
# col_names = FALSE
# )

input_in <- read_csv(here("Input", "input_day11.txt"), col_names = FALSE)

width <- input_in %>% slice(1) %>% pull(X1) %>% str_length()
length <- nrow(input_in)

make_num <- function(x){
  if_else(x=="L", as.integer(0), NA_integer_)
}

input <-
  input_in %>%
  separate(X1, into=str_c("X", into=1:width), sep=1:(width-1)) %>%
  mutate_all(make_num) %>%
  as.matrix()

occ_nbrs_cnt <- function(mat) {

  mat[is.na(mat)] <- 0
  eightneighbors(mat)
  
}


change_seats <- function(cur_layout){
  nnbrs <- occ_nbrs_cnt(cur_layout)
  next_layout <- cur_layout
  next_layout[cur_layout==0 & nnbrs==0] <- as.integer(1)
  next_layout[cur_layout==1 & nnbrs>=4] <- as.integer(0)
  
  return(next_layout)
}

cur_plan <- input
change <- TRUE
shuffles <- 0

while (change){
  next_plan <- change_seats(cur_plan)
  check.equal <- all(near(abs(cur_plan-next_plan), 0), na.rm = TRUE)
  if (check.equal){
    change <- FALSE
  } else{
    cur_plan <- next_plan
    shuffles <- shuffles + 1
  }
}


sum(cur_plan, na.rm=TRUE)
