library(here)
library(tidyverse)

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

occ_nbrs_cnt <- function(i, j, layout){
  rows <- max(i-1, 1):min(i+1, length)
  cols <- max(j-1, 1):min(j+1, width)
  nbrs_idx <- expand_grid(row=rows, col=cols) %>%
    filter(!(row == i & col == j)) %>%
    as.matrix()
  
  nbrs <- layout[nbrs_idx]
  
  sum(nbrs, na.rm=TRUE)
  
}

change_seats <- function(cur_layout){
  next_layout <- cur_layout 
  for (i in 1:nrow(cur_layout)){
    for (j in 1:ncol(cur_layout)){
      if (is.na(cur_layout[i, j])){
        # do nothing
      } else if (cur_layout[i , j]==0){ # seat is empty
        n_occ_nbrs <- occ_nbrs_cnt(i, j, cur_layout)
        # print(glue::glue("i={i}, j={j}, n_occ_nbrs={n_occ_nbrs}"))
        if (n_occ_nbrs==0){
          next_layout[i, j] <- 1
          }
      } else if (cur_layout[i, j]==1){ # seat is occupied
        n_occ_nbrs <- occ_nbrs_cnt(i, j, cur_layout)
        if (n_occ_nbrs >= 4){
          next_layout[i, j] <- 0
          }
      }
    }
  }
  return(next_layout)
}

cur_plan <- input
change <- TRUE
shuffles <- 0

while (change){
  next_plan <- change_seats(cur_plan)
  check.equal <- all.equal(cur_plan, next_plan)
  if (isTRUE(check.equal)){
    change <- FALSE
  } else{
    cur_plan <- next_plan
    shuffles <- shuffles + 1
  }
}


sum(cur_plan, na.rm=TRUE)
