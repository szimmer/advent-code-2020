library(here)
library(tidyverse)

get_row_col <- function(seat){
  seat_row <- str_sub(seat, 1, 7) %>%
    str_split(pattern="", simplify=TRUE) %>%
    as.vector()
  seat_col <- str_sub(seat, 8) %>%
    str_split(pattern="", simplify=TRUE) %>%
    as.vector()
  
  rows <- 0:127
  cols <- 0:7
  for (bin in seat_row){
    lr <- length(rows)
    if (bin=="F"){
      rows <- rows[1:(lr/2)]
    } else{
      rows <- rows[(lr/2+1):lr]
    }
  }
  
  for (bin in seat_col){
    lc <- length(cols)
    if (bin=="L"){
      cols <- cols[1:(lc/2)]
    } else{
      cols <- cols[(lc/2+1):lc]
    }
  }
  
  out <- tibble(row=rows, col=cols)
  return(out)
}

input_in <- read_csv(here("Input", "input_day5.txt"),
                     col_names = FALSE)

input_test <- tibble(X1=c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"))

input_test$X1 %>%
  map_df(get_row_col) %>%
  mutate(seat_id=row*8+col)

scanned_passes <- input_in$X1 %>%
  map_df(get_row_col) %>%
  mutate(seat_id=row*8+col) 

seat_seq <- tibble(seat_id=seq(min(scanned_passes$seat_id), max(scanned_passes$seat_id)))

scanned_passes %>%
  mutate(Scanned=1) %>%
  full_join(seat_seq, by="seat_id") %>%
  arrange(seat_id) %>%
  mutate(PrevScanned=lag(Scanned)==1,
         NextScanned=lead(Scanned==1)) %>%
  filter(is.na(Scanned)) 
  