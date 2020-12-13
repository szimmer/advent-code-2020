library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day12.txt"), col_names = FALSE)

input <- input_in %>%
  mutate(Inst=str_sub(X1, 1, 1),
         Dist=parse_number(X1))

Inst <- input %>% pull(Inst)
Magnitude <- input %>% pull(Dist)

cur_pos <- c(0, 0)
facing <- 0 # E=0, N=90, W=180, S=270

for (i in 1:nrow(input)){
  if (Inst[i]=="N"){
    cur_pos[2] <- cur_pos[2] + Magnitude[i]
  } else if (Inst[i]=="S"){
    cur_pos[2] <- cur_pos[2] - Magnitude[i]
  } else if (Inst[i]=="E"){
    cur_pos[1] <- cur_pos[1] + Magnitude[i]
  } else if (Inst[i]=="W"){
    cur_pos[1] <- cur_pos[1] - Magnitude[i]
  } else if (Inst[i]=="L"){
    facing <- facing + Magnitude[i]
    facing <- facing %% 360
  } else if (Inst[i]=="R"){
    facing <- facing - Magnitude[i]
    facing <- facing %% 360
  } else if (Inst[i]=="F"){
    if (near(facing, 90)){
      cur_pos[2] <- cur_pos[2] + Magnitude[i]
    } else if (near(facing, 270)){
      cur_pos[2] <- cur_pos[2] - Magnitude[i]
    } else if (near(facing, 0)){
      cur_pos[1] <- cur_pos[1] + Magnitude[i]
    } else if (near(facing, 180)){
      cur_pos[1] <- cur_pos[1] - Magnitude[i]
    }
  }
}

cur_pos
facing
sum(abs(cur_pos))
