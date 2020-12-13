library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day12.txt"), col_names = FALSE)

input <- input_in %>%
  mutate(Inst=str_sub(X1, 1, 1),
         Dist=parse_number(X1))

input %>%
  filter(Inst %in% c("L", "R")) %>% 
  count(Dist)
#90, 180, 270

Inst <- input %>% pull(Inst)
Magnitude <- input %>% pull(Dist)

cur_pos <- c(0, 0)
waypoint_pos <- c(10, 1)
facing <- 0 # E=0, N=90, W=180, S=270


for (i in 1:nrow(input)){
  if (Inst[i]=="N"){
    waypoint_pos[2] <- waypoint_pos[2] + Magnitude[i]
  } else if (Inst[i]=="S"){
    waypoint_pos[2] <- waypoint_pos[2] - Magnitude[i]
  } else if (Inst[i]=="E"){
    waypoint_pos[1] <- waypoint_pos[1] + Magnitude[i]
  } else if (Inst[i]=="W"){
    waypoint_pos[1] <- waypoint_pos[1] - Magnitude[i]
  } else if (Inst[i] %in% c("L", "R") & Magnitude[i]==180){
    waypoint_pos <- -waypoint_pos
  } else if ((Inst[i] == "L" & Magnitude[i]==90 )| (Inst[i] == "R" & Magnitude[i]==270)){
    waypoint_pos <- c(-waypoint_pos[2], waypoint_pos[1])
  } else if ((Inst[i] == "L" & Magnitude[i]==270)| (Inst[i] == "R" & Magnitude[i]==90 )){
    waypoint_pos <- c(waypoint_pos[2], -waypoint_pos[1])
  } else if (Inst[i]=="F"){
    cur_pos <- cur_pos + Magnitude[i]*waypoint_pos
  }
}

cur_pos
waypoint_pos
facing
sum(abs(cur_pos)) 
