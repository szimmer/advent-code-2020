library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day8.txt"), delim=" ", 
                       col_names = FALSE)

intcode <- function(input){
  inst <- pull(input, X1)
  instval <- pull(input, X2)
  
  hist <- NULL
  stopit <- FALSE
  idx <- 1
  accumulator <- 0
  listlen <- length(inst)
  
  while (!stopit){
    if (inst[idx]=="acc"){
      accumulator <- accumulator + instval[idx]
      idx <- idx + 1
    } else if (inst[idx]=="jmp"){
      idx <- idx + instval[idx]
    } else if (inst[idx]=="nop"){
      idx <- idx + 1
    }
    
    if (idx %in% hist){
      stopit <- TRUE
      infloop <- TRUE
    } else if (idx==(listlen+1)) {
      stopit <- TRUE
      infloop <- FALSE
    } else if (idx > (listlen+1)){
      stopit <- TRUE
    } else{
      hist <- c(hist, idx)
    }
  }
  
  tibble(accumulator=accumulator, infloop=infloop)
}

change_it <- function(i, input=input_in){
  if (input %>% slice(i) %>% pull(X1) %in% c("nop", "jmp")){
    input %>%
      mutate(
        X1=case_when(
          row_number()==i & X1=="nop"~"jmp",
          row_number()==i & X1=="jmp"~"nop",
          TRUE~X1
        )
      ) %>%
      intcode() %>%
      return()
  } else{
    input %>%
      intcode %>%
      return()
  }
}

out <- 1:nrow(input_in) %>% map_df(change_it, .id="ID")

out %>%
  filter(!infloop)
