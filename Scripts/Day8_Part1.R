library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day8.txt"), delim=" ", 
                       col_names = FALSE)

inst <- pull(input_in, X1)
instval <- pull(input_in, X2)

hist <- NULL
stopit <- FALSE
idx <- 1
accumulator <- 0

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
  } else{
    hist <- c(hist, idx)
  }
}