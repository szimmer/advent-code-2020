library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day9.txt"), col_names = FALSE) 

input <- input_in %>% pull(X1) 

listlen <- nrow(input_in)



for (i in 26:listlen){
  prev25 <- input[i-1:25]
  
  diff <- input[i] - prev25
  valid <- any(diff %in% prev25 & !near(diff, prev25))
  
  if(!valid){
    print(input[i])
    break
  }
}


