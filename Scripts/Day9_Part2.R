library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day9.txt"), col_names = FALSE) 

input <- input_in %>% pull(X1) 

prevnum <- 25
listlen <- length(input)

for (i in (prevnum+1):listlen){
  prev25 <- input[i-1:prevnum]
  
  diff <- input[i] - prev25
  valid <- any(diff %in% prev25 & !near(diff, prev25))
  
  if(!valid){
    invalid_num <- input[i]
    print(invalid_num)
    break
  }
}

ans <- NULL
for (startpoint in 1:(listlen-1)){
  keeploop <- TRUE
  endpoint <- startpoint + 1
  while (keeploop){
    sumit <- sum(input[startpoint:endpoint])
    if (near(sumit, invalid_num)){
      ans <- input[startpoint:endpoint]
      print(ans)
      keeploop <- FALSE
      break
    } else if (sumit < invalid_num){
      endpoint <- endpoint + 1
    } else if (sumit > invalid_num | endpoint==listlen){
      keeploop <- FALSE
      break
    }
  }
  if (!is.null(ans)) break
}
startpoint
endpoint
sum(min(ans), max(ans)) ### answer

