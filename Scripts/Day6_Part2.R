##### THIS SOLUTION IS NOT CORRECT

library(tidyverse)
library(here)

input_in <- read_file(here("Input", "input_day6.txt"))


## split input whenever there is a blank line to show new group
## remove the new lines

input_parse <- str_split(input_in, pattern="\n\n", simplify = TRUE) %>%
  as.vector()

check_group <- function(char){
  char2 <- str_split(char, "\n", simplify = TRUE) %>%
    str_replace_all(" ", "") %>%
    str_split("") %>%
    map(unique)
  
  if (length(char2)>1){
    char3 <- char2[[1]]
    for (i in 2:length(char2)){
      char3 <- intersect(char3, char2[[i]])
    }
  } else {
    char3 <- char2[[1]]
  }
  str_c(sort(char3), collapse="")
}

input_test <- list(`1`=c("abc"),
                   `2`=c("a", "b", "c"),
                   `3`=c("ab", "ac"),
                   `4`=rep("a", 4),
                   `5`="b")
input_test %>% map_chr(check_group) 

uniques <- input_parse %>% map_chr(check_group) 
uniques %>%
  map_int(str_length) %>%
  sum()
# 3484 is too low