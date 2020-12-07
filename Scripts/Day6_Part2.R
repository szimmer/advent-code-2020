##### THIS SOLUTION IS NOT CORRECT

library(tidyverse)
library(here)

input_in <- read_file(here("Input", "input_day6.txt"))

input <- str_replace_all(input_in, "\r", "")

if (str_ends(input, "\n")){
  input <- str_sub(input, 1, str_length(input)-2)
}


## split input whenever there is a blank line to show new group

input_parse <- str_split(input, pattern="\n\n", simplify = TRUE) %>%
  as.vector()

check_group <- function(char){

  # split by new line
  # collapse everything
  # find the unique characters
  char2 <- str_split(char, "\n", simplify = TRUE) %>%
    str_replace_all(" ", "") %>%
    str_split("") %>%
    map(unique) 
  
  # find the intersection among all people
  str_c(Reduce(intersect, char2), collapse="")  

}

input_test <- list(`1`=c("abc"),
                   `2`=c("a\nb\nc"),
                   `3`=c("ab\nac"),
                   `4`=str_c("a", sep="\n"),
                   `5`="b")
input_test %>% map_chr(check_group) %>% map_int(str_length)

uniques <- input_parse %>% map_chr(check_group) 
uniques %>%
  map_int(str_length) %>%
  sum()
