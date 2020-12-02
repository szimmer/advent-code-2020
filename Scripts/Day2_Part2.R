library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day2.txt"),
                       delim=" ", col_names = FALSE)

pwd_check <- input_in %>%
  rename(letter=X2, pwd=X3) %>%
  separate(X1, into=c("pos1", "pos2"), sep="-", convert=TRUE) %>%
  mutate(letter=str_sub(letter, 1, 1),
         test1 = str_sub(pwd, pos1, pos1)==letter,
         test2 = str_sub(pwd, pos2, pos2)==letter,
         pass = xor(test1, test2)
         )

pwd_check %>% pull(pass) %>% sum()
