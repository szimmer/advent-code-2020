library(tidyverse)
library(here)

input_in <- read_delim(here("Input", "input_day2.txt"),
                       delim=" ", col_names = FALSE)

pwd_check <- input_in %>%
  rename(letter=X2, pwd=X3) %>%
  separate(X1, into=c("minCount", "maxCount"), sep="-", convert=TRUE) %>%
  mutate(letter=str_sub(letter, 1, 1), 
         count_letter = str_count(pwd, letter), 
         pass = count_letter >= minCount & count_letter <= maxCount
         )

pwd_check %>% pull(pass) %>% sum()
