library(tidyverse)
library(here)

input_in <- read_file(here("Input", "input_day4.txt"))

pass_info_vec <- str_split(input_in, pattern="\n\n", simplify =  TRUE) %>%
  as.vector() %>%
  str_replace_all("\n", " ")

pass_info_df <- tibble(pass_glob=str_trim(pass_info_vec)) %>%
  rowid_to_column("ID") %>%
  separate(pass_glob, into=str_c("X", 1:10), fill="right", sep=" ") %>%
  pivot_longer(cols=-ID, values_drop_na = TRUE) %>%
  select(-name) %>%
  separate(value, into=c("field", "value"), sep=":") %>%
  pivot_wider(id_cols=ID, names_from=field, values_from=value)

pass_info_df

pass_info_df %>%
  select(-cid) %>%
  filter(complete.cases(.)) %>%
  nrow()
