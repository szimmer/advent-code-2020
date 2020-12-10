library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day10.txt"), col_names = FALSE) 

ans1_tab <- input_in %>% 
  add_row(X1=c(0, max(input_in$X1)+3)) %>% #add the charging outlet and one that is 3 higher than max
  arrange(X1) %>%
  mutate(jolt_diffs=X1-lag(X1)) %>%
  count(jolt_diffs) %>%
  filter(!is.na(jolt_diffs)) 

pull(ans1_tab) %>% prod()
