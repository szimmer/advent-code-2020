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

passinfo_valid <- pass_info_df %>%
  select(-cid) %>%
  filter(complete.cases(.)) %>%
  mutate(byr=as.numeric(byr),
         iyr=as.numeric(iyr),
         eyr=as.numeric(eyr)) %>%
  filter(between(byr, 1920, 2002)) %>%
  filter(between(iyr, 2010, 2020)) %>%
  filter(between(eyr, 2020, 2030)) %>%
  mutate(hgtnum=parse_number(hgt),
         hgtunit=str_sub(hgt, str_length(as.character(hgtnum))+1),
         validHgt=case_when(
           hgtunit=="cm" & between(hgtnum, 150, 193) ~ TRUE,
           hgtunit=="in" & between(hgtnum, 59, 76) ~ TRUE,
           TRUE ~ FALSE 
         )
         ) %>%
  filter(validHgt) %>%
  mutate(validHcl=str_detect(hcl, "^#[\\d\\w]{6}$")) %>%
  filter(validHcl) %>%
  filter(ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')) %>%
  filter(str_detect(pid, "^[\\d]{9}$"))

passinfo_valid %>%
  nrow()
