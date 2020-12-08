library(tidyverse)
library(here)

# input_in <- read_table(here("Input", "input_day7_test.txt"), col_names = FALSE)
input_in <- read_table(here("Input", "input_day7.txt"), col_names = FALSE)

input <- input_in %>%
  separate(X1, into=c("Outer", "Inner"), "contain|contains") %>%
  separate_rows(Inner, sep=",") %>%
  mutate(Outer=Outer %>% str_trim() %>% str_replace("bags", "bag"),
         Inner=if_else(Inner==" no other bags.", NA_character_, Inner),
         Inner=Inner %>% str_trim() %>% str_replace("bags", "bag") %>% str_replace("\\.", ""),
         InnerNumber=parse_number(Inner),
         InnerColor=str_sub(Inner, str_length(as.character(InnerNumber))+2)
         ) %>%
  select(-Inner) %>%
  filter(!is.na(InnerNumber))

bagtypes <- input %>% pull(Outer) %>% unique()

contains_bags <- function(bagtype){
  bagtype_orig <- bagtype
  stopit <- FALSE
  out <- NULL
  while (!stopit){
    init <- input %>%
      filter(Outer %in% bagtype)
    if (nrow(init)==0){
      stopit <- TRUE
    } else{
      out <- bind_rows(out, select(init, -Outer))
      bagtype <- init %>% pull(InnerColor) %>% unique()
    }
  }
  if (is.null(out)){
    return(tibble(Outer=bagtype_orig, InnerNumber=NA_real_, InnerColor=NA_character_))
  } else{
    out %>%
      mutate(Outer=bagtype_orig)
  }
}

bagtypes %>% map_df(contains_bags) %>%
  filter(InnerColor=="shiny gold bag") %>%
  distinct(Outer) %>%
  nrow()
