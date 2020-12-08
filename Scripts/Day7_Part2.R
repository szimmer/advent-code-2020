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
  joiner <- tibble(Outer=bagtype, multiplier=1)
  
  while (!stopit){
    init <- input %>%
      inner_join(joiner, by="Outer") 
    if (nrow(init)==0){
      stopit <- TRUE
    } else{
      mid <- init %>% 
        mutate(Outer=InnerColor,
               multiplier=multiplier*InnerNumber) %>%
        select(-InnerNumber)
      joiner <- mid %>%
        select(Outer, multiplier)
      out <- bind_rows(out, mid)
    }
  }
  if (is.null(out)){
    return(tibble(Outer=bagtype_orig, InnerColor=NA_character_))
  } else{
    out %>%
      mutate(Outer=bagtype_orig)
  }
}

ans <- contains_bags("shiny gold bag")
ans %>%
  summarise(ans=sum(multiplier))

