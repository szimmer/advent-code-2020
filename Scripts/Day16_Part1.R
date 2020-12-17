library(tidyverse)
library(here)
library(unglue)

input_in <- read_file(here("Input", "input_day16.txt"))

input_in_split <- input_in %>% str_split(pattern= "\\n\\n", simplify = TRUE) %>%
  as.vector()

rulepattern = "{category}: {l1}-{h1} or {l2}-{h2}"

rules <- tibble(rule=input_in_split[1]) %>%
  separate_rows(rule, sep="\\n") %>%
  unglue_unnest(rule, patterns=rulepattern, convert=TRUE) %>%
  mutate(RuleNumber=row_number())

# rules_long <- rules %>%
#   pivot_longer(-category) %>%
#   mutate(type=str_sub(name, 1, 1),
#          which=parse_number(name)) %>%
#   select(-name) %>%
#   pivot_wider(names_from=type, values_from=value)

nearby_tickets <- tibble(ticketdetail=input_in_split[3]) %>%
  separate_rows(ticketdetail, sep="\\n") %>%
  slice(-1) %>%
  separate(ticketdetail, into=str_c("Field",1:nrow(rules)), sep=",", convert=TRUE) %>%
  mutate(TicketID=row_number()) %>%
  pivot_longer(-TicketID) %>%
  mutate(Field=parse_number(name))
 
my_between <- function(x, left, right){
  x >= left & x <= right
}

rule_value <- function(value){
  rules %>%
    mutate(Check = my_between(value, l1, h1)| my_between(value, l2, h2)) %>%
    filter(Check) %>%
    pull(RuleNumber) %>%
    list()
}

check_ticket_values <- nearby_tickets %>%
  rowwise() %>%
  mutate(CheckList=rule_value(value))

check_ticket_values %>%
  ungroup() %>%
  mutate(ListLength=map_int(CheckList, length)) %>%
  filter(ListLength==0) %>%
  summarise(ErrorTotal=sum(value))

InvalidTickets <- check_ticket_values %>%
  ungroup() %>%
  mutate(ListLength=map_int(CheckList, length)) %>%
  filter(ListLength==0) %>%
  pull(TicketID) %>%
  unique()
######### Part 2

ValidTickets <- check_ticket_values %>%
  filter(! (TicketID %in% InvalidTickets)) %>%
  ungroup() %>%
  arrange(Field) 

rule_option <- function(FieldNum){
  ValidTickets %>%
    filter(Field==FieldNum) %>%
    pull(CheckList) %>%
    Reduce(intersect, .)
}

rule_options <- 1:nrow(rules) %>% map(rule_option)
rule_options_length <- map_int(rule_options, length)

pairing <- tibble(Rule=1:20, Field=NA_integer_)
allmatched <- FALSE

remove_it <- function(j, i){
  setdiff(j, i)
}

while(!(allmatched)){
  unique_rules <- which(rule_options_length==1)
  for (i in unique_rules){
    j <- rule_options[[i]]
    pairing <- pairing %>%
      mutate(Field=
               case_when(Rule==j~i,
                         TRUE~Field))
  }
  rule_options <- rule_options %>% map(remove_it, i=j)
  rule_options_length <- map_int(rule_options, length)
  if (all(near(rule_options_length, 0))) allmatched <- TRUE
}

departure_rules <- rules %>% 
  select(Rule=RuleNumber, category) %>%
  full_join(pairing, by="Rule") %>%
  filter(str_detect(category, "departure"))


my_ticket <- tibble(value=input_in_split[2]) %>%
  separate_rows(value, sep="\\n") %>%
  slice(2) %>%
  separate_rows(value, sep=",", convert=TRUE) %>%
  mutate(Field=row_number()) 

my_ticket %>%
  right_join(departure_rules, by="Field") %>%
  pull(value) %>%
  prod()

#582 is too low