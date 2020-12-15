library(tidyverse)
library(here)

input_in <- read_csv(here("Input", "input_day14.txt"), col_names=FALSE)

# input_in <-
#   read_csv(
# "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
# mem[8] = 11
# mem[7] = 101
# mem[8] = 0" 
#     , col_names=FALSE)  


input <-
  input_in %>%
  mutate(NewGroup=str_detect(X1, "mask"),
         Group=cumsum(NewGroup)) %>%
  mutate(Order=row_number())

input_mask <- input %>%
  filter(NewGroup) %>%
  mutate(mask=str_sub(X1, 8)) %>%
  select(Group, mask)

dec2bin <- function(x){
  
  longout <- as.integer(rev(intToBits(x)))
  geti <- function(i){
    str_c(longout[1:32+(i-1)*32], collapse="")
  }
  
  1:length(x) %>% map_chr(geti)
  
  
  
  }


input_upd <- input %>%
  filter(!NewGroup) %>%
  select(-NewGroup) %>%
  mutate(X2=str_replace_all(X1, "mem\\[|\\]", "")) %>%
  separate(X2, into=c("Address", "Update"), sep="=", convert = TRUE) %>%
  rowwise() %>%
  mutate(Update_bin=dec2bin(Update))

input_tog <- input_upd %>%
  select(Group, Order, Address, Update, Update_bin) %>%
  left_join(input_mask, by="Group")


get_val <- function(mask, value){
  maskv <- str_split_fixed(mask, "", n=36) %>% as.vector()
  valuev <- c(rep("0", 4), str_split_fixed(value, "", n=32) %>% as.vector())
  
  bits <- case_when(
    maskv %in% c("0", "1") ~ maskv,
    TRUE ~ valuev) %>%
    as.numeric() 
  
  sum(2^(35:0)*bits)
}

out <- input_tog %>%
  rowwise() %>%
  mutate(value=get_val(mask, Update_bin)) %>%
  select(Address, Order,Update, value)

out %>%
  arrange(Address, desc(Order)) %>% 
  group_by(Address) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(sum=sum(value)) %>%
  pull(sum) %>%
  sprintf("%.0f", .)
