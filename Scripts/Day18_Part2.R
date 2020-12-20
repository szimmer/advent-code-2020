library(tidyverse)
library(here)

# input_in <- read_csv(
# "1 + (2 * 3) + (4 * (5 + 6))
# 2 * 3 + (4 * 5)
# 5 + (8 * 3 + 9 + 3 * 4 * 3)
# 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
# ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",
# col_names = FALSE
# )

input_in <- read_csv(here("Input", "input_day18.txt"), col_names = FALSE)

myinnermath <- function(x){
  symexist <- str_detect(x, "[\\+\\*]")
  while (symexist){
    if (str_detect(x, "[\\+]")){
      curexp <- str_extract(x, "\\d+\\s?[\\+]\\s?\\d+")
      repexp <- as.character(eval(parse(text=curexp)))
      x <- str_replace(x, "\\d+\\s?[\\+]\\s?\\d+", repexp)
      symexist <- str_detect(x, "[\\+\\*]")
    } else {
      curexp <- str_extract(x, "\\d+\\s?[\\*]\\s?\\d+")
      repexp <- as.character(eval(parse(text=curexp)))
      x <- str_replace(x, "\\d+\\s?[\\*]\\s?\\d+", repexp)
      symexist <- str_detect(x, "[\\+\\*]")
      
    }
  }
  return(x)
}

mysimpmath <- function(x){
  parenexist <- str_detect(x, "\\(|\\)")
  while (parenexist){
    curexp <- str_extract(x, "\\(\\d+\\s?([*+]\\s?\\d+\\s?)+\\)")
    curexp <- str_replace_all(curexp, "\\(|\\)", "")
    x <- str_replace(x, "\\(\\d+\\s?([*+]\\s?\\d+\\s?)+\\)", myinnermath(curexp))
    parenexist <- str_detect(x, "\\(|\\)")
  }
  return(x)
}


newmath <- function(x){
  x %>% mysimpmath() %>% myinnermath() %>% as.numeric()
}

homework_list <- input_in %>%
  rowwise() %>%
  mutate(sol=newmath(X1))

homework_list %>%
  ungroup() %>%
  summarise(sum=sum(sol)) %>%
  pull(sum) %>%
  sprintf("%.0f", .)
