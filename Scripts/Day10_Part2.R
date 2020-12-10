library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day10.txt"), col_names = FALSE)


ans2_df <- input_in %>% 
  add_row(X1=c(0, max(input_in$X1)+3)) %>% #add the charging outlet and one that is 3 higher than max
  arrange(X1) %>%
  filter(X1-lag(X1)==1 |lead(X1)-X1==1) %>%
  mutate(Group_ord=if_else(X1-lag(X1,1, max(X1))==1, 0, 1),
         Group=cumsum(Group_ord)) %>%
  count(Group) %>%
  mutate(Ways=case_when(
    n==5~7,#choose(4,4)+choose(4,2)+choose(4,3)
    n==4~4, #choose(3, 3)+choose(3,2)
    n==3~2,
    n==2~1,
    TRUE~1
  ))

pull(ans2_df ,Ways) %>% prod() %>% sprintf("%.0f", .)
