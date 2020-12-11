library(here)
library(tidyverse)

input_in <- read_csv(here("Input", "input_day10.txt"), col_names = FALSE)

# run of 5: 1,2,3,4,5 - 7 combos 
# 1,2,3,4,5
# 1,2,3,5
# 1,3,4,5
# 1,2,4,5
# 1,2,5
# 1,3,5
# 1,4,5
# run of 4: 1,2,3,4 - 4 combos
# 1,2,3,4
# 1,2,4
# 1,3,4
# 1,4
# run of 3: 1,2,3 - 2 comboos
# 1,2,3
# 1,3
# everything else is 1.


ans2_df <- input_in %>% 
  add_row(X1=c(0, max(input_in$X1)+3)) %>% #add the charging outlet and one that is 3 higher than max
  arrange(X1) %>%
  filter(X1-lag(X1)==1 |lead(X1)-X1==1) %>%
  mutate(Group_ord=if_else(X1-lag(X1,1, max(X1))==1, 0, 1),
         Group=cumsum(Group_ord)) %>%
  count(Group) %>%
  mutate(Ways=case_when(
    n==5~7,#from enumeration above
    n==4~4,
    n==3~2,
    n==2~1,
    TRUE~1
  ))

pull(ans2_df ,Ways) %>% prod() %>% sprintf("%.0f", .)
