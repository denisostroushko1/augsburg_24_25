
library(tidyverse)

data_as_is <- read.csv('scores.csv')

data_as_is %>% select(home) %>% distinct() %>% arrange(home) %>% mutate(L_ = nchar(home))
data_as_is %>% select(away) %>% distinct() %>% arrange(away) %>% mutate(L_ = nchar(away))
