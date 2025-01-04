
library(tidyverse)

data_as_is <- read.csv('scores.csv')

data_as_is %>% select(home) %>% distinct()
data_as_is %>% select(away) %>% distinct()
