# Advent of code 2020
# Day7
rm(list = ls())
# library(gdata)
# library(dplyr)
# library(stringr)
# library(data.table)
# library(magrittr)
# library(tidyr)
library(tidyverse)


data <- read_lines("input/Input_day7.txt") %>%
  str_split(" contain ") %>%
  map(~ str_split(.x, ", ") %>% unlist()) %>%
  set_names(., map_chr(., ~ magrittr::extract2(., 1)) %>% str_remove(" bags?")) %>%
  map(~ magrittr::extract(., -1))


## part1
find_colors <- function(list, color) {
  tmp <- c()
  for (i in color) {
    tmp <- c(tmp, map_dbl(list, ~ str_detect(., i) %>% sum()) %>%
               magrittr::extract(.>0) %>%
               names() %>%
               str_remove(" bags?"))
  }
  return(tmp)
}

final <- c()
k <- "shiny gold"

while(length(k) != 0) {
  k <- find_colors(data, k)
  final <- c(final, k)        # don't do this in real code
}

## (slow) Answer:
length(unique(final))
##


## part2
df <- data %>%
  map( ~ list(amounts = .x %>% str_extract(., "[0-9]+"), bags = .x %>% str_extract(., "[a-zA-Z]+ [a-zA-Z]+"))) %>%
  bind_rows(.id = "bag") %>%
  mutate(amounts = as.numeric(amounts))

part2 <- function(df, bag) {
  
  multiplier <- table(bag) %>%
    as_tibble()
  
  tmp <- df %>%
    filter(bag %in% !!bag) %>%
    select(bag, bags, amounts) %>%
    filter(bags != "no other", !is.na(amounts)) %>%
    left_join(multiplier, by = c(bag = "bag")) %>%
    mutate(amounts = amounts * n)
  
  map2(tmp$bags, tmp$amounts, ~ rep(.x, .y)) %>%
    unlist()
}

k <- part2(df, "shiny gold")
bags <- length(k)

while(!is.null(k)) {
  k <- part2(df, k)
  bags <- bags + length(k)
}

## Answer
bags
