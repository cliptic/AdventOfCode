# Advent of code 2022
# Day03

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
options(scipen=999)

input  <- fread('input/input_2022_day03.txt', sep = ",", header = FALSE, fill = TRUE)

data <- input %>%
  mutate(lng = nchar(V1)/2) %>%
  mutate(sack1 = str_sub(V1, 1, lng),
         sack2 = str_sub(V1, lng+1, lng*2)) 

priority <- c(letters, LETTERS)

# s1 <- data$sack1[1]
# s2 <- data$sack2[1]

# intersect(strsplit(s1, "")[[1]], strsplit(s2, "")[[1]])

for(i in 1:nrow(data)){
  s1 <- data$sack1[i]
  s2 <- data$sack2[i]
    data[i, "priority_char"] <- intersect(strsplit(s1, "")[[1]], strsplit(s2, "")[[1]])
   data[i, "priority_num"] <- match(data[i, "priority_char"], priority)
}


sum(data$priority_num)
# ans: 8123

################# Part 2


data <- unlist(input)
elf_groups <- list()
for(l in 1:100){
  (l*3 - 2):(l*3)
  elf_groups[["items"]][[l]] <- data[(l*3 - 2):(l*3)]
  
  aa <- intersect(strsplit(elf_groups[["items"]][[l]][1], "")[[1]], strsplit(elf_groups[["items"]][[l]][2], "")[[1]])
  aaa <- intersect(strsplit(elf_groups[["items"]][[l]][3], "")[[1]], aa)
  
  elf_groups[["priority_letter"]][[l]] <- aaa
  elf_groups[["priority_num"]][[l]] <- match(aaa, priority)
}

sum(unlist(elf_groups$priority_num))

# ans 2620