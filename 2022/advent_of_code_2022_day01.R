# Advent of code 2022
# Day01

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
options(scipen=999)

input  <- fread('input/input_2022_day01.txt', sep = ",", header = FALSE, fill = TRUE)

l1 <- unlist(input$V1)

# Finding the elf with the most calories and the sum of it's calories carried
elves <- list()
elf <- 1
food <- list()
answer <- 0
for(l in l1){
  if(!is.na(l)){
    food <- c(food, l)
  }
  if(is.na(l)|l==length(l1)){
    elves[[elf]] <- food
    if(answer < sum(unlist(food))){
      answer_elf_ID <- elf
    }
    answer <- max(sum(unlist(food)), answer)
    food <- list()
    elf <- elf + 1
  }
}
# ans: 69501

# Finding the top3 elfs with the most calories and the sum of their calories carried

elves <- list()
elf <- 1
food <- list()
answer <- 0
top3 <- c(0,0,0)
answer_elf_ID <- c(NA, NA, NA)
for(l in l1){
  if(!is.na(l)){
    food <- c(food, l)
  }
  if(is.na(l)|l==length(l1)){
    elves[[elf]] <- food
    if(sum(unlist(food)) > min(top3)){
      top3[which.min(top3)] <- sum(unlist(food))
      answer_elf_ID[which.min(top3)] <- elf
    }
    answer <- max(sum(unlist(food)), answer)
    food <- list()
    elf <- elf + 1
  }
}

# ans: 202346
