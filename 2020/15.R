# Advent of code 2020
# Day15

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
library(plyr)
library(ggplot2)


input  <- c(9,3,1,0,8,4)

#Quest1

ls <- as.integer(input)
k <- length(input)

while(k < 2020){
  k <- k+1
  prev <- ls[k-1]
  n <- which(ls == prev, prev)
  if(length(n) < 2){
    ls <- c(ls, 0)
  }else if(length(n) >= 2){
    last.diff <- n[length(n)] - n[length(n)-1]
    ls <- c(ls, last.diff)
  }
}
ans <- ls[length(ls)]
ans
# 371

#Quest2
k <- length(input)
input <- as.integer(input)
IDs <- 1:k
names(IDs) <- input
prev <- as.character(input[k])

while(k <= 2020){
  if(prev %in% names(IDs[1:(k-1)])){
    df <- as.character(k + 1 - IDs[[prev]])
  }else{
    # IDs[["0"]] <- k
    df <- as.character(k + 1 - IDs[["0"]])
    # IDs[["0"]] <- k - 1
    # prev <- "0"
  }
  k <- k+1
  IDs[[df]] <- k
  print(k)
  prev <- df
}

prev
IDs[[prev]]

# Learn from Darius
play_game <- function(starting, turn){
  k <- length(starting)
  prev <- integer()
  for (i in 1:(k-1)){
    prev[starting[i] + 1] <- as.integer(i)
  }
  last <- starting[k]
  for (i in (k + 1):turn){
    new <- ifelse(is.na(prev[last + 1]), 0, i - 1 - prev[last + 1])
    prev[last + 1] <- as.integer(i - 1)
    last <- new
  }
  return(last)
}

# part 2
# test
N <- 30000000
r2 <- play_game(starting = input, turn = N)
print(r2)
