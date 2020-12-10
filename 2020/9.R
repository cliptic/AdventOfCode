# Advent of code 2020
# Day9
rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)

input  <- fread('input/Input_day9.txt',  header = FALSE, fill = TRUE) 

nums <- input$V1

#Quest1
for (i in 26:length(nums)){
  sm <- nums[i]
  lst <- nums[(i-25):(i-1)]
  other.halfs <- sm - lst
  if(!any(other.halfs %in% lst)){
    print(sm)
    ans1 <- sm
    stop("here it is")
  }
}
# 29221323



#Quest2

ans1 <- 29221323

for (i in 1:length(nums)){
  lst <- nums[1:i]
  sq <- seq(i, 1, by = -1)
  for (x in sq){
    sq2 <- seq(sq[1], x, by = -1)
    if( sum(lst[sq2]) == ans1){
      highest <- max(lst[sq2])
      lowest <- min(lst[sq2])
      ans2 <- highest + lowest
      print(ans2)
      stop("here you go again")
    }
  }
}
# 4389369

