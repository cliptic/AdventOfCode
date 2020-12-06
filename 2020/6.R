# Advent of code 2020
# Day6

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)


input  <- fread('input/Input_day6.txt',  header = FALSE, fill = TRUE) 
lst2 <- c()
ans <- 0
for (i in 1:nrow(input)){
  x <- input$V1[i] 
  if(x == ""){
    ans <- ans + length(unique(lst2))
    lst2 <- c()
  }else{
  x <- x %>%
    str_split(pattern = "")
  lst2 <- c(unlist(lst2), unlist(x))
  if(i == nrow(input)){
    ans <- ans + length(unique(lst2))
  }
  }
}
ans
#6534

#Quest2
lst2 <- c()
ans <- 0
for (i in 1:nrow(input)){
  x <- input$V1[i] 
  if(x == ""){
    ans <- ans + length(unique(lst2))
    lst2 <- c()
  }else{
    x <- unlist(str_split(x, pattern = ""))
    if(i == 1){
      lst2 <- x 
    }else if(input$V1[i-1] == ""){
      lst2 <- x 
    }else{
      lst2 <- lst2[lst2 %in% x]
    }
    if(i == nrow(input)){
      ans <- ans + length(unique(lst2))
    }
    
  }
}
ans
# 3402