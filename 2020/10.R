# Advent of Code 2020
# Day 10

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)

input  <- fread('input/Input_day10.txt',  header = FALSE, fill = TRUE)

ord <- input %>%
  order()

df <- c(0, input[ord]$V1)

#Quest1
df2 <- c(df[2:length(df)], df[length(df)]+3)
diffs <- df2-df
ones <- sum(diffs == 1)
threes <- sum(diffs == 3)
ANS1 <- ones*threes

ANS1


#Quest2


strings.of.ones <- c()
k <- 0
for(i in diffs){
  if(i == 1){
    k <- k+1
    i <- i+1
  }else{
    i <- i + 1
    strings.of.ones <- c(strings.of.ones, k)
    k <- 0
  }
}

strings.of.ones <- strings.of.ones[strings.of.ones != 0]
strings.of.ones <- strings.of.ones[strings.of.ones != 1]

ways <- 1
for(m in strings.of.ones){
 if(m == 4){
   ways = ways * 7
   print(m)
   print(ways)
 }else if(m == 3){
   ways = ways * 4
   print(m)
   print(ways)
 }else if(m == 2){
   ways = ways * 2
   print(m)
   print(ways)
 }else{
   stop("something's wrong")
 }
}

# Ans: 3.45419e+12
ways
