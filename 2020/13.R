# Advent of code 2020
# Day13

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
options(scipen=999)

input  <- fread('input/Input_day13.txt', sep = ",", header = FALSE, fill = TRUE)

ID <- as.numeric(input$V1[1])
map <- input$V1[2] %>%
  str_split(",") %>%
  unlist
sched <- map[map != "x"] %>%
  as.numeric

min(sched)
max(sched)

m<-c()
for(i in 1:length(sched)){
  bus <- sched[i]
  m[i] <- bus - (ID %% bus)
  if(ID %% bus == 0){
    print(sched[i])
    stop("one bus leaves now")
  }
}

ans <- sched[which(m == min(m))] *
  m[which(m == min(m))]
ans
#3997

#quest2
sh2 <- map
nn <- which(sh2 %in% sched)
nn <- nn-1
sh2 <- as.numeric(sh2[sh2 != "x"])

ord <- order(decreasing = TRUE, sh2)
sh2 <- sh2[ord]
nn <- nn[ord]
nn <- nn %% sh2

continue <- TRUE
k <- 1
good <- 3
interval <- 787*29*23
n <- 19
tmp <- c()
while(continue){
  frq <- k*interval + n
  for(i in 1:length(sh2)){
    tmp[i] <- frq %% sh2[i]
  }
  if(sum(tmp == nn) > good){
    print(good)
    if(all(tmp == nn)){
      print(frq)
      continue <- FALSE
      print(frq)
      # stop("all.good")
    }
    print(frq)
    browser()
    interval <- prod(sh2[tmp == nn])
    n = frq - interval
    good <- sum(tmp == nn)
    k <- 0
  }
  k <- k + 1
  tmp <- c()
}

#1409240223158943