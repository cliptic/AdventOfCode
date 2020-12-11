# Advent of Code 2020
# Day 10

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)

input <- fread('input/Input_day11.txt', sep="", check.names = T, stringsAsFactors = F, header = FALSE)$V1
# Quest 1
charlen <- nchar(input[1])
df <- as.data.frame(transpose(as.matrix(strsplit(input, split = ""), header = FALSE,)), stringsAsFactors = F)
vec <- c()
for (i in 1:charlen){
   vec <- c(vec, paste0("col", i))
}
colnames(df) <- vec 
df <- data.frame(df, stringsAsFactors = F)
# 
df_base <- df
df_base[df_base == "L"] <- 1
df_base[df_base == "."] <- 0

df_base <- data.frame(matrix(as.numeric(unlist(df_base)), nrow=length(df_base), byrow=T), stringsAsFactors = F)

onerow <- df_base[1,]
onerow[,] <- 4

map1 <- bind_rows(onerow, df_base, onerow)
onecol <- map1[,1]
onecol[] <- 4

mappy <- bind_cols(onecol, map1, onecol)

#QUEST1
map.new <- mappy
empty <- c(0, 1, 4)
change <- 1
iter <- 0
while(change > 0){
   iter <- iter + 1
   change <- 0
   map <- map.new
   for (cc in 2:(ncol(map)-1)){
      for(rr in 2:(nrow(map)-1)){
         seat <- map[rr,cc]
         #surrounding seats
         seat1 <- map[rr-1,cc-1]
         seat2 <- map[rr-1,cc]
         seat3 <- map[rr-1,cc + 1]
         seat4 <- map[rr,cc-1]
         seat5 <- map[rr,cc + 1]
         seat6 <- map[rr+1,cc-1]
         seat7 <- map[rr+1,cc]
         seat8 <- map[rr+1,cc + 1]
         around <- c(seat1, seat2, seat3, seat4, seat5, seat6, seat7, seat8)
         if((seat == 1) & all(around %in% empty)){
            map.new[rr,cc] <- 2
            change <- change + 1
         }else if((seat == 2) & (sum(around == 2) > 3)){
            map.new[rr,cc] <- 1
            change <- change + 1
         }
      }
   }
   print(iter)
   print(paste(change, "changes"))
}


sum(map.new == 2)
# 2277


#Quest2
map.new <- mappy
empty <- c(0, 1, 4)
change <- 1
iter <- 0
while(change > 0){
   iter <- iter + 1
   change <- 0
   map <- map.new
for (cc in 2:(ncol(map)-1)){
   for(rr in 2:(nrow(map)-1)){
      seat <- map[rr,cc]
      #surrounding seats
      if(seat !=0){
      seat1 <- map[rr-1,cc-1]
      i1 <- 1
      while(seat1 == 0){
         seat1 <- map[rr-1-i1,cc-1-i1]
         i1 <- i1 + 1
      }
      seat2 <- map[rr-1,cc]
      i1 <- 1
      while(seat2 == 0){
         seat2 <- map[rr-1-i1,cc]
         i1 <- i1 + 1
      }
      seat3 <- map[rr-1,cc + 1]
      i1 <- 1
      while(seat3 == 0){
         seat3 <- map[rr-1-i1,cc+1+i1]
         i1 <- i1 + 1
      }
      seat4 <- map[rr,cc-1]
      i1 <- 1
      while(seat4 == 0){
         seat4 <- map[rr,cc-1-i1]
         i1 <- i1 + 1
      }
      seat5 <- map[rr,cc + 1]
      i1 <- 1
      while(seat5 == 0){
         seat5 <- map[rr,cc+1+i1]
         i1 <- i1 + 1
      }
      seat6 <- map[rr+1,cc-1]
      i1 <- 1
      while(seat6 == 0){
         seat6 <- map[rr+1+i1,cc-1-i1]
         i1 <- i1 + 1
      }
      seat7 <- map[rr+1,cc]
      i1 <- 1
      while(seat7 == 0){
         seat7 <- map[rr+1+i1,cc]
         i1 <- i1 + 1
      }
      seat8 <- map[rr+1,cc + 1]
      i1 <- 1
      while(seat8 == 0){
         seat8 <- map[rr+1+i1,cc+1+i1]
         i1 <- i1 + 1
      }
      around <- c(seat1, seat2, seat3, seat4, seat5, seat6, seat7, seat8)
      
      if((seat == 1) & all(around %in% empty)){
         map.new[rr,cc] <- 2
         change <- change + 1
      }else if((seat == 2) & (sum(around == 2) > 4)){
         map.new[rr,cc] <- 1
         change <- change + 1
      }
      }
   }
}
   print(iter)
   print(paste(change, "changes"))
}
   
sum(map.new == 2)
# 2066