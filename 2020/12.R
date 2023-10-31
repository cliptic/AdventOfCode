# Advent of code 2020
# Day12

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)


input  <- fread('input/Input_day12.txt',  header = FALSE, fill = TRUE)

df <- as.data.frame(input, stringsasfactors = F) %>%
  mutate(side = substr(V1, 1, 1)) %>%
  mutate(steps = as.numeric(substr(V1, 2, nchar(V1)))) %>%
  select(-V1) 

moves <- df %>%
  mutate(X = ifelse(side == "E", steps, 
                    ifelse(side == "W", -steps, 0))) %>%
  mutate(Y = ifelse(side == "S", steps,
                    ifelse(side == "N", -steps, 0))) %>%
  mutate(way = 1)


for (i in 1:nrow(moves)){
  si <- moves[i,'side']
  st <- moves[i,'steps']
  way <- moves[i, "way"]
  if(si == "R"){
    way = way + moves[i, "steps"]/90 
  }else if( si == "L"){
    way = way - moves[i, "steps"]/90 
  }
  moves[i:nrow(moves), "way"] <- way
}

unique(moves$way)

mv <- moves %>%
  mutate(X = ifelse((way %% 4 == 1) & (side == "F"), steps, X)) %>%
  mutate(X = ifelse((way %% 4 == 3) & (side == "F"), -steps, X)) %>%
  mutate(Y = ifelse((way %% 4 == 0) & (side == "F"), -steps, Y)) %>%
  mutate(Y = ifelse((way %% 4 == 2) & (side == "F"), steps, Y))

XX <- sum(mv$X)
YY <- sum(mv$Y)

ans <- abs(XX) + abs(YY)
ans
# 2270

#Quest2
moves2 <- moves %>%
  mutate(way = 0)
moves2[1, "X"] <- moves2[1, "X"] + 10
moves2[1, "Y"] <- moves2[1, "Y"] - 1

for (i in 1:nrow(moves2)){
  si <- moves2[i,'side']
  st <- moves2[i,'steps']
  way <- moves2[i, "way"]
  if(si == "R"){
    way = st/90 
  }else if(si == "L"){
    way = -st/90 
  }
  moves2[i, "way"] <- way
}

moves3 <- moves2 %>%
  mutate(shipX = 0) %>%
  mutate(shipY = 0) %>%
  mutate(sX = 0, sY = 0)

for (i in 2:nrow(moves3)){
# for (i in 2:5){
  way <- moves3[i, "way"]
  
  if(moves3[i, "side"] %in% c("S", "N", "W", "E")){
    moves3[i, "X"] <- moves3[i, "X"] + moves3[i-1, "X"]
    moves3[i, "Y"] <- moves3[i, "Y"] + moves3[i-1, "Y"]
  }else if(moves2[i, "side"] %in% c("L", "R")){
    if(way %% 4 == 1){
      moves3[i, "Y"] <- moves3[i-1, "X"]
      moves3[i, "X"] <- -moves3[i-1, "Y"]
    }else if(way %% 4 == 2){
      moves3[i, "Y"] <- -moves3[i-1, "Y"]
      moves3[i, "X"] <- -moves3[i-1, "X"]
    }else if(way %% 4 == 3){
      moves3[i, "X"] <- moves3[i-1, "Y"]
      moves3[i, "Y"] <- -moves3[i-1, "X"]
    }else if(way %% 4 == 0){
      moves3[i, "X"] <- moves3[i-1, "Y"]
      moves3[i, "Y"] <- moves3[i-1, "X"]
    }
  }else if(moves3[i, "side"] == "F"){
    moves3[i, "X"] <- moves3[i-1, "X"]
    moves3[i, "Y"] <- moves3[i-1, "Y"]
    
    sX <- moves3[i, "steps"] * moves3[i, "X"]
    sY <- moves3[i, "steps"] * moves3[i, "Y"]
    
    moves3[i, "sX"] <- sX
    moves3[i, "sY"] <- sY
    
    moves3[i:nrow(moves3), "shipX"] <- sX + moves3[i-1, "shipX"]
    moves3[i:nrow(moves3), "shipY"] <- sY + moves3[i-1, "shipY"]
  }
}

options(scipen=999)

XX <- abs(moves3[nrow(moves3), "shipX"])
YY <- abs(moves3[nrow(moves3), "shipY"])
ans2 <- XX + YY
ans2

# ###
# moves3 <- moves3 %>%
#   mutate(Xpoint = X + shipX) %>%
#   mutate(Ypoint = Y + shipY)
# fig <- plot(moves3$shipX, moves3$shipY, type="l", col =  "red")
#   lines(moves3$Xpoint, moves3$Ypoint, type="l", col = "blue")
