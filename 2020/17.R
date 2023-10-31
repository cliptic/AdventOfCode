# Advent of code 2020
# Day17

rm(list = ls())

library(dplyr)
library("data.table")

input  <- fread('input/Input_day17.txt', sep = ",", header = FALSE, fill = TRUE)$V1
df <- as.data.frame(transpose(as.matrix(strsplit(input, split = ""), header = FALSE,)), stringsAsFactors = F)

# Quest 1

cubes <- df 
cubes[cubes == "."] <- 0
cubes[cubes == "#"] <- 1
colnames(cubes) <- paste0("R", 1:8)
cubes <- data.frame(sapply(cubes, as.integer))
for(i in cubes){
  if(i == 1){
    cb.ls[[k]] <- 
  }
}
cb <- which(cubes == 1, arr.ind = TRUE)

cubs <- data.frame(which(cubes == 1, arr.ind = TRUE)) 
colnames(cubs) = c("x", "y")
cubs[["z"]] <- 0

cub.list <- split(cubs, seq(nrow(cubs)))

cub.ls <- split(cubs, seq_len(nrow(cubs)))

nei <- list()
for(i in c(x,y,z)){
nei[[i]] <- c(-1,0,1)
}

nei <- expand.grid( "x" = c(-1,0,1), 
                    "y" = c(-1,0,1), 
                    "z" = c(-1,0,1)) %>%
  filter(!((x == 0) & (y == 0) & (z == 0))) 

for (i in 1:6){
  if(!((sum(nei) %in% c(2,3)) & (cube == 1))){
    cube <- 0
  }else if((cube == 0) & (sum(nei) == 3)){
    cube <- 1
  }
}