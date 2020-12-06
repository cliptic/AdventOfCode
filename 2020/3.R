# Advent of code 2020
# Day2
rm(list = ls())
library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)

input <- read.csv('input/Input_day3_.csv', check.names = T, stringsAsFactors = F) %>%
  mutate(Input = str_trim(Input))

# dt_031_in  <- fread('input/Input_day3.csv', sep = '', header = FALSE)
# dt_031_fl  <-
#   tidyr::separate(dt_031_in, 'V1', sep = '', into = paste0('R', 1:(nchar(dt_031_in[1, V1]) + 1)), ) %>%
#   as.matrix() %>%
#   `colnames<-`(NULL)
# if (all(dt_031_fl[, 1] == '')) {
#   dt_031_fl <- dt_031_fl[, -1]
# }

# Quest 1
charlen <- 31
vec <- c()
for (i in 1:charlen){
  vec <- c(vec, paste0("col", i))
}

df <- input %>%
  separate(Input, all_of(vec)) 
df <- as.data.frame(sapply(df, as.numeric))
  
# Quest1
trees <- 0
len <- 322
r <- 1 
c <- 1
a <- 1
for(r in 1:len){
  #step right
  while(a <= 3){
    c <- c+1
    if(c > 31){
      c <- 1
    }
    # trees <- trees + df[r,c]
    a <- a + 1
  }
  a <- 1
  # step down
  r <- r+1
  trees <- trees + df[r,c]
}

# ans1: 187

# Quest2

cstep <- c(1,3,5,7,1)
rstep <- c(1,1,1,1,2)
ans <- 1
for (x in 1:length(cstep)){
  trees <- 0
  len <- 322/rstep[x]
  c <- 1
  a <- 1
for(r in 1:len){
  #step right
  while(a <= cstep[x]){
    c <- c+1
    if(c > 31){
      c <- 1
    }
    a <- a + 1
  }
  a <- 1
  # step down
  r <- r+rstep[x]
  trees <- trees + df[r,c]
}
  ans <- ans*trees
}

# ans_2 4723283400

#1:1 86
#3:1 187
#5:1 75
#7:1 89
#1:2 44
# ans_2 = 86*187*75*89*44