# Advent of code 2020
# Day2

rm(list = ls())
library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)



input  <- fread('input/Input_day5.txt', sep = "",
                header = FALSE, fill = TRUE)[[1]] 
df <- as.data.frame(input, stringsAsFactors = F) %>% 
  rename(C0 = 'input') %>%
  tidyr::separate(C0, sep="", into = paste0("col", 1:nchar(input[1])))

df2 <- df

for (k in 1:length(input)){
for (i in 1:nchar(input[1])){
  df2[k,i] <- substring(input[k], i, i)
}
}

# Quest1

df3 <- df2 %>%
  mutate(row = NA) %>%
  mutate(columns = NA)

# RR <- 1:128 # 128 rows numbered 0 to 127
# CC <- 1:8 # 8 seats (0 to 7)

for (m in 1:nrow(df2)){
  RR <- 1:128
  CC <- 1:8
  for (n in 1:7){
    if (df3[m,n] == "F"){
      a <- RR[1]
      b <- (RR[1] + length(RR)/2 - 1) 
      RR <- a:b
    }else{
      b <- RR[length(RR)]
      a <- (RR[length(RR)] - length(RR)/2 + 1) 
      RR <- a:b
    }
  }
  for (n in 8:10){
    if(df3[m,n] == "L"){
      a <- CC[1]
      b <- (CC[1] + length(CC)/2 - 1) 
      CC <- a:b
    }else{
      b <- CC[length(CC)]
      a <- (CC[length(CC)] - length(CC)/2 + 1) 
      CC <- a:b
    }
  }
  df3[m, 'row'] <- RR - 1
  df3[m, 'columns'] <- CC - 1
}

df4 <- df3 %>%
  mutate(ID = row * 8 + columns)

#Quest 2
a <- min(df4$ID)
b <- max(df4$ID)
X <- a
for(k in sort(df4$ID)){
  if(k != X){
    print(X)
    stop("You found your seat")
  }
  X <- X+1
}

#619