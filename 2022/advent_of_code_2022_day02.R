# Advent of code 2022
# Day02

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
options(scipen=999)

input  <- fread('input/input_2022_day02.txt', sep = " ", header = FALSE, fill = TRUE)
colnames(input) <- c("Elf", "Me")

data <- input

data[, 'MyValue'] <- ifelse(data[, 'Me'] == "X", 1, ifelse(
  data[, 'Me'] == "Y", 2, ifelse(
    data[, 'Me'] == "Z", 3, NA)))

# rock  paper Scissors
# A     B    C
# X     Y    Z

nms <- list()
nms[[1]] <- c("A", "B", "C")
nms[[2]] <- c("X", "Y", "Z")
calcs <- matrix(data = NA, nrow = 3, ncol = 3, dimnames = nms)
calcs["A","Z"] <- 0
calcs["B","X"] <- 0
calcs["C","Y"] <- 0
calcs["A", "X"] <- 3
calcs["B", "Y"] <- 3
calcs["C", "Z"] <- 3
calcs["A","Y"] <- 3
calcs["B","Z"] <- 6
calcs["C","X"] <- 6


data[, "MyWin"] <- 0

for(n in 1:nrow(input)){
  data[n, "MyWin"] <- calcs[unlist(data[n, "Elf"]), unlist(data[n, "Me"])]
}

sum(data$MyValue) + sum(data$MyWin)
# ans: total score is 10718

######### Part 2
# rock  paper Scissors
# A     B    C
# Lose  Draw  Win
# X     Y    Z

# rock  paper scissors
# 1     2     3


calcs["A","X"] <- 3 
calcs["B","X"] <- 1
calcs["C","X"] <- 2
calcs["A", "Y"] <- 1
calcs["B", "Y"] <- 2
calcs["C", "Y"] <- 3
calcs["A","Z"] <- 2
calcs["B","Z"] <- 3
calcs["C","Z"] <- 1

data <- input
data[, 'MyWin'] <- ifelse(data[, 'Me'] == "X", 0, ifelse(
  data[, 'Me'] == "Y", 3, ifelse(
    data[, 'Me'] == "Z", 6, NA)))

for(n in 1:nrow(input)){
  data[n, 'MyValue'] <- calcs[unlist(data[n, "Elf"]), unlist(data[n, "Me"])]
}

sum(data$MyValue) + sum(data$MyWin)
# ans: 14652