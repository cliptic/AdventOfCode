# Advent of code 2020
# Day14

rm(list = ls())

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)
library(binaryLogic)
options(scipen = 100, digits = 4)

input  <- fread('input/Input_day14.txt', sep = ",", header = FALSE, fill = TRUE)$V1

ls<- list()

#Quest1
for (i in 1:length(input)){
  if(grepl("mask", input[i])){
    mask <- substr(input[i], 8, nchar(input[i]))
  }else if(grepl("mem", input[i])){
    dress <- as.numeric(regmatches(input[i], 
                                   gregexpr('[0-9]+',input[i]))[[1]][1])
    nm <- as.binary(as.numeric(regmatches(input[i], 
                                           gregexpr('[0-9]+',input[i]))[[1]][2]), n=36)
    for(k in 1:nchar(mask)){
      ch <- substr(mask, k, k)
      if(ch != "X"){
        nm[k] <- as.binary(ch)
      }
    }
    ls[[dress]] <- as.numeric(nm)
  }
}

ans <- sum(unlist(ls))
ans
# 13105044880745

#Quest2
ls<- list()

#Quest1
for (i in 1:length(input)){
  if(grepl("mask", input[i])){
    mask <- substr(input[i], 8, nchar(input[i]))
  }else if(grepl("mem", input[i])){
    dress <- as.binary(as.numeric(regmatches(input[i], 
                                             gregexpr('[0-9]+',input[i]))[[1]][1]), n=36)
    nm <- as.numeric(regmatches(input[i], 
                                gregexpr('[0-9]+',input[i]))[[1]][2])
    l.mask <- c()
    for(k in 1:nchar(mask)){
      ch <- substr(mask, k, k)
      l.mask[k] <- ch
      if(ch == "1"){
        dress[k] <- as.binary(1)
      }
    }
    n <- sum(l.mask == "X")
    gr <- expand.grid(rep(list(0:1), n))
    for(op in 1:nrow(gr)){
      nox <- 1
      for(cx in 1:length(l.mask)){
        if(l.mask[cx] == "X"){
          dress[cx] <- as.binary(gr[op, nox])
          nox <- 1 + nox
        }
      }
      dr <- paste(as.numeric(dress))
      ls[[dr]] <- nm
    }
  }
}

ans <- sum(unlist(ls))
ans
# 3505392154485