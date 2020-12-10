# Advent of code 2020
# Day7

library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(tidyr)


input  <- fread('input/Input_day7.txt',  header = FALSE, fill = TRUE) 
input2 <- input %>%
  mutate(Outer = paste(V1, V2),
         BagNo1 = V5,
         BagCol1 = paste(V6,V7),
         BagNo2 = V9,
         BagCol2 = paste(V10,V11),
         BagNo3 = V13,
         BagCol3 = paste(V14,V15),
         BagNo4 = V17,
         BagCol4 = paste(V18,V19)
         ) %>%
  select(Outer,paste0("BagNo", 1:4),paste0("BagCol", 1:4)) 

# Quest1
lvl1list <- c()
inslist <- c("shiny gold")
newans <- 0
for(k in 1:30){
  for(i in 1:nrow(input2)) {
    oldans <- newans
    if((input2[i,BagCol4] %in% inslist)|
       (input2[i,BagCol3] %in% inslist)|
       (input2[i,BagCol2] %in% inslist)|
       (input2[i,BagCol1] %in% inslist)){
      lvl1list <- c(unlist(lvl1list), input2[i,Outer])
    }
  }
  inslist <- unique(lvl1list, "shiny gold")
  uniq.outers <- unique(lvl1list[lvl1list %in% input2$Outer])
  print(paste("iteration ", k))
  print(paste("unique outer colors ", length(uniq.outers)))
  newans <- length(uniq.outers)
  if(newans == oldans){
    ans <- length(uniq.outers)
    print("Your answer")
    print(ans)
    stop("you found it")
  }
}
#144

#Quest2

# inslist <- c("shiny gold")
# x <- 2
# df.ans <- data.frame(bag = "shiny gold", times = 1, stringsAsFactors = F)
# inslist2 <- c()
# for(k in 1:50){
#   for(bag.col in inslist){
#     if(bag.col %in% input2$Outer){
#       i <- which(input2$Outer == bag.col)
#       inside.cols <- c(input2[i, BagCol1], input2[i, BagCol2], 
#                        input2[i, BagCol3], input2[i, BagCol4])
#       inside.cols <- inside.cols[inside.cols != " "]
#       inside.all <- c(input2[i, BagCol1], input2[i, BagNo1], 
#                       input2[i, BagCol2], input2[i, BagNo2], 
#                       input2[i, BagCol3], input2[i, BagNo3], 
#                       input2[i, BagCol4], input2[i, BagNo4])
#       inside.all <- inside.all[inside.all != " "]
#       inside.all <- inside.all[!is.na(inside.all)]
#       
#       mltpl <- df.ans[which(df.ans$bag == bag.col)[length(which(df.ans$bag == bag.col))], 2]
#       
#       for(cl in 1:length(inside.cols)){
#         df.ans[x, 1] <- inside.cols[cl]
#         df.ans[x, 2] <- as.numeric(inside.all[2*cl]) * mltpl
#         x <- x+1
#         inslist2 <- c(inslist2, inside.cols[cl])
#       }
#     }
#   }
#   inslist <- inslist2
#   inslist2 <- c()
# }
# 
# df.ans2 <- df.ans %>%
#   na.omit() %>%
#   summarise(SUMS = sum(times))
# df.ans2 <- sum(df.ans2$times)
# #not 3032

## part2
df <- read_lines("input/Input_day7.txt") %>%
  str_split(" contain ") %>%
  map(~ str_split(.x, ", ") %>% unlist()) %>%
  set_names(., map_chr(., ~ magrittr::extract2(., 1)) %>% str_remove(" bags?")) %>%
  map(~ magrittr::extract(., -1)) %>%
  map( ~ list(amounts = .x %>% str_extract(., "[0-9]+"), bags = .x %>% str_extract(., "[a-zA-Z]+ [a-zA-Z]+"))) %>%
  bind_rows(.id = "bag") %>%
  mutate(amounts = as.numeric(amounts))

part2 <- function(df, bag) {
  
  multiplier <- table(bag) %>%
    as_tibble()
  
  tmp <- df %>%
    filter(bag %in% !!bag) %>%
    select(bag, bags, amounts) %>%
    filter(bags != "no other", !is.na(amounts)) %>%
    left_join(multiplier, by = c(bag = "bag")) %>%
    mutate(amounts = amounts * n)
  
  map2(tmp$bags, tmp$amounts, ~ rep(.x, .y)) %>%
    unlist()
}

k <- part2(df, "shiny gold")
bags <- length(k)

while(!is.null(k)) {
  k <- part2(df, k)
  bags <- bags + length(k)
}

## Answer
bags
