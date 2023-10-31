# Advent of code 2020
# Day16

rm(list = ls())

library(dplyr)
library("data.table")


options(scipen = 100, digits = 4)

input  <- fread('input/Input_day16.txt', sep = ",", header = FALSE, fill = TRUE)
one <- unlist(str_split(input[26, "V1"], pattern = ","))
tickets <- as.data.frame(input[26:nrow(input), "V1"], stringsasfactors = F) %>%
  separate(unlist(V1), sep = ",", into = paste0("R",(1:length(one)))) 

#Quest16

mine <- unlist(str_split(input[23, "V1"], pattern = ","))

Rules <- as.data.frame(input[1:20, "V1"], stringsasfactors = F) %>%
  separate(V1, sep = ": |-| or ", into = paste0("R",(1:5))) %>%
  mutate(R2 = as.numeric(R2),
         R3 = as.numeric(R3),
         R4 = as.numeric(R4),
         R5 = as.numeric(R5))
colnames(Rules) <- c("rule", "min1", "max1", "min2", "max2")

good.num <- c()
for(i in 1:nrow(Rules)){
  good.num <- c(Rules[i, "min1"]:Rules[i, "max1"], 
                Rules[i, "min2"]:Rules[i, "max2"], good.num)
}
good.num <- unique(good.num)

wrong <- sum(as.numeric(unlist(tickets)[!(unlist(tickets) %in% as.character(good.num))]))
wrong

#Quest2
# tickets <- data.frame(matrix(unlist(tickets), nrow=length(tickets), byrow=T), stringsAsFactors = F) 
tickets <- data.frame(sapply(tickets, as.integer))
tickets2 <- tickets
for(i in 1:nrow(tickets2)){
  tickets2[i,] <- ifelse(all(tickets2[i,] %in% good.num), unlist(tickets2[i,]), NA)
}

case1 <- complete.cases(tickets2)
tick <- tickets %>%
  filter(case1)
# all good tickets tick
rul <- c()
for(i in 1:nrow(Rules)){
  range <- c(Rules[i, "min1"]:Rules[i, "max1"], Rules[i, "min2"]:Rules[i, "max2"])
  rul[Rules$rule[i]] <- list(range)
}


for(i in 1:length(rul)){
  
  rn <- rul[[i]]
  for(k in 1:ncol(tick)){
    Rules[i, 5+k] <- ifelse(all(unlist(tick[,k]) %in% rn), names(rul)[i], 0)
  }
}

for (i in 1:nrow(Rules)){
  Rules[i,"SU"] <- sum(Rules[i,6:(ncol(Rules)-1)])
}
  mutate(SU = sum(Rules[,6:ncol(Rules)]))
  
Rules[Rules == 0] <- NA
poss <- Rules[,6:(ncol(Rules)-1)]
  
ans.list <- c()
Ru <- Rules
for(n in 1:20){
  for(c in 1:nrow(poss)){
    if(Ru$SU[c] == 1){
      Ru$SU <- Ru$SU - 1
      nm <- Rules[c, "rule"]
      print(nm)
      cn <- which(!is.na(Ru[c,6:(ncol(Ru)-1)]))
      ans.list[[nm]] <- cn
      Ru[,cn+5] <- NA
    }
  }
}

ans.l <- sort(ans.list)
colnames(tick) <- names(ans.l)

ans2 <- prod(as.integer(mine[which(grepl("departure", names(ans.l)))]))
ans2
