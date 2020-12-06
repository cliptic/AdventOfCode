# Advent of code 2020
# Day2

rm(list = ls())
library(gdata)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)
library(re)


input  <- fread('input/Input_day4.txt', sep = " ", header = FALSE, fill = TRUE) 

df <- input %>%
  mutate(V1 = ifelse(V1 == "", "*", V1))

df2 <- paste(df, collapse = " ")
list2 <- c()
for (i in 1:nrow(input)){
  adding <- input[i, ]
  if(adding == ""){
    adding <- "~"
  }
  list2 <- c(list2, unlist(adding))
}

list3 <- c()
for (m in list2){
  m <- unlist(strsplit(m, split = " "))
  list3 <- c(unlist(list3), m)
}

list3 <- unlist(list3)

df_all <- data.frame(byr = NA, iyr = NA, eyr = NA, 
                     hgt = NA, hcl = NA,  ecl = NA, 
                     pid = NA, cid = NA)
m <- 1
k <- ncol(df_all)
for (i in list3){
  if (i == "~"){
    m <- m+1
  }else{
    colname <- substring(i, 1, 3)
   df_all[m, colname] <- substring(i, 5)
  }
}

# Quest1
df_ans <- df_all %>%
  mutate(cid = ifelse(is.na(cid), 'ignore', cid)) %>%
  na.omit()
Ans1 <- nrow(df_ans)
print(Ans1)
#Quest2

df_ans2 <- df_all %>%
  mutate(cid = ifelse(is.na(cid), 'ignore', cid)) %>%
  mutate(byr = as.numeric(byr),
         iyr = as.numeric(iyr),
         eyr = as.numeric(eyr)) %>%
 #invalid = NA
  mutate(byr = ifelse(byr >= 1920 & byr <= 2002, byr, NA)) %>%
  mutate(iyr = ifelse(iyr >= 2010 & iyr <= 2020, iyr, NA)) %>%
  mutate(eyr = ifelse(eyr >= 2020 & eyr <= 2030, eyr, NA)) %>%
  mutate(hgt.mes = ifelse(str_sub(hgt, start= -2) %in% c("in", "cm"), 
                          str_sub(hgt, start= -2), NA)) %>%
  mutate(hgt = as.numeric(str_remove(hgt, "cm|in"))) %>%
  mutate(hgt = ifelse(((hgt.mes == "cm") & (hgt %in% 150:193) |
                                (hgt.mes == "in") & (hgt %in% 59:76)), 
                              hgt, NA)) %>%
  # mutate(hcl = ifelse(stringi::stri_detect_regex(hcl, 
  #                                                pattern = ("(^#[0-9]{6}$)|(^#[a-z]{6}$)")), 
  #                     hcl, NA)) %>%
  mutate(hcl = ifelse(stringi::stri_detect_regex(hcl,
                                                 pattern = ("(^#[0-9a-z]{6}$)")),
                      hcl, NA)) %>%
  mutate(ecl = ifelse(ecl %in% c("amb", "blu", "brn", 
                                 "gry", "grn", "hzl", "oth"), ecl, NA)) %>%
  mutate(pid = ifelse(stringi::stri_detect_regex(pid, pattern = ("^[0-9]{9}$")), 
                      pid, NA)) %>%
  na.omit() 
  
Ans2 <- nrow(df_ans2)
print(Ans2)

# not 109
