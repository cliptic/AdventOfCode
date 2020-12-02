# Advent of code 2020
# Day2
rm(list = ls())
library(gdata)
library(dplyr)
library(stringr)

input <- read.csv('input/Input_day2.csv', check.names = T, stringsAsFactors = F) %>%
  separate(Input, c("A", "B", "C", "D"), extra = "warn", fill = "right")

# Quest 1
df <- input %>%
  rename(low = A, high = B, letter = C, pass = D) %>%
  mutate(low = as.numeric(low)) %>%
  mutate(high = as.numeric(high))

df1 <- df %>%
  mutate(valid1 = ifelse((str_count(pass, letter) <= high) & (str_count(pass, letter) >= low), 
                        1, 0))
Answer1 <- sum(df1$valid1)

# Quest 2

df2 <- df1 %>%
  mutate(valid2 = ifelse((substring(pass, low, low) == letter) | 
                           (substring(pass, high, high) == letter), 1, 0)) %>%
  mutate(valid2 = ifelse((substring(pass, low, low) == letter) & 
                           (substring(pass, high, high) == letter), 0, valid2))

Answer2 <- sum(df2$valid2)
