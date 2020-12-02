# Advent of code 2020
# Day1

input <- read.csv('input/input_day1.csv', check.names = T, stringsAsFactors = F)$Value

# Quest 1

for (i in input){
  for(m in input){
    if(i + m == 2020){
      print(i*m)
      stop('You found your answer')
    }
  }
}

# Quest 2

for (i in input){
  for(m in input){
    for(k in input){
      if(i + m + k == 2020){
        print(i*m*k)
        stop('You found your answer')
      }
    }
  }
  
}

# Two stars
