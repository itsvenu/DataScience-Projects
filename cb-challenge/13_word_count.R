## Venu Thatikonda

## word count in a given sentence

word_count <- function(my_sen){
  
  res = str_split(my_sen, pattern = " ")[[1]]

  res = length(res)  
  
  return(res)
  
}

word_count('I love R')

word_count('I love dataScience')