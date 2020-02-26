## Venu Thatikonda
## return longest word/words

library(stringr)

longest_word <- function(sen){
  
  suppressPackageStartupMessages(library(dplyr))
  
  sen_split = str_split(sen, pattern = " ")[[1]]
  res = str_length(sen_split)
  names(res) = sen_split
  
  first_longest = res[res == max(res)][1] %>% names()
  all_longest = res[res == max(res)] %>% names
  
  cat("First longest: ", first_longest, "\n")
  cat("All longest: ", all_longest, "\n")
  
  
}

longest_word(sen = 'my name is venu')

longest_word(sen = 'i love R')