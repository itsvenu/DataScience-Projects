
## Venu Thatikonda
## string maipulation
## Algorithm
## in a given string, replace every letter with next letter in alphabets & capitalize vowels in the final resul


letter_change <- function(my_str){
  
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(dplyr))
  #my_str = x
  
  res = NULL
  
  my_str = str_split(my_str, pattern = "")[[1]]
  
  all_letters = 1:26
  
  names(all_letters) = letters

  for(ind in 1:length(my_str)){
    
    ori_letter = my_str[ind]
    
    ori_index = all_letters[names(all_letters) == ori_letter] %>% unname()
    
    if(ori_index == 26){
      
      req_index = 1
    } else {
      req_index = ori_index + 1
    }
    
    req_letter = all_letters[req_index] %>% names()
    
    res = c(res, req_letter)
    
  }
  
  res = paste0(res, collapse = "")
  
  ## vowel task - capitalize vowels
  n_res = str_replace(string = res, pattern = "a", replacement = "A")
  n_res = str_replace(string = n_res, pattern = "e", replacement = "E")
  n_res = str_replace(string = n_res, pattern = "i", replacement = "I")
  n_res = str_replace(string = n_res, pattern = "o", replacement = "O")
  n_res = str_replace(string = n_res, pattern = "u", replacement = "U")
  
  return(n_res)
  
}

letter_change(my_str = 'venua')

letter_change(my_str = 'ilover')

# n(n+1)/2