## Venu Thatikonda

## https://www.coderbyte.com/editor/guest:Letter%20Count%20I:Python
## Return first word with highest number of repeated letters in it

letter_count <- function(my_sen){
  
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(dplyr))
  
  sen_splt = str_split(my_sen, pattern = " ")[[1]] 
  
  final_res = NULL
  
  for(word in 1:length(sen_splt)){
    
    my_word = sen_splt[word]
    
    word_res = table(unlist(strsplit(my_word, ""), use.names=FALSE))
    
    my_max = word_res %>% max
    
    req_res = my_max
    names(req_res) = my_word
    
    final_res = c(final_res, req_res)
    
  }
  
  final_max = final_res %>% max
  
  
  if(final_max == 1){
    
    cat("-1")
    
  } else{
    
    my_result = final_res[final_res == final_max] %>% names
    
    return(my_result)
  }
  
}

letter_count(my_sen = 'my name is Veeeenu')

letter_count(my_sen = 'xyz')