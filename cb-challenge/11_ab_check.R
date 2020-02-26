## Venu Thatikonda

## AB check
## check in agiven string if 'a' and 'b' are spearated exactly by 3 spaces

ab_check <- function(my_str){
  
  suppressPackageStartupMessages(library(stringr))
  
  str_vec = str_split(my_str, pattern = "")[[1]]
  
  named_vec = 1:length(str_vec)
  names(named_vec) = str_vec
  
  a_ind = named_vec[names(named_vec) == "a"] %>% unname()
  b_ind = named_vec[names(named_vec) == "b"] %>% unname()
  
  res = NULL
  
  ## loop through a and b and create a new list with index diffferences
  for(i in 1:length(a_ind)){
    
    a_ind_abs = a_ind[i]
    
    for(j in 1:length(b_ind)){
      
      b_ind_abs = b_ind[j]
      ind_diff = b_ind_abs - a_ind_abs
      
      res = c(res, ind_diff)
    }
  }
  
  final_res = 4 %in% res
  
  return(final_res)
  
}

ab_check(my_str = 'atyhb')

ab_check(my_str = 'aty b')

ab_check(my_str = 'atb')