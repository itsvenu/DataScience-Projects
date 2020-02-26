## Venu Thatikonda

## count number of vowels

count_vowels <- function(my_str){
  
  str_vec = str_split(my_str, pattern = "")[[1]]
  
  a_count = str_vec[str_vec == 'a'] %>% length
  e_count = str_vec[str_vec == 'e'] %>% length
  i_count = str_vec[str_vec == 'i'] %>% length
  o_count = str_vec[str_vec == 'o'] %>% length
  u_count = str_vec[str_vec == 'u'] %>% length
  
  res = a_count + e_count + i_count + o_count + u_count
  
  return(res)
  
}

count_vowels(my_str = 'venu')

count_vowels(my_str = 'agsyh')

count_vowels(my_str = 'gsyh')
