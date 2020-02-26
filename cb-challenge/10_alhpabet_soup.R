## Venu Thatikonda

## arrange a given string in alphabetical order

## many different appraoches (which ahev lengthy code)
## my appraoch here,
    ## split given string into char vector
    ## use stringi::str_sort function
    ## collapse into one string and return

alphabet_soup <- function(my_str){
  
  res = str_split(my_str, pattern = "")[[1]] %>% 
    stringi::stri_sort() %>% 
    paste(collapse = "")
  
  return(res)
  
}

alphabet_soup(my_str = "zxy")

alphabet_soup(my_str = "venu")