## Venu Thatikonda
## True if same number of 'x' and 'o's in a given string

ex_oh <- function(my_str){
  
  suppressPackageStartupMessages(library(stringr))
  
  x_cnt = stringr::str_count(my_str, pattern = "x")
  o_cnt = stringr::str_count(my_str, pattern = "o")
  
  if(x_cnt == o_cnt){
    return(TRUE)
  }else{
    return(FALSE)
  }

}

ex_oh(my_str = 'xohsj')

ex_oh(my_str = 'ohsj')
