## Venu Thatikonda
## simple symbols
## Algorithm
## ''' in a given string a letter must be surrounded by + symbol'''

simple_symbols <- function(my_str){
  
  suppressPackageStartupMessages(library(stringr))
  
  str_detect(my_str, regex("\\+\\w\\+", ignore_case = TRUE))
  
}

simple_symbols(my_str = "venu")

simple_symbols(my_str = "+v+enu")
