## Venu Thatikonda

## capitalize first letter of every word in a given sentence

letter_capitalize <- function(my_sen){
  
  suppressPackageStartupMessages(library(stringi))
  
  res = stri_trans_totitle(my_sen)
  
  return(res)
  
}

letter_capitalize(my_sen = "my name is venu")

letter_capitalize(my_sen = "i love r")


